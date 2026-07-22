# Nim-LibP2P
# Copyright (c) 2023-2024 Status Research & Development GmbH
# Licensed under either of
#  * Apache License, version 2.0, ([LICENSE-APACHE](LICENSE-APACHE))
#  * MIT license ([LICENSE-MIT](LICENSE-MIT))
# at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.

{.push raises: [].}

import pkg/[chronos, chronicles, stew/byteutils]
import ../../stream/connection, ../../utility, ../../varint, ../../vbuffer, ../muxer

logScope:
  topics = "libp2p mplexcoder"

type
  MessageType* {.pure.} = enum
    New
    MsgIn
    MsgOut
    CloseIn
    CloseOut
    ResetIn
    ResetOut

  Msg* = tuple[id: uint64, msgType: MessageType, data: seq[byte]]

  InvalidMplexMsgType* = object of MuxerError

# https://github.com/libp2p/specs/tree/master/mplex#writing-to-a-stream
const MaxMsgSize* = 1 shl 20 # 1mb

proc newInvalidMplexMsgType*(): ref InvalidMplexMsgType =
  newException(InvalidMplexMsgType, "invalid message type")

proc readMsg*(
    conn: Connection
): Future[Msg] {.async: (raises: [CancelledError, LPStreamError, MuxerError]).} =
  let header = await conn.readVarint()
  trace "read header varint", varint = header, conn

  let data = await conn.readLp(MaxMsgSize)
  trace "read data", dataLen = data.len, data = shortLog(data), conn

  let msgType = header and 0x7
  if msgType.int > ord(MessageType.ResetOut):
    raise newInvalidMplexMsgType()

  return (header shr 3, MessageType(msgType), data)

proc encodedSize(id: uint64, msgType: MessageType, dataLen: int): int =
  ## Exact size of the chunked wire encoding of a `dataLen`-byte message.
  let header = id shl 3 or ord(msgType).uint64
  if dataLen == 0:
    return vsizeof(header) + vsizeof(uint(0))
  var left = dataLen
  while left > 0:
    let chunkSize =
      if left > MaxMsgSize:
        MaxMsgSize - 64
      else:
        left
    result += vsizeof(header) + vsizeof(uint(chunkSize)) + chunkSize
    left = left - chunkSize

proc encodeMsg*(
    buf: var VBuffer, id: uint64, msgType: MessageType, prefix, data: openArray[byte]
) =
  ## Write the chunked length-prefixed encoding of `prefix ++ data` into
  ## `buf`, without materializing the concatenation.
  let totalLen = prefix.len + data.len
  var
    left = totalLen
    offset = 0

  # Split message into length-prefixed chunks
  while left > 0 or totalLen == 0:
    let chunkSize =
      if left > MaxMsgSize:
        MaxMsgSize - 64
      else:
        left

    buf.writePBVarint(id shl 3 or ord(msgType).uint64)
    if chunkSize == 0:
      buf.writeLPVarint(0.uint)
    elif offset + chunkSize <= prefix.len:
      buf.writeSeq(prefix.toOpenArray(offset, offset + chunkSize - 1))
    elif offset >= prefix.len:
      buf.writeSeq(data.toOpenArray(offset - prefix.len, offset - prefix.len + chunkSize - 1))
    else:
      # Chunk spans the prefix/data seam: write the length varint, then
      # both parts raw.
      let firstLen = prefix.len - offset
      buf.writeLPVarint(uint(chunkSize))
      buf.writeArray(prefix.toOpenArray(offset, prefix.high))
      buf.writeArray(data.toOpenArray(0, chunkSize - firstLen - 1))

    left = left - chunkSize
    offset = offset + chunkSize

    if totalLen == 0:
      break

proc writeMsg*(
    conn: Connection,
    id: uint64,
    msgType: MessageType,
    prefix: openArray[byte],
    data: openArray[byte],
): Future[void] {.async: (raises: [CancelledError, LPStreamError], raw: true).} =
  ## Write `prefix ++ data` as chunked length-prefixed mplex frames,
  ## without materializing the concatenation. Single underlying write,
  ## so close/reset messages cannot interleave between chunks.
  var buf = VBuffer(buffer: newSeqOfCap[byte](encodedSize(id, msgType, prefix.len + data.len)))
  encodeMsg(buf, id, msgType, prefix, data)

  trace "writing mplex message",
    conn, id, msgType, data = prefix.len + data.len, encoded = buf.buffer.len

  # Write all chunks in a single write to avoid async races where a close
  # message gets written before some of the chunks
  conn.write(buf.buffer)

proc writeMsg*(
    conn: Connection, id: uint64, msgType: MessageType, data: seq[byte] = @[]
): Future[void] {.async: (raises: [CancelledError, LPStreamError], raw: true).} =
  conn.writeMsg(id, msgType, default(seq[byte]), data)

proc writeMsg*(
    conn: Connection, id: uint64, msgType: MessageType, data: string
): Future[void] {.async: (raises: [CancelledError, LPStreamError], raw: true).} =
  conn.writeMsg(id, msgType, data.toBytes())
