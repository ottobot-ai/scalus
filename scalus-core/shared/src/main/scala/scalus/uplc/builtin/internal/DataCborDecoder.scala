package scalus.uplc.builtin.internal

import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.serialization.flat.FlatDecodingError
import scalus.uplc.builtin.Data.{B, Constr, I}
import scalus.uplc.builtin.{ByteString, Data}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/** Reads the first CBOR item of a byte array as [[Data]], as the Plutus reference does for a `data`
  * constant of a script.
  *
  * The reference reads the constant through `FlatViaSerialise`, whose `deserialiseOrFail` runs
  * `decodeData` (`PlutusCore/Data.hs`, Plutus 1.70.0.0) and drops whatever follows the item
  * (`Codec/Serialise.hs` of serialise 0.2.6.1, `deserialiseOrFail`). `decodeData` has no limit on
  * the nesting of lists, maps and constructors; the only size limit is that a definite byte string,
  * or a chunk of an indefinite one, holds at most 64 bytes.
  *
  * Nesting is walked on an explicit stack, so the depth is bounded by the input and not by the
  * thread's stack.
  */
private[scalus] object DataCborDecoder {

    /** The [[Data]] that the first CBOR item of `bytes` encodes, and the offset where the item
      * ends.
      *
      * @throws FlatDecodingError
      *   if the bytes do not start with a CBOR item that `decodeData` accepts
      */
    def decodePrefix(bytes: Array[Byte]): (Data, Int) = {
        val decoder = new Decoder(bytes)
        val data = decoder.run()
        (data, decoder.position)
    }

    /** An open list, map or constructor argument list. `remaining` counts the elements (pairs for a
      * map) still to read, or is -1 for an indefinite-length sequence. `closeEnvelope` is set for
      * the arguments of the `102` form written as an indefinite array: its break follows them.
      */
    private final class Frame(
        val kind: Int,
        var remaining: Long,
        val constrIndex: BigInt,
        val closeEnvelope: Boolean
    ) {
        val items = new ArrayBuffer[Data]
    }

    private final val ListKind = 0
    private final val MapKind = 1
    private final val ConstrKind = 2

    private final class Decoder(bytes: Array[Byte]) {
        var position = 0
        private val frames = new ArrayBuffer[Frame]

        private def fail(message: String): Nothing =
            throw new FlatDecodingError(s"Invalid data constant at byte $position: $message")

        private def remainingBytes: Long = (bytes.length - position).toLong

        private def readByte(): Int = {
            if position >= bytes.length then fail("the input ends inside a CBOR item")
            val b = bytes(position) & 0xff
            position += 1
            b
        }

        private def readUnsigned(width: Int): BigInt = {
            if remainingBytes < width then fail("the input ends inside a CBOR item")
            var value = BigInt(0)
            var i = 0
            while i < width do {
                value = (value << 8) | BigInt(bytes(position + i) & 0xff)
                i += 1
            }
            position += width
            value
        }

        /** The argument of an item head, or -1 for the indefinite-length marker. */
        private def readArgument(info: Int): BigInt =
            if info < 24 then BigInt(info)
            else if info == 24 then readUnsigned(1)
            else if info == 25 then readUnsigned(2)
            else if info == 26 then readUnsigned(4)
            else if info == 27 then readUnsigned(8)
            else if info == 31 then BigInt(-1)
            else fail(s"reserved additional information $info")

        /** A definite length, bounded by what the input can still hold at `unit` bytes an element:
          * a longer sequence cannot be complete, so it is refused before anything is allocated.
          */
        private def definiteLength(argument: BigInt, unit: Int): Long = {
            if argument * unit > BigInt(remainingBytes) then
                fail("the input ends inside a CBOR item")
            argument.toLong
        }

        /** A byte string of at most 64 bytes behind a head with this argument. */
        private def readBoundedBytes(argument: BigInt): Array[Byte] = {
            if argument > 64 then fail("a byte string chunk is longer than 64 bytes")
            val length = argument.toInt
            if remainingBytes < length then fail("the input ends inside a CBOR item")
            val chunk = java.util.Arrays.copyOfRange(bytes, position, position + length)
            position += length
            chunk
        }

        /** The content of the byte string at the cursor, definite or indefinite. */
        private def readByteString(): Array[Byte] = {
            val head = readByte()
            if head >> 5 != 2 then fail("expected a byte string")
            val argument = readArgument(head & 0x1f)
            if argument.signum >= 0 then readBoundedBytes(argument)
            else {
                val out = new java.io.ByteArrayOutputStream
                var more = true
                while more do {
                    val chunkHead = readByte()
                    if chunkHead == 0xff then more = false
                    else {
                        if chunkHead >> 5 != 2 || (chunkHead & 0x1f) == 31 then
                            fail("expected a definite byte string chunk")
                        out.write(readBoundedBytes(readArgument(chunkHead & 0x1f)))
                    }
                }
                out.toByteArray
            }
        }

        /** A completed value goes to the open sequence that is waiting for it, which may thereby be
          * complete, and so on outwards. Returns the value of the outermost item once it is
          * complete, otherwise null.
          */
        private def deliver(first: Data): Data | Null = {
            var value = first
            while frames.nonEmpty do {
                val frame = frames.last
                frame.items += value
                // a map counts pairs, so only the value completes an element
                val elementDone = frame.kind != MapKind || frame.items.size % 2 == 0
                if elementDone && frame.remaining > 0 then frame.remaining -= 1
                if frame.remaining == 0 then value = close()
                else return null
            }
            value
        }

        /** Closes the innermost sequence, which holds all its elements. */
        private def close(): Data = {
            val frame = frames.remove(frames.size - 1)
            if frame.closeEnvelope then {
                if readByte() != 0xff then fail("expected exactly two elements")
            }
            frame.kind match {
                case ListKind => Data.List(PList.from(frame.items))
                case ConstrKind =>
                    Constr(frame.constrIndex, PList.from(frame.items))
                case _ =>
                    val pairs = new ListBuffer[(Data, Data)]
                    val it = frame.items.iterator
                    while it.hasNext do {
                        val key = it.next()
                        pairs += ((key, it.next()))
                    }
                    Data.Map(PList.from(pairs))
            }
        }

        /** Opens a sequence behind the head just read, or completes it if it is empty. */
        private def open(
            kind: Int,
            argument: BigInt,
            constrIndex: BigInt,
            closeEnvelope: Boolean
        ): Data | Null = {
            val remaining =
                if argument.signum < 0 then -1L
                else definiteLength(argument, if kind == MapKind then 2 else 1)
            frames += new Frame(kind, remaining, constrIndex, closeEnvelope)
            if remaining == 0 then deliver(close()) else null
        }

        /** The head of an array at the cursor: its argument, or -1 if it is indefinite. */
        private def readArrayHead(): BigInt = {
            val head = readByte()
            if head >> 5 != 4 then fail("expected an array")
            readArgument(head & 0x1f)
        }

        /** `decodeConstr` after the tag has been read. */
        private def openConstr(tag: BigInt): Data | Null =
            if tag == 102 then {
                val envelope = readArrayHead()
                if envelope.signum >= 0 && envelope != 2 then fail("expected exactly two elements")
                val indexHead = readByte()
                if indexHead >> 5 != 0 then fail("expected an unsigned constructor index")
                val index = readArgument(indexHead & 0x1f)
                if index.signum < 0 then fail("expected an unsigned constructor index")
                open(ConstrKind, readArrayHead(), index, envelope.signum < 0)
            } else if tag >= 121 && tag < 128 then
                open(ConstrKind, readArrayHead(), tag - 121, closeEnvelope = false)
            else if tag >= 1280 && tag < 1401 then
                open(ConstrKind, readArrayHead(), tag - 1280 + 7, closeEnvelope = false)
            else fail(s"unrecognized tag $tag")

        /** Reads the item at the cursor: a leaf is delivered, a sequence is opened. Returns the
          * outermost item once it is complete.
          */
        private def step(): Data | Null = {
            val head = readByte()
            val major = head >> 5
            val info = head & 0x1f
            val argument = readArgument(info)
            major match {
                case 0 if argument.signum >= 0 => deliver(I(argument))
                case 1 if argument.signum >= 0 => deliver(I(-1 - argument))
                case 2 =>
                    if argument.signum >= 0 then
                        deliver(B(ByteString.unsafeFromArray(readBoundedBytes(argument))))
                    else {
                        position -= 1
                        deliver(B(ByteString.unsafeFromArray(readByteString())))
                    }
                case 4 => open(ListKind, argument, 0, closeEnvelope = false)
                case 5 => open(MapKind, argument, 0, closeEnvelope = false)
                // `peekTokenType` calls only the heads 0xc2 and 0xc3 a bignum (cborg's
                // `decodeTokenTypeTable`); tag 2 or 3 behind a longer head is `TypeTag`, goes to
                // `decodeConstr` and is not a constructor tag
                case 6 if head == 0xc2 || head == 0xc3 =>
                    val magnitude = BigInt(1, readByteString())
                    deliver(I(if head == 0xc2 then magnitude else -1 - magnitude))
                case 6 if argument.signum >= 0 => openConstr(argument)
                case _ => fail(s"unrecognized CBOR item of major type $major")
            }
        }

        def run(): Data = {
            var result: Data | Null = null
            while result == null do {
                val top = if frames.isEmpty then null else frames.last
                // an indefinite sequence ends at a break, in a map only between pairs
                val atBreak = top != null && top.remaining < 0 && position < bytes.length &&
                    (bytes(position) & 0xff) == 0xff &&
                    (top.kind != MapKind || top.items.size % 2 == 0)
                result = if atBreak then {
                    position += 1
                    deliver(close())
                } else step()
            }
            result.nn
        }
    }
}
