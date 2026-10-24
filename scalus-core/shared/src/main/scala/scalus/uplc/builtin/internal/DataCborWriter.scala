package scalus.uplc.builtin.internal

import scalus.uplc.builtin.Data

import java.io.ByteArrayOutputStream
import scala.collection.mutable.ArrayDeque

/** The CBOR encoding of [[Data]] that the `serialiseData` builtin returns, written without
  * recursion.
  *
  * The bytes are those of `PlutusCore.Data.encodeData` of the reference:
  *   - `Constr i`: tag `121 + i` for `0 <= i < 7`, tag `1280 + i - 7` for `7 <= i < 128`, otherwise
  *     tag 102 and a two-element array of the index and the fields;
  *   - a list, and the fields of a `Constr`, are an indefinite-length array when non-empty and
  *     `0x80` when empty; a map has a definite length;
  *   - an integer in `[-2^64, 2^64)` is a plain CBOR integer, any other is tag 2 or 3 over a byte
  *     string, which is split into chunks of 64 bytes when it is longer than that;
  *   - the index of a tag-102 `Constr` is written by cborg's `encodeInteger` (`Data.hs` line 159),
  *     which writes a big number as tag 2 or 3 and ONE definite byte string, never chunked;
  *   - a byte string of up to 64 bytes is definite, a longer one is indefinite, in chunks of 64
  *     bytes.
  *
  * The reference has no limit on the nesting depth, and neither the recursion of a borer encoder
  * nor the JVM stack may impose one: the open lists and maps are kept on the heap.
  *
  * @see
  *   [[https://github.com/IntersectMBO/plutus/blob/507cab8cf6192e5143f12be5b2d3005a2928dc6d/plutus-core/plutus-core/src/PlutusCore/Data.hs PlutusCore.Data]]
  *   (plutus 1.70.0.0)
  */
object DataCborWriter {

    /** An open list or map: the items still to write, and the byte that closes it (`-1` for none).
      */
    private final class Open(val rest: Iterator[Data], val close: Int)

    private val MaxChunk = 64
    private val BreakByte = 0xff

    def encode(root: Data): Array[Byte] = {
        val out = new ByteArrayOutputStream()
        val open = ArrayDeque.empty[Open]

        def writeArray(items: Iterator[Data], isEmpty: Boolean): Unit =
            if isEmpty then out.write(0x80)
            else {
                out.write(0x9f)
                open.append(new Open(items, BreakByte))
            }

        def writeItem(data: Data): Unit = data match
            case Data.Constr(constr, args) =>
                if constr >= 0 && constr < 7 then writeHead(out, 6, 121L + constr.toLong)
                else if constr >= 7 && constr < 128 then
                    writeHead(out, 6, 1280L + constr.toLong - 7L)
                else {
                    writeHead(out, 6, 102L)
                    writeHead(out, 4, 2L)
                    writeConstrIndex(out, constr)
                }
                val fields = args.toScalaList
                writeArray(fields.iterator, fields.isEmpty)
            case Data.Map(values) =>
                val entries = values.toScalaList
                writeHead(out, 5, entries.size.toLong)
                open.append(
                  new Open(entries.iterator.flatMap { case (k, v) => Iterator(k, v) }, -1)
                )
            case Data.List(values) =>
                val items = values.toScalaList
                writeArray(items.iterator, items.isEmpty)
            case Data.I(value) => writeInteger(out, value)
            case Data.B(value) => writeBytes(out, value.bytes)

        writeItem(root)
        while open.nonEmpty do {
            val top = open.last
            if top.rest.hasNext then writeItem(top.rest.next())
            else {
                open.removeLast()
                if top.close >= 0 then out.write(top.close)
            }
        }
        out.toByteArray
    }

    /** The magnitude of a big number without the sign byte that `BigInt.toByteArray` adds. */
    private def magnitudeBytes(magnitude: BigInt): Array[Byte] = {
        val bytes = magnitude.toByteArray
        if bytes.length > 1 && bytes.head == 0 then bytes.tail else bytes
    }

    /** The tag and the magnitude of a big number: tag 2 over `n`, tag 3 over `-1 - n`. */
    private def bignum(value: BigInt): (Long, Array[Byte]) =
        if value.signum >= 0 then (2L, magnitudeBytes(value)) else (3L, magnitudeBytes(-1 - value))

    /** A CBOR integer within 64 bits, as the plain head of major type 0 or 1. */
    private def writeSmall(out: ByteArrayOutputStream, value: BigInt): Unit =
        // for a value of 64 bits `longValue` keeps the bits, read as unsigned by `writeHead`
        if value.signum >= 0 then writeHead(out, 0, value.longValue)
        else writeHead(out, 1, (-1 - value).longValue)

    /** An integer of `I`: a plain CBOR integer when it fits 64 bits either way, otherwise tag 2 or
      * 3 and a byte string chunked as [[writeBytes]] does (`Data.hs` lines 180-182).
      */
    private def writeInteger(out: ByteArrayOutputStream, value: BigInt): Unit =
        if value.bitLength <= 64 then writeSmall(out, value)
        else {
            val (tag, bytes) = bignum(value)
            writeHead(out, 6, tag)
            writeBytes(out, bytes)
        }

    /** The bytes of [[writeConstrIndex]]: the reference's `Constr` has any `Integer` index, and the
      * function writes a negative one as well as a non-negative one.
      */
    private[builtin] def encodeConstrIndex(value: BigInt): Array[Byte] = {
        val out = new ByteArrayOutputStream()
        writeConstrIndex(out, value)
        out.toByteArray
    }

    /** The index of a tag-102 `Constr`, as cborg's `encodeInteger` writes it: a plain head within
      * 64 bits, otherwise tag 2 or 3 and one definite byte string of the minimal magnitude.
      */
    private def writeConstrIndex(out: ByteArrayOutputStream, value: BigInt): Unit =
        if value.bitLength <= 64 then writeSmall(out, value)
        else {
            val (tag, bytes) = bignum(value)
            writeHead(out, 6, tag)
            writeHead(out, 2, bytes.length.toLong)
            out.write(bytes, 0, bytes.length)
        }

    private def writeBytes(out: ByteArrayOutputStream, bytes: Array[Byte]): Unit =
        if bytes.length <= MaxChunk then {
            writeHead(out, 2, bytes.length.toLong)
            out.write(bytes, 0, bytes.length)
        } else {
            out.write(0x5f)
            var offset = 0
            while offset < bytes.length do {
                val length = math.min(MaxChunk, bytes.length - offset)
                writeHead(out, 2, length.toLong)
                out.write(bytes, offset, length)
                offset += length
            }
            out.write(BreakByte)
        }

    /** A CBOR head: the major type and an unsigned 64-bit argument, in the shortest form. */
    private def writeHead(out: ByteArrayOutputStream, major: Int, arg: Long): Unit = {
        val m = major << 5
        if arg >= 0 && arg < 24 then out.write(m | arg.toInt)
        else {
            // the additional information 24 to 27 announces an argument of 1, 2, 4 or 8 bytes
            val (additional, width) =
                if (arg >>> 8) == 0 then (24, 1)
                else if (arg >>> 16) == 0 then (25, 2)
                else if (arg >>> 32) == 0 then (26, 4)
                else (27, 8)
            out.write(m | additional)
            var shift = 8 * (width - 1)
            while shift >= 0 do {
                out.write((arg >>> shift).toInt & 0xff)
                shift -= 8
            }
        }
    }
}
