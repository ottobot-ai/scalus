package scalus.uplc

/** Flat-encoded Plutus scripts written bit by bit, for inputs that Scalus's own encoder cannot
  * produce: a `data` constant with bytes after its CBOR item, a `string` constant that is not
  * UTF-8, a version wider than 32 bits, a filler that does not end the input. It is a tiny term
  * language and its writer, following `PlutusCore/FlatInstances.hs` and
  * `UntypedPlutusCore/Core/Instance/Flat.hs`; test code, so terms are written by plain recursion.
  */
object FlatProgramBuilder {
    sealed trait Tm
    case class Delay(body: Tm) extends Tm
    case class Lam(body: Tm) extends Tm
    case class Ap(function: Tm, argument: Tm) extends Tm
    case class Builtin(tag: Int) extends Tm
    case class Constr(tag: BigInt, args: Vector[Tm]) extends Tm

    /** A constant: its type tags (preorder) and the writer of its value. */
    case class Const(types: Vector[Int], value: Bits => Unit) extends Tm

    val unit: Tm = Const(Vector(3), _ => ())

    /** A `data` constant whose flat byte string holds exactly `cbor`, trailing bytes and all. */
    def data(cbor: Array[Byte]): Tm = Const(Vector(8), _.byteString(cbor))

    /** A `string` constant whose flat byte string holds exactly `raw`, UTF-8 or not. */
    def string(raw: Array[Byte]): Tm = Const(Vector(2), _.byteString(raw))

    /** `levels` definite CBOR lists, each holding the next, around the integer 0. */
    def nestedList(levels: Int): Array[Byte] =
        Array.fill[Byte](levels)(0x81.toByte) :+ 0x00.toByte

    /** `(\_ -> unit) term`: evaluates to unit whatever `term` is, as long as it decodes. */
    def discard(term: Tm): Tm = Ap(Lam(unit), term)

    final class Bits {
        private val out = new StringBuilder

        def bit(b: Int): Unit = out.append(if b == 0 then '0' else '1')

        def bits(value: Int, width: Int): Unit =
            (width - 1 to 0 by -1).foreach(i => bit((value >> i) & 1))

        /** A flat natural: groups of seven bits, least significant first, each behind a continue
          * bit.
          */
        def natural(n: BigInt): Unit = {
            var rest = n
            var more = true
            while more do {
                val group = (rest & 0x7f).toInt
                rest = rest >> 7
                more = rest != 0
                bits(group | (if more then 0x80 else 0), 8)
            }
        }

        /** Zero or more 0 bits and a 1 bit, up to a byte boundary. */
        def filler(): Unit = {
            while (out.length + 1) % 8 != 0 do bit(0)
            bit(1)
        }

        def byteString(b: Array[Byte]): Unit = {
            filler()
            b.grouped(255).foreach { chunk =>
                bits(chunk.length, 8)
                chunk.foreach(x => bits(x & 0xff, 8))
            }
            bits(0, 8)
        }

        /** The bits so far followed by the minimal filler. */
        def bytes: Array[Byte] = {
            filler()
            packed
        }

        /** Zero bits up to the next byte boundary, and no filler. */
        def zeros(): Unit = while out.length % 8 != 0 do bit(0)

        /** The bits written so far, which must end on a byte boundary, without a filler. */
        def packed: Array[Byte] =
            out.toString.grouped(8).map(Integer.parseInt(_, 2).toByte).toArray
    }

    private def term(t: Tm, b: Bits): Unit = t match {
        case Delay(x) => b.bits(1, 4); term(x, b)
        case Lam(x)   => b.bits(2, 4); term(x, b)
        case Ap(f, a) => b.bits(3, 4); term(f, b); term(a, b)
        case Const(ts, v) =>
            b.bits(4, 4)
            ts.foreach { tag =>
                b.bit(1)
                b.bits(tag, 4)
            }
            b.bit(0)
            v(b)
        case Builtin(tag) => b.bits(7, 4); b.bits(tag, 7)
        case Constr(tag, args) =>
            b.bits(8, 4)
            b.natural(tag)
            args.foreach { arg =>
                b.bit(1)
                term(arg, b)
            }
            b.bit(0)
    }

    /** The bits of a program up to the end of its term: what follows is the caller's to write. */
    def unpadded(version: (BigInt, BigInt, BigInt), t: Tm): Bits = {
        val b = new Bits
        b.natural(version._1)
        b.natural(version._2)
        b.natural(version._3)
        term(t, b)
        b
    }

    /** The flat bytes of a program whose version components may be any natural. */
    def flatOf(version: (BigInt, BigInt, BigInt), t: Tm): Array[Byte] =
        unpadded(version, t).bytes

    /** The flat bytes of a program of the given UPLC version. */
    def flat(version: (Int, Int, Int), t: Tm): Array[Byte] =
        flatOf((BigInt(version._1), BigInt(version._2), BigInt(version._3)), t)

    /** A CBOR byte string around `content`. */
    def cborByteString(content: Array[Byte]): Array[Byte] = {
        val n = content.length
        val head =
            if n < 24 then Array((0x40 + n).toByte)
            else if n < 256 then Array(0x58.toByte, n.toByte)
            else if n < 65536 then Array(0x59.toByte, (n >> 8).toByte, n.toByte)
            else Array(0x5a.toByte, (n >> 24).toByte, (n >> 16).toByte, (n >> 8).toByte, n.toByte)
        head ++ content
    }

    /** The script as the ledger holds it: a CBOR byte string around the flat bytes. */
    def script(version: (Int, Int, Int), t: Tm): Array[Byte] =
        cborByteString(flat(version, t))
}
