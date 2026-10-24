package scalus.uplc.builtin

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Data.{B, Constr, I}
import scalus.uplc.builtin.internal.DataCborWriter
import scalus.uplc.test.ArbitraryInstances

/** `serialiseData` writes the bytes of `PlutusCore.Data.encodeData` (plutus 1.70.0.0, commit
  * 507cab8cf6192e5143f12be5b2d3005a2928dc6d, `plutus-core/plutus-core/src/PlutusCore/Data.hs`). The
  * non-recursive writer must agree with borer's recursive encoder wherever that is right, which is
  * everywhere except the index of a tag-102 `Constr` beyond 64 bits: there the expected bytes are
  * written out.
  */
class DataCborWriterTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {

    private def recursive(d: Data): Seq[Byte] = Cbor.encode(d).toByteArray.toSeq
    private def iterative(d: Data): Seq[Byte] = DataCborWriter.encode(d).toSeq
    private def bytes(n: Int): Data = B(
      ByteString.fromArray(Array.tabulate(n)(i => (i * 31).toByte))
    )

    test("agrees with the recursive encoder on arbitrary Data") {
        forAll { (d: Data) =>
            assert(iterative(d) == recursive(d))
        }
    }

    test("serialiseData returns the bytes of the writer") {
        forAll { (d: Data) =>
            assert(Builtins.serialiseData(d).bytes.toSeq == recursive(d))
        }
    }

    test("Data.toCbor and toCborByteString return the bytes of the writer") {
        forAll { (d: Data) =>
            assert(d.toCbor.toSeq == iterative(d))
            assert(d.toCborByteString.bytes.toSeq == iterative(d))
        }
    }

    test("integers around the 64-bit boundaries and big numbers") {
        val two64 = BigInt(1) << 64
        val two63 = BigInt(1) << 63
        val edges = for
            base <- Seq(BigInt(0), two63, two64, BigInt(1) << 520)
            delta <- Seq(BigInt(-2), BigInt(-1), BigInt(0), BigInt(1), BigInt(2))
            sign <- Seq(BigInt(1), BigInt(-1))
        yield sign * (base + delta)
        val small =
            (-30 to 30).map(BigInt(_)) ++ Seq(255, 256, 65535, 65536, 4294967295L, 4294967296L)
                .flatMap(n => Seq(BigInt(n) - 1, BigInt(n), BigInt(n) + 1, -BigInt(n)))
        for n <- edges ++ small do assert(iterative(I(n)) == recursive(I(n)), s"integer $n")
    }

    test("constructor indices around the tag ranges") {
        val indices = Seq(0, 1, 6, 7, 8, 127, 128, 129, 255, 256)
        val wide = Seq(BigInt(1) << 63, (BigInt(1) << 64) - 1)
        for i <- indices.map(BigInt(_)) ++ wide do {
            for fields <- Seq(PList.Nil, PList.singleton(I(1)), PList.from(Seq(I(1), bytes(3))))
            do {
                val d = Constr(i, fields)
                assert(iterative(d) == recursive(d), s"constr $i")
            }
        }
    }

    // Expected bytes of `Constr(index, [])` for an index of `bits` bits, `2^bits - 1` or its
    // negation. The index is written as cborg's `encodeInteger` writes it (`Data.hs` line 159 of
    // plutus 1.70.0.0): tag 2 or 3 and ONE definite byte string of the minimal magnitude, never
    // chunked (only `I` is, lines 180-182). The rows are the results of `serialiseData` in
    // plutus-ledger-api 1.70.0.0: `evaluateScript` of
    // `equalsByteString (serialiseData (constrData index [])) expected` succeeds on these bytes and
    // fails on them with the last bit changed, and on a chunked form of the 513-bit magnitude.
    private val wideIndices: Seq[(Int, Boolean, String)] = Seq(
      (65, true, "d86682c24901ffffffffffffffff80"),
      (65, false, "d86682c34901fffffffffffffffe80"),
      (72, true, "d86682c249ffffffffffffffffff80"),
      (72, false, "d86682c349fffffffffffffffffe80"),
      (
        512,
        true,
        "d86682c25840ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" +
            "ffffffffffffffffffffffffffffffffffffffffffff80"
      ),
      (
        512,
        false,
        "d86682c35840ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" +
            "fffffffffffffffffffffffffffffffffffffffffffe80"
      ),
      (
        513,
        true,
        "d86682c2584101ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" +
            "ffffffffffffffffffffffffffffffffffffffffffffff80"
      ),
      (
        513,
        false,
        "d86682c3584101ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" +
            "fffffffffffffffffffffffffffffffffffffffffffffe80"
      ),
      (
        600,
        true,
        "d86682c2584bffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" +
            "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80"
      ),
      (
        600,
        false,
        "d86682c3584bffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" +
            "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe80"
      )
    )

    private def hex(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString

    test("a tag-102 constructor index beyond 64 bits is one definite byte string") {
        for (bits, positive, expected) <- wideIndices do {
            val magnitude = (BigInt(1) << bits) - 1
            val index = if positive then magnitude else -magnitude
            // `d866 82` opens the constructor and `80` closes its empty field list
            val indexBytes = expected.drop(6).dropRight(2)
            assert(
              hex(DataCborWriter.encodeConstrIndex(index)) == indexBytes,
              s"index of $bits bits, positive=$positive"
            )
            assert(hex(DataCborWriter.encode(Constr(index, PList.Nil))) == expected, s"$bits")
        }
    }

    test("Data.toCbor writes a constructor index beyond 64 bits as the writer does") {
        for (bits, positive, expected) <- wideIndices do {
            val magnitude = (BigInt(1) << bits) - 1
            // a `Constr` of this library has no negative index
            if positive then
                assert(hex(Constr(magnitude, PList.Nil).toCbor) == expected, s"index of $bits bits")
        }
    }

    test("a constructor index of 2^64 is a 9-byte big number") {
        val written = DataCborWriter.encode(Constr(BigInt(1) << 64, PList.Nil))
        assert(hex(written) == "d86682c249" + "01" + "0000000000000000" + "80")
    }

    test("byte strings are chunked by 64 bytes") {
        for n <- Seq(0, 1, 63, 64, 65, 127, 128, 129, 200) do
            assert(iterative(bytes(n)) == recursive(bytes(n)), s"$n bytes")
    }

    test("empty and nested containers") {
        val empties = Seq(
          Data.List(PList.Nil),
          Data.Map(PList.Nil),
          Constr(0, PList.Nil),
          Data.List(PList.singleton(Data.List(PList.Nil))),
          Data.Map(PList.from(Seq((Data.List(PList.Nil), Data.Map(PList.Nil)), (I(1), I(2)))))
        )
        for d <- empties do assert(iterative(d) == recursive(d), d.toString)
    }
}
