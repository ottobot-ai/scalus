package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{PlutusScript, Script}
import scalus.serialization.flat.FlatDecodingError
import scalus.uplc.FlatDecodeOracle.Row
import scalus.uplc.builtin.ByteString
import scalus.uplc.FlatProgramBuilder.*

/** A flat program must end the bytes it is read from. The reference's `unflatWith` reads the filler
  * behind the program (zero or more 0 bits and a 1 bit, `PlutusCore/Flat/Filler.hs`) and then
  * requires the end of the input right there (`strictDecoder`, `PlutusCore/Flat/Decoder/Run.hs`):
  * bytes after the filler, a filler without its 1 bit and a filler that ends inside a byte are
  * refused, and a filler longer than the minimum is not.
  */
class FlatProgramEndingTest extends AnyFunSuite {

    // Verdicts of `deserialiseScript` followed by `evaluateScript` of plutus-ledger-api 1.70.0.0
    // (Plutus commit 507cab8cf6192e5143f12be5b2d3005a2928dc6d) on these scripts, see
    // `FlatDecodeOracle`. The scripts are rebuilt here and checked against the row's hash.
    private val rows = Seq(
      Row(
        "filler-minimal",
        "eb57b149a70ea9db471feca2ce3eab4119dfd51f6d6fb5a21cdc0de9ab87d7b2",
        "1/5 1/6 1/7 2/7 1/8 2/8 1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "filler-trailing-zero-byte",
        "7fb48d5d8749cd713f6b82d5a4731acae40347d7c810c712dc3783eef1185bee",
        "",
        "1/5 1/6 1/7 2/7 1/8 2/8 1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11"
      ),
      Row(
        "filler-trailing-ff-byte",
        "ab95306742675961df5cdc7d475884ec9b1253a2ab5c2da5d171d0b9f35145c9",
        "",
        "1/5 1/6 1/7 2/7 1/8 2/8 1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11"
      ),
      Row(
        "filler-longer",
        "055d938b4847ecd81f93b41d619d2d0c4aacdf7f14e3a1b4e51a543e4abb19ab",
        "1/5 1/6 1/7 2/7 1/8 2/8 1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "filler-without-one",
        "db882f35bf9a462de6b6dcbf15016c6eb8ef2998809e87fb8b620ac3a1053684",
        "",
        "1/5 1/6 1/7 2/7 1/8 2/8 1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11"
      ),
      Row(
        "filler-misaligned",
        "a115cb01effe779ba7d3a5e0d6a0802dcb428a585631ae3fcb6fe83a7ee8ce21",
        "",
        "1/5 1/6 1/7 2/7 1/8 2/8 1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11"
      ),
      Row(
        "cbor-remainder-and-flat-trailing",
        "4d2e2895bb9f6293b9d95565deefdacbf56ffa4c579c4ffc16bada1207594274",
        "",
        "1/5 1/6 1/7 2/7 1/8 2/8 1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11"
      ),
    )

    private val version = (BigInt(1), BigInt(0), BigInt(0))

    /** The flat bytes of the program, built from its bits up to the filler. */
    private val flats: Map[String, Array[Byte]] = {
        def ending(end: Bits => Array[Byte]): Array[Byte] = end(unpadded(version, unit))
        Map(
          "filler-minimal" -> ending(_.bytes),
          "filler-trailing-zero-byte" -> ending(_.bytes :+ 0x00.toByte),
          "filler-trailing-ff-byte" -> ending(_.bytes :+ 0xff.toByte),
          // 0 bits up to a byte boundary, then a whole byte of the filler: 0000000 00000001
          "filler-longer" -> ending { b =>
              b.zeros()
              b.bits(1, 8)
              b.packed
          },
          "filler-without-one" -> ending { b =>
              b.zeros()
              b.packed
          },
          "filler-misaligned" -> ending { b =>
              b.bit(1)
              b.zeros()
              b.packed
          }
        )
    }

    private val scripts: Map[String, Array[Byte]] =
        flats.map { case (id, flat) => id -> cborByteString(flat) } +
            // the bytes that follow the filler are inside the CBOR byte string, and then one more
            // byte follows the string itself
            ("cbor-remainder-and-flat-trailing" ->
                (cborByteString(flats("filler-minimal") :+ 0x00.toByte) :+ 0x00.toByte))

    test("scripts with bytes after the flat program are admitted as the reference does") {
        val disagreements = FlatDecodeOracle.disagreements(rows, scripts)
        assert(disagreements.isEmpty, disagreements.mkString("\n"))
    }

    test("a program that ends exactly behind its filler decodes, whether the filler is minimal") {
        for id <- Seq("filler-minimal", "filler-longer") do
            assert(
              DeBruijnedProgram.fromFlatEncoded(flats(id)).term == Term.Const(Constant.Unit),
              id
            )
    }

    test(
      "a program whose filler is missing or misplaced, or that is followed by bytes, is refused"
    ) {
        for id <- Seq(
              "filler-trailing-zero-byte",
              "filler-trailing-ff-byte",
              "filler-without-one",
              "filler-misaligned"
            )
        do assertThrows[FlatDecodingError](DeBruijnedProgram.fromFlatEncoded(flats(id)))
    }

    test("the program of a script refuses bytes after its filler, in every language") {
        val script = ByteString.fromArray(scripts("filler-trailing-zero-byte"))
        for
            make <- Seq[ByteString => PlutusScript](
              Script.PlutusV1(_),
              Script.PlutusV2(_),
              Script.PlutusV3(_)
            )
            decode <- Seq[PlutusScript => Any](_.program, _.deBruijnedProgram)
        do assertThrows[FlatDecodingError](decode(make(script)))
    }

    test("decoding with the remaining bytes still returns them") {
        val result = ProgramFlatCodec.decodeFlatWithRemainingBytes(flats("filler-trailing-ff-byte"))
        assert(result.remainder.sameElements(Array(0xff.toByte)))
    }
}
