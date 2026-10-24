package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.flat.FlatDecodingError
import scalus.uplc.FlatDecodeOracle.Row
import scalus.uplc.FlatProgramBuilder.*

/** A `string` constant must be UTF-8: the reference decodes it with the strict `decodeUtf8'`
  * (`dUTF8`, `PlutusCore/Flat/Decoder/Strict.hs`) and fails on anything else, where Scalus used to
  * replace it with U+FFFD and go on.
  */
class FlatStringConstantTest extends AnyFunSuite {

    // Verdicts of `deserialiseScript` followed by `evaluateScript` of plutus-ledger-api 1.70.0.0
    // (Plutus commit 507cab8cf6192e5143f12be5b2d3005a2928dc6d) on these scripts, see
    // `FlatDecodeOracle`. The scripts are rebuilt here and checked against the row's hash.
    private val rows = Seq(
      Row(
        "string-utf8",
        "e6294e949d137df98e84717a98626c01d3072e5bbff14e536e97b1159a0ddda3",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "string-not-utf8",
        "705dab8b18863e0a99bdb43d209da15cbf0a97c86b94a8f5f11a26549d0b473b",
        "",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11"
      ),
      Row(
        "string-surrogate",
        "b4e09b65e2de565a64c00a93fd97fdef67b9190904858d30c6e6f19ededa9646",
        "",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11"
      ),
    )

    private val v100 = (1, 0, 0)

    private val constants: Map[String, Array[Byte]] = Map(
      "string-utf8" -> "ok".getBytes("UTF-8"),
      "string-not-utf8" -> Array[Byte](0xff.toByte, 0xfe.toByte),
      // U+D800, a surrogate, written as three bytes
      "string-surrogate" -> Array[Byte](0xed.toByte, 0xa0.toByte, 0x80.toByte)
    )

    private val scripts: Map[String, Array[Byte]] =
        constants.map { case (id, raw) => id -> script(v100, discard(string(raw))) }

    test("scripts with string constants are admitted as the reference admits them") {
        val disagreements = FlatDecodeOracle.disagreements(rows, scripts)
        assert(disagreements.isEmpty, disagreements.mkString("\n"))
    }

    test("a UTF-8 string constant decodes to its text") {
        val program =
            DeBruijnedProgram.fromFlatEncoded(flat(v100, discard(string(constants("string-utf8")))))
        program.term match {
            case Term.Apply(_, Term.Const(Constant.String(text), _), _) => assert(text == "ok")
            case other => fail(s"unexpected term $other")
        }
    }

    test("a string constant that is not UTF-8 is refused") {
        for id <- Seq("string-not-utf8", "string-surrogate") do
            assertThrows[FlatDecodingError](
              DeBruijnedProgram.fromFlatEncoded(flat(v100, discard(string(constants(id)))))
            )
    }
}
