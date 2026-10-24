package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.FlatDecodeOracle.Row
import scalus.uplc.FlatProgramBuilder.*
import scalus.uplc.{Constant, DeBruijnedProgram, FlatDecodeOracle, Term}
import scalus.utils.Hex.hexToBytes

/** `decodeData` classifies a CBOR item by its head byte before it looks at a tag's value: cborg's
  * `peekTokenType` calls only the heads 0xc2 and 0xc3 a bignum, any other tag head (0xd8 and a
  * byte, 0xd9, 0xda, 0xdb, ...) is a plain tag and must be a constructor tag, so tag 2 or 3 behind
  * a longer head is refused, while a constructor tag may have any head width.
  */
class DataConstantTagHeadsTest extends AnyFunSuite {

    // Verdicts of `deserialiseScript` followed by `evaluateScript` of plutus-ledger-api 1.70.0.0
    // (Plutus commit 507cab8cf6192e5143f12be5b2d3005a2928dc6d) on these scripts, see
    // `FlatDecodeOracle`. The scripts are rebuilt here and checked against the row's hash.
    private val rows = Seq(
      Row(
        "data-tag2-short-head",
        "46bfe6b61ccf9fe2dd6d3d1fd44f27f108c2a7f0416153d5b0796aa88f24758e",
        "1/10 2/10 3/10",
        ""
      ),
      Row(
        "data-tag2-wide-head",
        "514d2fa6a16b91c764469038e5bdf21ab3d8ae512dc5f6f51cee76a7c7079e6e",
        "",
        "1/10 2/10 3/10"
      ),
      Row(
        "data-tag3-wide-head",
        "8560fe9fc7cb60e8be4640812f4c32dfbb2913cf61cb7fe41db7ac69861ec69d",
        "",
        "1/10 2/10 3/10"
      ),
      Row(
        "data-tag3-short-head",
        "63271cab469e8f8a8d152dc45895361136338c7eb92c15d37f49fbba7bade46b",
        "1/10 2/10 3/10",
        ""
      ),
      Row(
        "data-tag102-wide64-head",
        "f6b050a4aa22d11e1a35e16f70d5528f8185b198319cd7452289abf2c9ad006a",
        "1/10 2/10 3/10",
        ""
      ),
      Row(
        "data-tag102-standard",
        "2512b333ea0a072f89b1d6bbb8098db0df90763d572fa359317e0b03b94e7e11",
        "1/10 2/10 3/10",
        ""
      ),
      Row(
        "data-tag121-wide4-head",
        "e29e5bc1fb1a4e228fe31c06bd299790baf9283e1428f44a8d1ed2c8c876d376",
        "1/10 2/10 3/10",
        ""
      ),
    )

    private val constants: Map[String, String] = Map(
      "data-tag2-short-head" -> "c24101",
      "data-tag2-wide-head" -> "d8024101",
      "data-tag3-short-head" -> "c34101",
      "data-tag3-wide-head" -> "d8034101",
      "data-tag102-standard" -> "d866820080",
      "data-tag102-wide64-head" -> "db0000000000000066820080",
      "data-tag121-wide4-head" -> "da0000007980"
    )

    private val scripts: Map[String, Array[Byte]] =
        constants.map { case (id, cbor) => id -> script((1, 0, 0), discard(data(cbor.hexToBytes))) }

    test(
      "data constants with bignum and constructor tags are admitted as the reference admits them"
    ) {
        val disagreements = FlatDecodeOracle.disagreements(rows, scripts)
        assert(disagreements.isEmpty, disagreements.mkString("\n"))
    }

    test("a bignum behind the short head decodes to its value") {
        def constant(id: String): Data =
            DeBruijnedProgram
                .fromFlatEncoded(flat((1, 0, 0), discard(data(constants(id).hexToBytes))))
                .term match {
                case Term.Apply(_, Term.Const(Constant.Data(d), _), _) => d
                case other => fail(s"unexpected term shape $other")
            }
        assert(constant("data-tag2-short-head") == Data.I(1))
        assert(constant("data-tag3-short-head") == Data.I(-2))
    }
}
