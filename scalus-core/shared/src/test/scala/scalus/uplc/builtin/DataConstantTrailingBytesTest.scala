package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.FlatDecodeOracle.Row
import scalus.uplc.FlatProgramBuilder.*
import scalus.uplc.{Constant, DeBruijnedProgram, FlatDecodeOracle, Term}

/** The flat decoder reads the first CBOR item of a `data` constant and drops what follows it: the
  * reference reads the constant through `FlatViaSerialise`, whose `deserialiseOrFail` returns the
  * value as soon as the decoder is done (`Codec/Serialise.hs` 134-143 of serialise 0.2.6.1).
  */
class DataConstantTrailingBytesTest extends AnyFunSuite {

    // Verdicts of `deserialiseScript` followed by `evaluateScript` of plutus-ledger-api 1.70.0.0
    // (Plutus commit 507cab8cf6192e5143f12be5b2d3005a2928dc6d) on these scripts, see
    // `FlatDecodeOracle`. The scripts are rebuilt here and checked against the row's hash.
    private val rows = Seq(
      Row(
        "data-trailing-zero",
        "07cb370d4ec3ab07ab86f9c5220cdfd900f144e2c23bb0107743be0e0bbcadc8",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "data-trailing-bytes",
        "5f0cd28f7c9140991f222bf9838cf9399a5136593e3a4bd3f00fb4c55b77c50d",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "data-trailing-truncated-item",
        "77c36ce20911033d29f2f4f8d5260d5c0a012bb7d427493e11fbd4837560e09b",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "data-truncated",
        "94b1d53f3b068952f4c8dbeb396b0741f20c8015b71a87a853c787c731034150",
        "",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11"
      ),
    )

    private val v100 = (1, 0, 0)

    private val constants: Map[String, Array[Byte]] = Map(
      "data-trailing-zero" -> Array[Byte](0x00, 0x00),
      "data-trailing-bytes" -> Array[Byte](0x00, 0xff.toByte, 0x1c),
      "data-trailing-truncated-item" -> Array[Byte](0x00, 0x58),
      "data-truncated" -> Array[Byte](0x82.toByte, 0x00)
    )

    private val scripts: Map[String, Array[Byte]] =
        constants.map { case (id, cbor) => id -> script(v100, discard(data(cbor))) }

    test(
      "scripts with bytes after a data constant's CBOR item are admitted as the reference does"
    ) {
        val disagreements = FlatDecodeOracle.disagreements(rows, scripts)
        assert(disagreements.isEmpty, disagreements.mkString("\n"))
    }

    test("the bytes after the first CBOR item do not change the decoded constant") {
        for id <- Seq("data-trailing-zero", "data-trailing-bytes", "data-trailing-truncated-item")
        do {
            val program =
                DeBruijnedProgram.fromFlatEncoded(flat(v100, discard(data(constants(id)))))
            program.term match {
                case Term.Apply(_, Term.Const(Constant.Data(d), _), _) =>
                    assert(d == Data.I(0), id)
                case other => fail(s"unexpected term shape $other")
            }
        }
    }

    test("a data constant that ends inside its first CBOR item is refused") {
        assertThrows[scalus.serialization.flat.FlatDecodingError] {
            DeBruijnedProgram.fromFlatEncoded(
              flat(v100, discard(data(constants("data-truncated"))))
            )
        }
    }
}
