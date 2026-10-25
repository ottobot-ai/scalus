package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
import scalus.serialization.flat.FlatDecodingError
import scalus.uplc.FlatDecodeOracle.Row
import scalus.uplc.FlatProgramBuilder.*
import scalus.utils.Hex.hexToBytes

/** Naturals wider than the type the reference reads them as. A program's version components are
  * `Natural`s of any size in the reference, and it refuses a version it does not know when it sets
  * the script up, not when it deserialises it. A variable index or a `constr` tag is a `Word64`,
  * read by flat's `dWord64` (`PlutusCore/Flat/Decoder/Strict.hs`), which fails on a natural of 2^64
  * or more. Scalus used to wrap both.
  */
class FlatWideNaturalTest extends AnyFunSuite {

    // Verdicts of `deserialiseScript` followed by `evaluateScript` of plutus-ledger-api 1.70.0.0
    // (Plutus commit 507cab8cf6192e5143f12be5b2d3005a2928dc6d) on these scripts, see
    // `FlatDecodeOracle`. The scripts are rebuilt here, or taken from the probe the reference ran,
    // and checked against the row's hash.
    //
    // `version-minor-2p32-constr` tells the fix from the old wrapping: the version is 1.2^32.0, no
    // version that `evaluateScript` knows (it fails at its setup check, which Scalus has no
    // counterpart of), but `deserialiseScript` admits the program, `constr` term and all. Wrapped
    // to 1.0.0 the version cannot carry a `constr`, and Scalus refused the program.
    private val rows = Seq(
      Row(
        "version-major-wide",
        "b7756c2a2b849d43d6472820c925c5a6ea1f0a823bbdc6be82f285c3786deee5",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "version-minor-wide",
        "c4cfae6af50a3905a54bc0f102e12f5b730c2be0dbce58105e229ea6588077c9",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "version-minor-2p32-constr",
        "39d804a310a5fff9861ccccb8f6eebd048d472d657404ab1ba2da68b46137161",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "scope-natural-65",
        "7c570241c07697682468c9c84d85fffe55cb6db2964dd1549d1dd95ba61c8482",
        "",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11"
      ),
      Row(
        "var-10-groups-padded",
        "c7afd2827138533e50384417239cf3102de8070df1f68b07deb3cd631b96a033",
        "1/10 2/10 3/10",
        ""
      ),
      Row(
        "var-11-groups-padded",
        "6fc764f82916c470ee53b9085f846af98004c517554897984d796220aee99930",
        "",
        "1/10 2/10 3/10"
      ),
      Row(
        "var-10-groups-bit63",
        "b64d0b66f371eed4afa37bc24ae8765f8f8a71cb149d155fba9df676c699057b",
        "1/10 2/10 3/10",
        ""
      ),
      Row(
        "var-10-groups-bit64",
        "2d20d3dce25a24747a81577ff5bdffbd6851a511a08a847f5eb6220fc9c34814",
        "",
        "1/10 2/10 3/10"
      )
    )

    private val wideMajor = (BigInt(1) + (BigInt(1) << 32), BigInt(0), BigInt(0))
    private val wideMinor = (BigInt(1), BigInt(1) + (BigInt(1) << 32), BigInt(0))

    // `(program 1.1.0 [(lam _ (con unit ())) (delay (var 18446744073709551616))])`: the index is a
    // natural of 65 bits (`scope-natural-65` of the reference's boundary probes)
    private val natural65 = "510101003249842020202020202020200081".hexToBytes

    private val scripts: Map[String, Array[Byte]] = Map(
      "version-major-wide" -> cborByteString(flatOf(wideMajor, unit)),
      "version-minor-wide" -> cborByteString(flatOf(wideMinor, unit)),
      "version-minor-2p32-constr" -> cborByteString(
        flatOf((BigInt(1), BigInt(1) << 32, BigInt(0)), Constr(0, Vector.empty))
      ),
      "scope-natural-65" -> natural65,
      // `Var` with a natural of ten groups of seven bits that holds 1, and with one of eleven
      // groups that holds 1; with one that holds bit 63, and with one that holds bit 64:
      // `dWord64` reads at most ten groups, and the tenth can hold only bit 63
      "var-10-groups-padded" -> "4f010000110818080808080808080001".hexToBytes,
      "var-11-groups-padded" -> "5001000011081808080808080808080001".hexToBytes,
      "var-10-groups-bit63" -> "4f010000110808080808080808080011".hexToBytes,
      "var-10-groups-bit64" -> "4f010000110808080808080808080021".hexToBytes
    )

    test("scripts with wide naturals are admitted as the reference admits them") {
        val disagreements = FlatDecodeOracle.disagreements(rows, scripts)
        assert(disagreements.isEmpty, disagreements.mkString("\n"))
    }

    test("a version component beyond 32 bits does not read as a smaller version") {
        val major = DeBruijnedProgram.fromFlatEncoded(flatOf(wideMajor, unit))
        assert(major.version._1 > 1)
        val minor = DeBruijnedProgram.fromFlatEncoded(flatOf(wideMinor, unit))
        assert(minor.version._1 == 1 && minor.version._2 > 1)
    }

    test("a program with a constr term behind a version component beyond 32 bits is read") {
        val program = DeBruijnedProgram.fromFlatEncoded(
          flatOf((BigInt(1), BigInt(1) << 32, BigInt(0)), Constr(0, Vector.empty))
        )
        assert(program.version._2 > 1)
        program.term match {
            case Term.Constr(tag, args, _) => assert(tag == Word64(0) && args.isEmpty)
            case other                     => fail(s"unexpected term $other")
        }
    }

    test("a variable index of 65 bits, or of more than ten groups, is refused") {
        for id <- Seq("scope-natural-65", "var-11-groups-padded", "var-10-groups-bit64") do
            assertThrows[FlatDecodingError](DeBruijnedProgram.fromFlatEncoded(scripts(id).drop(1)))
    }

    test("a variable index of ten groups that fits 64 bits is read") {
        for id <- Seq("var-10-groups-padded", "var-10-groups-bit63") do
            DeBruijnedProgram.fromFlatEncoded(scripts(id).drop(1))
    }
}
