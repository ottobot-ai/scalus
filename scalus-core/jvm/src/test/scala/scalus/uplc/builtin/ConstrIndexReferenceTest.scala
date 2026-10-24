package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.uplc.*
import scalus.uplc.eval.{NoBudgetSpender, NoLogger, PlutusVM}

import java.security.MessageDigest
import scala.io.Source
import scala.util.Try

/** Replays the verdicts of the reference on scripts that build data with `constrData` at the sign
  * and 64-bit boundaries of the index, and then read it with `serialiseData`, `unConstrData`,
  * `equalsData` and `chooseData`.
  *
  * Each script ends in `()` when its property holds and in `error` otherwise. The expected verdicts
  * are rows of `constr-index-verdicts.tsv`, taken from the executed reference (plutus 1.70.0.0,
  * `evaluateScript`) for Plutus V2 and V3 at protocols 9 and 10, and the bytes that `serialiseData`
  * must produce are those of `constr-index-serialise-data.tsv`; no expectation is written down in
  * this file. A row also holds the SHA-256 of the script it was obtained for, and the test checks
  * that the script built here is that one.
  */
class ConstrIndexReferenceTest extends AnyFunSuite {
    private case class Index(value: BigInt, empty: String, oneField: String)

    private def lines(resource: String): Vector[Array[String]] = {
        val source = Source.fromResource(resource)
        try
            source.getLines().filterNot(_.startsWith("#")).toVector.map(_.split("\t", -1))
        finally source.close()
    }

    private val indices: Vector[Index] =
        lines("plutus-reference/constr-index-serialise-data.tsv").map(c =>
            Index(BigInt(c(0)), c(1), c(2))
        )

    private def constr(i: BigInt, fields: String) =
        s"[ (builtin constrData) (con integer $i) (con (list data) [$fields]) ]"

    private def holds(condition: String) =
        s"(force [ (force (builtin ifThenElse)) $condition (delay (con unit ())) (delay (error)) ])"

    private val fst = "(force (force (builtin fstPair)))"
    private val snd = "(force (force (builtin sndPair)))"

    private def bodies(index: Index): Vector[(String, String)] = {
        val i = index.value
        val empty = constr(i, "")
        val one = constr(i, "I 1")
        Vector(
          "serialise-empty" -> holds(
            s"[ (builtin equalsByteString) [ (builtin serialiseData) $empty ] (con bytestring #${index.empty}) ]"
          ),
          "serialise-field" -> holds(
            s"[ (builtin equalsByteString) [ (builtin serialiseData) $one ] (con bytestring #${index.oneField}) ]"
          ),
          "unconstr-index" -> holds(
            s"[ (builtin equalsInteger) [ $fst [ (builtin unConstrData) $one ] ] (con integer $i) ]"
          ),
          "unconstr-rebuild" -> holds(
            s"[ (lam p [ (builtin equalsData) [ (builtin constrData) [ $fst p ] [ $snd p ] ] $one ]) [ (builtin unConstrData) $one ] ]"
          ),
          "equals-same" -> holds(s"[ (builtin equalsData) $one $one ]"),
          "equals-next" -> holds(s"[ (builtin equalsData) $empty ${constr(i + 1, "")} ]"),
          "choose" -> ("(force [ (force (builtin chooseData)) " + empty +
              " (delay (con unit ())) (delay (error)) (delay (error)) (delay (error)) (delay (error)) ])")
        )
    }

    private def compiled(body: String): Array[Byte] =
        Program.parseUplc(s"(program 1.0.0 $body)").fold(e => fail(e), _.cborEncoded)

    private val probes: Vector[(String, Array[Byte])] =
        for
            index <- indices
            (name, body) <- bodies(index)
        yield (s"constr-index-${index.value}-$name", compiled(body))

    private def sha256(bytes: Array[Byte]): String =
        MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

    private case class Row(id: String, language: Int, protocol: Int, sha: String, success: Boolean)

    private val rows: Vector[Row] =
        lines("plutus-reference/constr-index-verdicts.tsv")
            .map(c => Row(c(0), c(1).toInt, c(2).toInt, c(3), c(4) == "success"))
            .filter(_.protocol <= 10)

    private def vm(language: Int, protocol: Int): PlutusVM = {
        val pv = MajorProtocolVersion(protocol)
        language match
            case 2 => PlutusVM.makePlutusV2VM(pv)
            case 3 => PlutusVM.makePlutusV3VM(pv)
    }

    test("the probes are the scripts of the recorded rows") {
        assert(rows.map(_.id).distinct.toSet == probes.map(_._1).toSet)
        for (id, script) <- probes do {
            val digest = sha256(script)
            for row <- rows.filter(_.id == id) do assert(row.sha == digest, id)
        }
    }

    for (id, script) <- probes do
        test(s"constrData verdict of the reference: $id") {
            val program = DeBruijnedProgram.fromCbor(script)
            for row <- rows.filter(_.id == id) do {
                val verdict = Try(
                  vm(row.language, row.protocol).evaluateScript(program, NoBudgetSpender, NoLogger)
                )
                assert(
                  verdict.isSuccess == row.success,
                  s"$id V${row.language} protocol ${row.protocol}: $verdict"
                )
            }
        }
}
