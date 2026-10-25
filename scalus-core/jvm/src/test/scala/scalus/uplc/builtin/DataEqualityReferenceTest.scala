package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.*
import scalus.uplc.builtin.Data.{Constr, I}
import scalus.uplc.eval.{NoBudgetSpender, NoLogger, PlutusVM}

import java.security.MessageDigest
import scala.io.Source
import scala.util.Try

/** Replays the verdicts of the reference on scripts that compare maps with `equalsData`: reordered
  * and repeated entries, maps inside lists, constructors and maps, maps as keys, and nesting 900
  * levels deep.
  *
  * Each script ends in `()` when `equalsData left right` is `True` and in `error` otherwise. The
  * expected verdicts are rows of `equals-data-verdicts.tsv`, taken from the executed reference
  * (plutus 1.70.0.0, `evaluateScript`) for Plutus V1 to V3 at protocols 9 to 11; no expectation is
  * written down in this file. A row also holds the SHA-256 of the script it was obtained for, and
  * the test checks that the script built here is that one.
  */
class DataEqualityReferenceTest extends AnyFunSuite {
    private def i(n: Int): Data = I(BigInt(n))
    private def map(entries: (Data, Data)*): Data = Data.Map(PList.from(entries.toList))
    private def list(items: Data*): Data = Data.List(PList.from(items.toList))
    private def constr(tag: Int, fields: Data*): Data =
        Constr(BigInt(tag), PList.from(fields.toList))

    private val ab = map(i(1) -> i(1), i(2) -> i(2))
    private val ba = map(i(2) -> i(2), i(1) -> i(1))
    private val abDuplicate = map(i(1) -> i(1), i(2) -> i(2), i(1) -> i(1))
    private val abReps = map(i(1) -> i(1), i(1) -> i(2))
    private val baReps = map(i(1) -> i(2), i(1) -> i(1))

    private val depth = 900

    private def nest(bottom: Data)(wrap: Data => Data): Data =
        (1 to depth).foldLeft(bottom)((inner, _) => wrap(inner))

    private val probes: Seq[(String, Data, Data)] = Seq(
      ("equal-maps", ab, ab),
      ("equal-maps-rebuilt", ab, map(i(1) -> i(1), i(2) -> i(2))),
      ("empty-maps", map(), map()),
      ("reordered", ab, ba),
      ("duplicate-pair", ab, abDuplicate),
      ("duplicate-pair-single", map(i(1) -> i(1), i(1) -> i(1)), map(i(1) -> i(1))),
      ("duplicate-key-reordered", abReps, baReps),
      ("duplicate-key-equal", abReps, abReps),
      ("duplicate-key-dropped", abReps, map(i(1) -> i(1))),
      ("different-values", ab, map(i(1) -> i(1), i(2) -> i(3))),
      ("map-and-empty-list", map(), list()),
      ("list-of-maps-equal", list(ab, ba), list(ab, ba)),
      ("list-of-maps-reordered-inside", list(ab), list(ba)),
      ("list-of-maps-swapped", list(ab, ba), list(ba, ab)),
      ("constr-with-map-equal", constr(0, ab), constr(0, ab)),
      ("constr-with-map-reordered-inside", constr(0, ab), constr(0, ba)),
      ("constr-index-differs", constr(0, ab), constr(1, ab)),
      ("map-of-maps-equal", map(i(0) -> ab, i(1) -> ba), map(i(0) -> ab, i(1) -> ba)),
      ("map-of-maps-inner-reordered", map(i(0) -> ab), map(i(0) -> ba)),
      ("map-of-maps-outer-reordered", map(i(0) -> ab, i(1) -> ba), map(i(1) -> ba, i(0) -> ab)),
      ("map-key-is-map-reordered", map(ab -> i(0)), map(ba -> i(0))),
      (
        "map-in-constr-in-list-in-map",
        map(i(7) -> list(constr(2, ab))),
        map(i(7) -> list(constr(2, ba)))
      ),
      ("deep-lists-equal", nest(ab)(x => list(x)), nest(ab)(x => list(x))),
      ("deep-lists-bottom-reordered", nest(ab)(x => list(x)), nest(ba)(x => list(x))),
      (
        "deep-maps-equal",
        nest(ab)(x => map(i(0) -> x)),
        nest(ab)(x => map(i(0) -> x))
      ),
      (
        "deep-maps-bottom-reordered",
        nest(ab)(x => map(i(0) -> x)),
        nest(ba)(x => map(i(0) -> x))
      ),
      (
        "deep-constrs-bottom-duplicate",
        nest(ab)(x => constr(0, x)),
        nest(abDuplicate)(x => constr(0, x))
      )
    )

    /** `force (ifThenElse (equalsData left right) (delay ()) (delay error))`. */
    private def script(left: Data, right: Data): Array[Byte] = {
        val equal = Term.Apply(
          Term.Apply(Term.Builtin(DefaultFun.EqualsData), Term.Const(Constant.Data(left))),
          Term.Const(Constant.Data(right))
        )
        val check = Term.Force(
          Term.Apply(
            Term.Apply(
              Term.Apply(Term.Force(Term.Builtin(DefaultFun.IfThenElse)), equal),
              Term.Delay(Term.Const(Constant.Unit))
            ),
            Term.Delay(Term.Error())
          )
        )
        Program((1, 0, 0), check).cborEncoded
    }

    private def sha256(bytes: Array[Byte]): String =
        MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

    private case class Row(id: String, language: Int, protocol: Int, sha: String, success: Boolean)

    private val rows: Seq[Row] = {
        val source = Source.fromResource("plutus-reference/equals-data-verdicts.tsv")
        try
            source.getLines().filterNot(_.startsWith("#")).toVector.map { line =>
                val cells = line.split("\t", -1)
                Row(cells(0), cells(1).toInt, cells(2).toInt, cells(3), cells(4) == "success")
            }
        finally source.close()
    }

    private def vm(language: Int, protocol: Int): PlutusVM = {
        val pv = MajorProtocolVersion(protocol)
        language match
            case 1 => PlutusVM.makePlutusV1VM(pv)
            case 2 => PlutusVM.makePlutusV2VM(pv)
            case 3 => PlutusVM.makePlutusV3VM(pv)
    }

    test("the probes are the scripts of the recorded rows") {
        assert(rows.map(_.id).distinct.toSet == probes.map(_._1).toSet)
        for (id, left, right) <- probes do {
            val digest = sha256(script(left, right))
            for row <- rows.filter(_.id == id) do assert(row.sha == digest, id)
        }
    }

    for (id, left, right) <- probes do
        test(s"equalsData verdict of the reference: $id") {
            val bytes = script(left, right)
            val program = DeBruijnedProgram.fromCbor(bytes)
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
