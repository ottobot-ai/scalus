package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.serialization.flat.FlatDecodingError
import scalus.uplc.FlatDecodeOracle.Row
import scalus.uplc.FlatProgramBuilder.*
import scalus.uplc.builtin.internal.{DataCborDecoder, DataCborWriter}
import scalus.uplc.test.ArbitraryInstances
import scalus.uplc.{DeBruijnedProgram, FlatDecodeOracle, Term}

import scala.util.Try

/** A `data` constant has no limit on its nesting in the Plutus reference: `decodeData`
  * (`PlutusCore/Data.hs`) reads it with no depth bound, and the only size limit is 64 bytes for a
  * byte string or a chunk of one.
  */
class DataConstantDepthTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {

    // Verdicts of `deserialiseScript` followed by `evaluateScript` of plutus-ledger-api 1.70.0.0
    // (Plutus commit 507cab8cf6192e5143f12be5b2d3005a2928dc6d) on these scripts, see
    // `FlatDecodeOracle`. The scripts are rebuilt here and checked against the row's hash.
    private val rows = Seq(
      Row(
        "data-depth-1001",
        "be7d09504f71e5f3e27b8776f6cb495acdd669036f1ed592045e45f5004ca539",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "data-depth-1001-result",
        "b09433123908dfcd573f4fd9e623f2b32d68c7c4267d31644ba45a6450595d0f",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "data-depth-1001-serialised",
        "53ac8250d80be67bcc5e2bfcd128a0ac0e21e9fbae0d14293e55cdb247d5db5e",
        "2/9 3/9 2/10 3/10 1/11 2/11 3/11",
        "1/9 1/10"
      ),
      Row(
        "data-depth-1003",
        "c7dc5566502eef527a31724128ed1dbb68f02df875d1d19a5ed9657dbe86ab86",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "data-depth-1003-result",
        "5c77445a5253127b61acc1a30238d790141c4850b24f493f77e55b0855f35baa",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "data-depth-1003-serialised",
        "02b83338583e1f3d2aa9383e1e13c761ca71aab40be3a8578adf108f38e60e3b",
        "2/9 3/9 2/10 3/10 1/11 2/11 3/11",
        "1/9 1/10"
      ),
      Row(
        "data-depth-16000",
        "20abb2da518aa6d5292b0fb42f712c2a5b89305b8a07627dc2635a06cb995b60",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "data-depth-16000-result",
        "e212d7a3ee7dc98b6a3a4b0565f492146409feb374e2b317d836a41241703e71",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
      Row(
        "data-depth-16000-serialised",
        "fcf43739f3e4ddb40ee4feb471b2b3e271456acbc888397190ab3e75eb73d6ab",
        "2/9 3/9 2/10 3/10 1/11 2/11 3/11",
        "1/9 1/10"
      ),
      Row(
        "data-depth-1003-equal",
        "46ac5e4c42878315639da777a8b2236b113b7a03fc7ba2574b6834cc41989e7f",
        "1/9 2/9 3/9 1/10 2/10 3/10 1/11 2/11 3/11",
        ""
      ),
    )

    private val v100 = (1, 0, 0)

    private val serialiseDataTag = 51
    private val equalsDataTag = 47

    private val scripts: Map[String, Array[Byte]] = {
        val nested = Seq(1001, 1003, 16000).flatMap { levels =>
            val deep = data(nestedList(levels))
            Seq(
              s"data-depth-$levels" -> script(v100, discard(deep)),
              s"data-depth-$levels-result" -> script(v100, deep),
              s"data-depth-$levels-serialised" ->
                  script(v100, discard(Ap(Builtin(serialiseDataTag), deep)))
            )
        }
        val equal = "data-depth-1003-equal" -> script(
          v100,
          discard(
            Ap(Ap(Builtin(equalsDataTag), data(nestedList(1003))), data(nestedList(1003)))
          )
        )
        (nested :+ equal).toMap
    }

    /** Runs `body` on a thread whose stack is far too small for one stack frame per nesting level
      * of a 1,001-level `data` constant, let alone 16,000.
      */
    private def onSmallStack[A](body: => A): A = {
        val result = new java.util.concurrent.atomic.AtomicReference[Either[Throwable, A]]()
        val worker = new Thread(
          null,
          () =>
              result.set(
                try Right(body)
                catch { case e: Throwable => Left(e) }
              ),
          "small-stack",
          256L * 1024
        )
        worker.start()
        worker.join()
        // a StackOverflowError is a fatal error to ScalaTest: report it as a failed test instead
        result.get().fold(e => throw new RuntimeException(s"on the small stack: $e", e), identity)
    }

    test("scripts with deeply nested data constants are admitted as the reference admits them") {
        val disagreements = onSmallStack(FlatDecodeOracle.disagreements(rows, scripts))
        assert(disagreements.isEmpty, disagreements.mkString("\n"))
    }

    /** The number of lists around the integer 0, counted with a loop: `==`, `hashCode` and
      * `toString` of `Data` recurse once per level.
      */
    private def levelsAround(zero: Data): Int = {
        var levels = 0
        var current = zero
        var continue = true
        while continue do
            current match {
                case Data.List(items) =>
                    items.toScalaList match {
                        case inner :: Nil =>
                            levels += 1
                            current = inner
                        case _ => continue = false
                    }
                case _ => continue = false
            }
        assert(current == Data.I(0))
        levels
    }

    for levels <- Seq(1001, 1003, 16000) do
        test(s"a data constant of $levels levels decodes to as many nested lists") {
            val flat = flatOf((1, 0, 0), discard(data(nestedList(levels))))
            val program = onSmallStack(DeBruijnedProgram.fromFlatEncoded(flat))
            val constants = program.term match {
                case Term.Apply(_, Term.Const(scalus.uplc.Constant.Data(d), _), _) => d
                case other => fail(s"unexpected term shape $other")
            }
            assert(levelsAround(constants) == levels)
        }

    // The constant is written again in the form of `serialiseData`, with indefinite lists, so the
    // bytes are not those it was read from; what is checked is that a constant too deep for the
    // stack of the thread can be encoded, and reads back as the same nesting.
    for levels <- Seq(1001, 16000) do
        test(s"a decoded data constant of $levels levels is encoded again") {
            val flat = flatOf((1, 0, 0), discard(data(nestedList(levels))))
            def constantOf(program: DeBruijnedProgram): Data = program.term match {
                case Term.Apply(_, Term.Const(scalus.uplc.Constant.Data(d), _), _) => d
                case other => fail(s"unexpected term shape $other")
            }
            val (written, reread) = onSmallStack {
                val program = DeBruijnedProgram.fromFlatEncoded(flat)
                (
                  DataCborWriter.encode(constantOf(program)).toSeq,
                  DeBruijnedProgram.fromFlatEncoded(program.flatEncoded)
                )
            }
            val indefinite = Array.fill[Byte](levels)(0x9f.toByte) :+ 0x00.toByte
            assert(written == (indefinite ++ Array.fill[Byte](levels)(0xff.toByte)).toSeq)
            assert(levelsAround(constantOf(reread)) == levels)
        }

    test("the data decoder agrees with the borer decoder below its nesting limit") {
        def strict(bytes: Array[Byte]): Data = {
            val (value, end) = DataCborDecoder.decodePrefix(bytes)
            if end != bytes.length then throw new FlatDecodingError("trailing bytes")
            value
        }
        forAll(minSuccessful(2000)) { (d: Data, position: Int, flip: Byte) =>
            val bytes = d.toCbor
            assert(strict(bytes) == d)
            val at = Math.floorMod(position, bytes.length)
            val flipped = bytes.clone()
            flipped(at) = (flipped(at) ^ (flip | 1)).toByte
            val truncated = bytes.take(at)
            for mutated <- Seq(flipped, truncated) do {
                // borer sizes its array from the header of a definite-length list, so a header
                // that claims more elements than the input holds can end in an error that is not
                // a decoding error; that is a refusal here
                val expected =
                    try Try(Data.fromCbor(mutated))
                    catch { case e: OutOfMemoryError => scala.util.Failure(e) }
                val actual = Try(strict(mutated))
                assert(
                  actual.isSuccess == expected.isSuccess,
                  s"${mutated.toSeq}: $expected vs $actual"
                )
                if actual.isSuccess then assert(actual.get == expected.get)
            }
        }
    }
}
