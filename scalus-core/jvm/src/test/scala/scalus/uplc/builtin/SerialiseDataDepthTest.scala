package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Data.{B, Constr, I}
import scalus.uplc.builtin.internal.DataCborWriter

import java.util.concurrent.atomic.AtomicReference

/** `serialiseData` has no nesting limit in the reference: `PlutusCore.Data.encodeData` recurses
  * through `serialise` without a depth bound (plutus 1.70.0.0,
  * `plutus-core/plutus-core/src/PlutusCore/Data.hs`, commit
  * 507cab8cf6192e5143f12be5b2d3005a2928dc6d).
  */
class SerialiseDataDepthTest extends AnyFunSuite {

    /** Runs `body` on a thread with a small stack, so that a recursive implementation fails. */
    private def onSmallStack[A](body: => A): A = {
        val result = new AtomicReference[Either[Throwable, A]]()
        val thread = new Thread(
          null,
          () =>
              result.set(
                try Right(body)
                catch { case t: Throwable => Left(t) }
              ),
          "serialise-data-small-stack",
          256L * 1024
        )
        thread.start()
        thread.join()
        result.get().fold(t => fail(s"failed on the small stack: $t"), identity)
    }

    /** `depth` nested one-element lists around an integer, built without recursion. */
    private def nestedLists(depth: Int): Data =
        (0 until depth).foldLeft(I(1): Data)((d, _) => Data.List(PList.singleton(d)))

    private def nestedConstrs(depth: Int): Data =
        (0 until depth).foldLeft(B(ByteString.fromArray(Array.fill(70)(7.toByte))): Data)((d, _) =>
            Constr(0, PList.singleton(d))
        )

    private def nestedMaps(depth: Int): Data =
        (0 until depth).foldLeft(I(1): Data)((d, _) => Data.Map(PList.singleton((I(0), d))))

    private def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString

    // `depth` opening bytes, then the innermost item, then `depth` break bytes
    private def expectedLists(depth: Int): String =
        "9f" * depth + "01" + "ff" * depth

    for depth <- Seq(1001, 16000) do {
        test(s"serialiseData of $depth nested lists on a small stack") {
            val bytes = onSmallStack(Builtins.serialiseData(nestedLists(depth)).bytes)
            assert(hex(bytes) == expectedLists(depth))
        }

        test(s"Data.toCbor of $depth nested lists on a small stack") {
            val bytes = onSmallStack(nestedLists(depth).toCbor)
            assert(hex(bytes) == expectedLists(depth))
        }

        test(s"serialiseData of $depth nested constrs with a long byte string on a small stack") {
            val bytes = onSmallStack(Builtins.serialiseData(nestedConstrs(depth)).bytes)
            // tag 121 and an indefinite array per level; 70 bytes are chunked as 64 + 6
            val inner =
                "5f" + "5840" + "07" * 64 + "46" + "07" * 6 + "ff"
            assert(hex(bytes) == ("d8799f" * depth) + inner + ("ff" * depth))
        }

        test(s"serialiseData of $depth nested maps on a small stack") {
            val bytes = onSmallStack(Builtins.serialiseData(nestedMaps(depth)).bytes)
            assert(hex(bytes) == ("a100" * depth) + "01")
        }
    }

    test("Data.toCbor of 100000 nested lists and maps on a small stack is the writer's bytes") {
        val lists = nestedLists(100000)
        val maps = nestedMaps(100000)
        val (viaToCbor, viaWriter) = onSmallStack(
          (
            (lists.toCbor.toSeq, maps.toCbor.toSeq),
            (DataCborWriter.encode(lists).toSeq, DataCborWriter.encode(maps).toSeq)
          )
        )
        assert(viaToCbor == viaWriter)
        assert(viaToCbor._1.length == 200001)
    }
}
