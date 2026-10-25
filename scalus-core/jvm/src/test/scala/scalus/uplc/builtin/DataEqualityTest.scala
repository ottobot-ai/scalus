package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Data.{B, Constr, I}

import java.util.concurrent.atomic.AtomicReference

/** `equalsData` and `==` on [[Data]] follow the reference: the derived `Eq` of `PlutusCore.Data`
  * (plutus 1.70.0.0) compares the entries of a map as an ordered list. The reference's verdicts for
  * scripts are replayed by `DataEqualityReferenceTest`.
  */
class DataEqualityTest extends AnyFunSuite {
    private def i(n: Int): Data = I(BigInt(n))
    private def map(entries: (Data, Data)*): Data = Data.Map(PList.from(entries.toList))

    private val ab = map(i(1) -> i(1), i(2) -> i(2))
    private val ba = map(i(2) -> i(2), i(1) -> i(1))

    test("maps with the same entries in another order are different") {
        assert(ab != ba)
        assert(!Builtins.equalsData(ab, ba))
    }

    test("a repeated entry makes a map different") {
        val repeated = map(i(1) -> i(1), i(2) -> i(2), i(1) -> i(1))
        assert(ab != repeated)
        assert(map(i(1) -> i(1), i(1) -> i(1)) != map(i(1) -> i(1)))
        assert(!Builtins.equalsData(ab, repeated))
    }

    test("equal maps are equal and hash alike") {
        val rebuilt = map(i(1) -> i(1), i(2) -> i(2))
        assert(ab == rebuilt)
        assert(ab.hashCode == rebuilt.hashCode)
        assert(Builtins.equalsData(ab, rebuilt))
    }

    test("the order counts inside lists, constructors and map keys and values") {
        assert(Data.List(PList(ab)) != Data.List(PList(ba)))
        assert(Constr(0, PList(ab)) != Constr(0, PList(ba)))
        assert(map(ab -> i(0)) != map(ba -> i(0)))
        assert(map(i(0) -> ab) != map(i(0) -> ba))
        assert(Constr(0, PList(ab)) == Constr(0, PList(ab)))
    }

    test("different shapes are different") {
        assert(Data.Map(PList.Nil) != Data.List(PList.Nil))
        assert(Constr(0, PList.Nil) != Constr(1, PList.Nil))
        assert(Constr(0, PList(i(1))) != Constr(0, PList.Nil))
        assert(Data.List(PList(i(1), i(2))) != Data.List(PList(i(1))))
        assert(I(1) != B(ByteString.fromHex("01")))
        assert(!ab.equals("not data"))
    }

    test("a set of maps keeps both orders") {
        assert(Set(ab, ba, ab).size == 2)
    }

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
          "data-equality-small-stack",
          256L * 1024
        )
        thread.start()
        thread.join()
        result.get().fold(t => fail(s"failed on the small stack: $t"), identity)
    }

    private def deep(depth: Int, bottom: Data)(wrap: Data => Data): Data =
        (0 until depth).foldLeft(bottom)((inner, _) => wrap(inner))

    test("deep data is compared and hashed on a small stack") {
        val depth = 100000
        val wraps: Seq[(String, Data => Data)] = Seq(
          "list" -> (d => Data.List(PList.singleton(d))),
          "constr" -> (d => Constr(0, PList.singleton(d))),
          "map" -> (d => Data.Map(PList.singleton((i(0), d))))
        )
        for (name, wrap) <- wraps do {
            val left = deep(depth, ab)(wrap)
            val equal = deep(depth, ab)(wrap)
            val other = deep(depth, ba)(wrap)
            onSmallStack {
                assert(left == equal, name)
                assert(left.hashCode == equal.hashCode, name)
                assert(left != other, name)
                assert(Builtins.equalsData(left, equal), name)
                assert(!Builtins.equalsData(left, other), name)
            }
        }
    }

    test("data that straddles the depth at which the comparison changes strategy") {
        val wraps: Seq[Data => Data] = Seq(
          d => Data.List(PList.singleton(d)),
          d => Constr(0, PList.singleton(d)),
          d => Data.Map(PList.singleton((i(0), d))),
          d => Data.Map(PList.singleton((d, i(0))))
        )
        for wrap <- wraps; depth <- 120 to 140 do {
            val left = deep(depth, i(1))(wrap)
            assert(left == deep(depth, i(1))(wrap), s"depth $depth")
            assert(left.hashCode == deep(depth, i(1))(wrap).hashCode, s"depth $depth")
            assert(left != deep(depth, i(2))(wrap), s"depth $depth")
            assert(left != deep(depth + 1, i(1))(wrap), s"depth $depth")
        }
    }

    test("long lists of entries are compared on a small stack") {
        val entries = (0 until 200000).map(n => (i(n), i(n)))
        val left = Data.Map(PList.from(entries))
        val right = Data.Map(PList.from(entries.toList))
        val swapped = Data.Map(PList.from(entries.reverse))
        onSmallStack {
            assert(left == right)
            assert(left.hashCode == right.hashCode)
            assert(left != swapped)
        }
    }
}
