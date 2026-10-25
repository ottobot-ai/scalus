package scalus.uplc.builtin

import scalus.uplc.builtin.Data.{B, Constr, I, List, Map}
import scalus.cardano.onchain.plutus.prelude.List as PList

import scala.annotation.tailrec
import scala.collection.mutable.ArrayDeque
import scala.compiletime.asMatchable
import scala.util.hashing.MurmurHash3

/** The universal data type for Plutus smart contracts.
  *
  * `Data` is the core serializable type used throughout Cardano for representing:
  *   - Script datums attached to UTxOs
  *   - Redeemers passed to validators
  *   - Arbitrary structured data in smart contracts
  *
  * It corresponds to the `BuiltinData` type in Plutus Core and can represent any algebraic data
  * type through its five constructors:
  *
  *   - [[Data.Constr]] - Constructor application with tag and arguments (for sum/product types)
  *   - [[Data.Map]] - Key-value mappings
  *   - [[Data.List]] - Ordered sequences
  *   - [[Data.I]] - Arbitrary-precision integers
  *   - [[Data.B]] - Byte strings
  *
  * ==Encoding Convention==
  *
  * Scalus uses the standard Plutus Data encoding:
  *   - Sum types use `Constr` with constructor index as tag
  *   - Product types (case classes) use `Constr` with fields as arguments
  *   - `Option[A]` encodes as `Constr(0, [])` for `None`, `Constr(1, [value])` for `Some`
  *   - `Boolean` encodes as `Constr(0, [])` for `false`, `Constr(1, [])` for `true`
  *
  * ==UPLC Builtin Operations==
  *
  * The following UPLC builtins operate on Data:
  *   - `chooseData` - Pattern match on Data constructor
  *   - `constrData`, `mapData`, `listData`, `iData`, `bData` - Construct Data values
  *   - `unConstrData`, `unMapData`, `unListData`, `unIData`, `unBData` - Deconstruct Data values
  *   - `equalsData` - Structural equality comparison, in which the entries of a [[Data.Map]] are an
  *     ordered list: reordered or repeated entries make two maps different
  *   - `serialiseData` - CBOR serialization (Plutus V2+)
  *
  * @example
  *   {{{
  *   // Encoding a simple value
  *   val intData: Data = Data.I(42)
  *
  *   // Encoding a product type (pair)
  *   val pairData: Data = Data.Constr(0, List(Data.I(1), Data.I(2)))
  *
  *   // Encoding a sum type (Option)
  *   val someData: Data = Data.Constr(1, List(Data.I(42)))  // Some(42)
  *   val noneData: Data = Data.Constr(0, List.Nil)          // None
  *   }}}
  *
  * @see
  *   [[scalus.uplc.builtin.ToData]] for automatic encoding to Data
  * @see
  *   [[scalus.uplc.builtin.FromData]] for automatic decoding from Data
  * @see
  *   [[https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf Plutus Core Specification]]
  */
sealed abstract class Data {

    /** Structural equality as the reference defines it (the derived `Eq` of `PlutusCore.Data`): the
      * entries of a [[Data.Map]] are an ordered list, so their order and repetitions count.
      *
      * Data nested deeper than a fixed bound is compared with a work list on the heap, so data of
      * any depth can be compared.
      */
    override def equals(other: Any): Boolean = other.asMatchable match
        case that: Data => Data.structurallyEqual(this, that)
        case _          => false

    /** A hash that agrees with [[equals]]; it is computed without recursion as well. */
    override def hashCode(): Int = Data.structuralHash(this)

    /** Debug string representation matching the Pretty instance format */
    override def toString: String = this match {
        case Constr(constr, args) =>
            s"<$constr, [${args.toScalaList.map(_.toString).mkString(", ")}]>"
        case Map(values) =>
            s"{${values.toScalaList.map { case (k, v) => s"$k: $v" }.mkString(", ")}}"
        case List(values) =>
            s"[${values.toScalaList.map(_.toString).mkString(", ")}]"
        case I(value) =>
            s"$value"
        case B(value) =>
            s"\"${value.toHex}\""
    }
}

/** Companion object for [[Data]] providing constructors and type class instances.
  *
  * Use the [[ToData]] and [[FromData]] type classes for automatic conversion between Scala types
  * and Data.
  */
object Data extends DataApi:

    /** Type class for converting values to Data representation. */
    type ToData[A] = scalus.uplc.builtin.ToData[A]

    extension [A: ToData](a: A)
        /** Converts this value to its Data representation using the ToData type class. */
        inline def toData: Data = summon[ToData[A]](a)

    extension (inline data: Data)
        /** Converts this Data to type A using the FromData type class.
          * @throws Exception
          *   if the Data structure doesn't match the expected type
          */
        inline def to[A](using inline ev: FromData[A]): A = ev(data)

    /** Type class for converting Data back to typed values. */
    type FromData[A] = scalus.uplc.builtin.FromData[A]

    /** Converts Data to type A using the FromData type class.
      *
      * @param data
      *   the Data to convert
      * @return
      *   the converted value
      * @throws Exception
      *   if the Data structure doesn't match the expected type
      */
    inline def fromData[A](inline data: Data)(using inline ev: scalus.uplc.builtin.FromData[A]): A =
        ev(
          data
        )

    /** Constructor application - represents sum types and product types.
      *
      * This is the primary way to encode algebraic data types in Plutus:
      *   - For sum types, `constr` is the constructor index (0, 1, 2, ...)
      *   - For product types (case classes), `constr` is typically 0
      *   - `args` contains the constructor's field values
      *
      * Corresponds to UPLC builtins `constrData` and `unConstrData`.
      *
      * @param constr
      *   the constructor index (must be non-negative)
      * @param args
      *   the constructor arguments as a list of Data values
      *
      * @example
      *   {{{
      *   // A pair (Int, String)
      *   Constr(0, List(I(42), B(ByteString.fromString("hello"))))
      *
      *   // Either.Left(10)
      *   Constr(0, List(I(10)))
      *
      *   // Either.Right("x")
      *   Constr(1, List(B(ByteString.fromString("x"))))
      *   }}}
      */
    case class Constr(constr: BigInt, args: PList[Data]) extends Data {
        assert(constr >= 0, s"Constructor must be non-negative, got $constr")
    }

    /** Key-value mapping - represents associative data structures.
      *
      * Used to encode `Map[K, V]` types where both keys and values are Data. Note that duplicate
      * keys are allowed (this is a list of pairs, not a true map). Equality compares the pairs in
      * order, as Plutus does: maps with the same entries in a different order are not equal.
      *
      * Corresponds to UPLC builtins `mapData` and `unMapData`.
      *
      * @param values
      *   list of key-value pairs
      *
      * @example
      *   {{{
      *   // A map from integers to strings
      *   Map(List((I(1), B(hex"aa")), (I(2), B(hex"bb"))))
      *   }}}
      */
    case class Map(values: PList[(Data, Data)]) extends Data

    /** Ordered sequence - represents list types.
      *
      * Used to encode `List[A]` types where elements are Data.
      *
      * Corresponds to UPLC builtins `listData` and `unListData`.
      *
      * @param values
      *   the list elements
      *
      * @example
      *   {{{
      *   // A list of integers [1, 2, 3]
      *   List(PList(I(1), I(2), I(3)))
      *   }}}
      */
    case class List(values: PList[Data]) extends Data

    /** Arbitrary-precision integer.
      *
      * Corresponds to UPLC builtins `iData` and `unIData`.
      *
      * @param value
      *   the integer value
      *
      * @example
      *   {{{
      *   I(42)
      *   I(-1000000000000000000L)
      *   }}}
      */
    case class I(value: BigInt) extends Data

    /** Byte string - represents binary data.
      *
      * Used to encode `ByteString`, `PubKeyHash`, `CurrencySymbol`, `TokenName`, etc.
      *
      * Corresponds to UPLC builtins `bData` and `unBData`.
      *
      * @param value
      *   the byte string
      *
      * @example
      *   {{{
      *   B(ByteString.fromHex("deadbeef"))
      *   B(ByteString.empty)
      *   }}}
      */
    case class B(value: ByteString) extends Data

    /** The unit value encoded as Data.
      *
      * Equivalent to `Constr(0, Nil)` - an empty constructor with tag 0.
      */
    val unit: Data = Constr(0, PList.Nil)

    /** The deepest nesting that [[structurallyEqual]] follows by recursion. */
    private inline val MaxRecursiveDepth = 128

    /** Compares two values. Shallow data is compared by recursion, which is fast; data nested
      * deeper than [[MaxRecursiveDepth]] levels is handed to [[iterativelyEqual]], so the stack use
      * is bounded whatever the depth of the data. Lists and entries are walked in a loop.
      */
    private[builtin] def structurallyEqual(left: Data, right: Data): Boolean =
        boundedEqual(left, right, 0)

    private def boundedEqual(a: Data, b: Data, depth: Int): Boolean =
        if a eq b then true
        else if depth >= MaxRecursiveDepth then iterativelyEqual(a, b)
        else
            (a, b) match
                case (Constr(i, xs), Constr(j, ys)) => i == j && elementsEqual(xs, ys, depth + 1)
                case (Map(xs), Map(ys))             => entriesEqual(xs, ys, depth + 1)
                case (List(xs), List(ys))           => elementsEqual(xs, ys, depth + 1)
                case (I(x), I(y))                   => x == y
                case (B(x), B(y))                   => x == y
                case _                              => false

    @tailrec private def elementsEqual(xs: PList[Data], ys: PList[Data], depth: Int): Boolean =
        (xs, ys) match
            case (PList.Cons(x, xt), PList.Cons(y, yt)) =>
                boundedEqual(x, y, depth) && elementsEqual(xt, yt, depth)
            case (PList.Nil, PList.Nil) => true
            case _                      => false

    @tailrec private def entriesEqual(
        xs: PList[(Data, Data)],
        ys: PList[(Data, Data)],
        depth: Int
    ): Boolean = (xs, ys) match
        case (PList.Cons((xk, xv), xt), PList.Cons((yk, yv), yt)) =>
            boundedEqual(xk, yk, depth) && boundedEqual(xv, yv, depth) &&
            entriesEqual(xt, yt, depth)
        case (PList.Nil, PList.Nil) => true
        case _                      => false

    /** Compares two values with a work list on the heap. A pair of lists of different lengths, or
      * of different constructors, ends the comparison with `false` at once.
      */
    private def iterativelyEqual(left: Data, right: Data): Boolean =
        // pairs to compare, a left element below its right element
        val pending = ArrayDeque[Data](left, right)

        @tailrec def pushPairs(xs: PList[Data], ys: PList[Data]): Boolean = (xs, ys) match
            case (PList.Cons(x, xt), PList.Cons(y, yt)) =>
                pending.append(x)
                pending.append(y)
                pushPairs(xt, yt)
            case (PList.Nil, PList.Nil) => true
            case _                      => false

        @tailrec def pushEntries(xs: PList[(Data, Data)], ys: PList[(Data, Data)]): Boolean =
            (xs, ys) match
                case (PList.Cons((xk, xv), xt), PList.Cons((yk, yv), yt)) =>
                    pending.append(xk)
                    pending.append(yk)
                    pending.append(xv)
                    pending.append(yv)
                    pushEntries(xt, yt)
                case (PList.Nil, PList.Nil) => true
                case _                      => false

        var equal = true
        while equal && pending.nonEmpty do
            val b = pending.removeLast()
            val a = pending.removeLast()
            equal = (a eq b) || ((a, b) match
                case (Constr(i, xs), Constr(j, ys)) => i == j && pushPairs(xs, ys)
                case (Map(xs), Map(ys))             => pushEntries(xs, ys)
                case (List(xs), List(ys))           => pushPairs(xs, ys)
                case (I(x), I(y))                   => x == y
                case (B(x), B(y))                   => x == y
                case _                              => false)
        equal

    /** Hashes the constructors, the sizes and the leaves in a fixed walk of the value, so two
      * values that [[structurallyEqual]] hashes alike.
      */
    private[builtin] def structuralHash(root: Data): Int =
        val pending = ArrayDeque[Data](root)
        var hash = MurmurHash3.productSeed
        var count = 0

        def mix(token: Int): Unit =
            hash = MurmurHash3.mix(hash, token)
            count += 1

        @tailrec def pushAll(xs: PList[Data]): Unit = xs match
            case PList.Cons(x, xt) =>
                pending.append(x)
                pushAll(xt)
            case PList.Nil => ()

        @tailrec def pushEntries(xs: PList[(Data, Data)]): Unit = xs match
            case PList.Cons((k, v), xt) =>
                pending.append(k)
                pending.append(v)
                pushEntries(xt)
            case PList.Nil => ()

        @tailrec def length(xs: PList[Any], acc: Int): Int = xs match
            case PList.Cons(_, xt) => length(xt, acc + 1)
            case PList.Nil         => acc

        while pending.nonEmpty do
            pending.removeLast() match
                case Constr(i, xs) =>
                    mix(1)
                    mix(i.hashCode)
                    mix(length(xs, 0))
                    pushAll(xs)
                case Map(xs) =>
                    mix(2)
                    mix(length(xs, 0))
                    pushEntries(xs)
                case List(xs) =>
                    mix(3)
                    mix(length(xs, 0))
                    pushAll(xs)
                case I(x) =>
                    mix(4)
                    mix(x.hashCode)
                case B(x) =>
                    mix(5)
                    mix(x.hashCode)
        MurmurHash3.finalizeHash(hash, count)
