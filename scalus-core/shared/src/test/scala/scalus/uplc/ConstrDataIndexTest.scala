package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{platform, Builtins, ByteString, Data}
import scalus.uplc.eval.{BuiltinException, CekValue, NoLogger, PlutusVM}

/** The index of `constrData`: the reference takes any `Integer` in the semantics variants A, B and
  * C (protocol versions up to 10) and an unsigned 64-bit integer in D and E (protocol 11,
  * `ensurable`); see `toBuiltinMeaning semvar ConstrData` in plutus 1.70.0.0,
  * `PlutusCore/Default/Builtins.hs`.
  */
class ConstrDataIndexTest extends AnyFunSuite {
    private val costModel = PlutusVM.makePlutusV3VM().machineParams.builtinCostModel

    private val twoTo64 = BigInt(1) << 64

    private def constrData(variant: BuiltinSemanticsVariant, index: BigInt): CekValue =
        new CardanoBuiltins(costModel, platform, variant).ConstrData.f(
          NoLogger,
          Seq(
            CekValue.VCon(Constant.Integer(index)),
            CekValue.VCon(Constant.List(DefaultUni.Data, Nil))
          )
        )

    private val unrestricted =
        Seq(BuiltinSemanticsVariant.A, BuiltinSemanticsVariant.B, BuiltinSemanticsVariant.C)

    private val indices =
        Seq(
          BigInt(-1),
          BigInt(0),
          twoTo64 - 1,
          twoTo64,
          -twoTo64,
          -(BigInt(1) << 600),
          (BigInt(1) << 600) - 1
        )

    for variant <- unrestricted; index <- indices do
        test(s"constrData in variant $variant builds a constructor with index $index") {
            assert(
              constrData(variant, index) ==
                  CekValue.VCon(Constant.Data(Data.Constr(index, PList.Nil)))
            )
        }

    test("a constructor with a negative index is data, and serialiseData writes its index") {
        val constr = Data.Constr(-1, PList.Nil)
        assert(Builtins.unConstrData(constr).fst == BigInt(-1))
        assert(Builtins.serialiseData(constr) == ByteString.fromHex("d866822080"))
        assert(
          Builtins.serialiseData(Data.Constr(-twoTo64, PList.Nil)) ==
              ByteString.fromHex("d866823bffffffffffffffff80")
        )
        assert(Data.Constr(-1, PList.Nil) != Data.Constr(0, PList.Nil))
    }

    private val restricted = Seq(BuiltinSemanticsVariant.D, BuiltinSemanticsVariant.E)

    for variant <- restricted do {
        test(s"constrData in variant $variant rejects an index outside the Word64 range") {
            for index <- Seq(BigInt(-1), twoTo64, -twoTo64, -(BigInt(1) << 600), BigInt(1) << 600)
            do intercept[BuiltinException](constrData(variant, index))
        }

        test(s"constrData in variant $variant accepts the ends of the Word64 range") {
            for index <- Seq(BigInt(0), BigInt(1), BigInt(1) << 63, twoTo64 - 1) do
                assert(
                  constrData(variant, index) ==
                      CekValue.VCon(Constant.Data(Data.Constr(index, PList.Nil)))
                )
        }
    }
}
