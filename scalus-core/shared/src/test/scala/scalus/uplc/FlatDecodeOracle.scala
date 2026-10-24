package scalus.uplc

import scalus.cardano.ledger.{Language, MajorProtocolVersion, PlutusScript}
import scalus.uplc.builtin.{Builtins, ByteString}

/** Verdicts of the Plutus reference on serialised scripts, and the means to hold Scalus to them.
  *
  * A row is the verdict of `deserialiseScript` of plutus-ledger-api 1.70.0.0 (Plutus commit
  * 507cab8cf6192e5143f12be5b2d3005a2928dc6d), checked by `evaluateScript` on the same script, for a
  * script identified by the SHA-256 of its bytes. `admitted` is whether the script deserialised,
  * for each Plutus language (1 to 3) and major protocol version of the form `language/protocol`.
  * The scripts themselves are rebuilt by the tests and compared with the hash, so that a row can
  * only be applied to the bytes the reference saw.
  *
  * Scalus's counterpart of `deserialiseScript` is [[PlutusScript.isWellFormed]].
  */
object FlatDecodeOracle {

    /** @param admitted
      *   the `language/protocol` pairs at which the reference deserialised the script
      */
    case class Row(id: String, sha256: String, admitted: String, refused: String) {
        private def cells(s: String): Seq[(Int, Int)] =
            s.split(' ').filter(_.nonEmpty).toSeq.map { cell =>
                val Array(language, protocol) = cell.split('/')
                (language.toInt, protocol.toInt)
            }

        def verdicts: Seq[((Int, Int), Boolean)] =
            cells(admitted).map(_ -> true) ++ cells(refused).map(_ -> false)
    }

    def sha256Hex(bytes: Array[Byte]): String =
        Builtins.sha2_256(ByteString.unsafeFromArray(bytes)).toHex

    private def language(id: Int): Language = id match {
        case 1 => Language.PlutusV1
        case 2 => Language.PlutusV2
        case 3 => Language.PlutusV3
    }

    def scalusAdmits(script: Array[Byte], languageId: Int, protocol: Int): Boolean =
        PlutusScript.isWellFormed(
          ByteString.unsafeFromArray(script),
          language(languageId),
          MajorProtocolVersion(protocol)
        )

    /** The rows whose script differs from the reference's, and the cells at which Scalus's verdict
      * differs from the reference's, as text for an assertion.
      */
    def disagreements(rows: Seq[Row], scripts: Map[String, Array[Byte]]): Seq[String] =
        rows.flatMap { row =>
            val script = scripts(row.id)
            if sha256Hex(script) != row.sha256 then
                Seq(s"${row.id}: script bytes differ from the oracle's")
            else
                row.verdicts.collect {
                    case ((languageId, protocol), expected)
                        if scalusAdmits(script, languageId, protocol) != expected =>
                        s"${row.id} at $languageId/$protocol: reference admitted=$expected"
                }
        }
}
