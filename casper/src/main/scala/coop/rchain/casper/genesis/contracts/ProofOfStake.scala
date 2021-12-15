package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.PublicKey
import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangTemplate
import coop.rchain.models.rholang.implicits._
import coop.rchain.shared.Base16

// TODO: Eliminate validators argument if unnecessary.
// TODO: eliminate the default for epochLength. Now it is used in order to minimise the impact of adding this parameter
// TODO: Remove hardcoded keys from standard deploys: https://rchain.atlassian.net/browse/RCHAIN-3321?atlOrigin=eyJpIjoiNDc0NjE4YzYxOTRkNDcyYjljZDdlOWMxYjE1NWUxNjIiLCJwIjoiaiJ9
final case class ProofOfStake(
    minimumBond: Long,
    maximumBond: Long,
    validators: Seq[Validator],
    epochLength: Int,
    quarantineLength: Int,
    numberOfActiveValidators: Int,
    deployerPubKey: String,
    insertSignedSignature: String
) extends CompiledRholangTemplate(
      "PoS.rhox",
      NormalizerEnv.withDeployerId(
        PublicKey(
          Base16.unsafeDecode(
            "047b43d6548b72813b89ac1b9f9ca67624a8b372feedd71d4e2da036384a3e1236812227e524e6f237cde5f80dbb921cac12e6500791e9a9ed1254a745a816fe1f"
          )
        )
      ),
      "minimumBond"              -> minimumBond,
      "maximumBond"              -> maximumBond,
      "initialBonds"             -> ProofOfStake.initialBonds(validators),
      "epochLength"              -> epochLength,
      "quarantineLength"         -> quarantineLength,
      "numberOfActiveValidators" -> numberOfActiveValidators,
      "deployerPubKey"           -> deployerPubKey,
      "insertSignedSignature"    -> insertSignedSignature
    ) {

  require(minimumBond <= maximumBond)

  require(validators.nonEmpty)

}

object ProofOfStake {
  // TODO: Determine how the "initial bonds" map can simulate transferring stake into the PoS contract
  //       when this must be done during genesis, under the authority of the genesisPk, which calls the
  //       linear receive in PoS.rho
  def initialBonds(validators: Seq[Validator]): String = {
    import coop.rchain.crypto.util.Sorting.publicKeyOrdering
    val sortedValidators = validators.sortBy(_.pk)
    val mapEntries = sortedValidators.iterator
      .map { validator =>
        val pkString = Base16.encode(validator.pk.bytes)
        s""" "$pkString".hexToBytes() : ${validator.stake}"""
      }
      .mkString(", ")
    s"{$mapEntries}"
  }
}
