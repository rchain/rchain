package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16

//TODO: include other fields relevent to PoS (e.g. rewards channel)
final case class ProofOfStakeValidator(id: PublicKey, stake: Long)

final case class ProofOfStakeParams(
    minimumBond: Long,
    maximumBond: Long,
    validators: Seq[ProofOfStakeValidator]
) {
  require(minimumBond <= maximumBond)
  require(validators.nonEmpty)
}

object ProofOfStake {
  def initialBondsCode(validators: Seq[ProofOfStakeValidator]): String = {
    import coop.rchain.crypto.util.Sorting.publicKeyOrdering
    val sortedValidators = validators.sortBy(_.id)
    val mapEntries = sortedValidators.iterator.zipWithIndex
      .map {
        case (ProofOfStakeValidator(id, stake), index) =>
          val pk = Base16.encode(id.bytes)
          s""" "$pk".hexToBytes() : ($stake, "secp256k1Verify", Nil, ${index + 1})"""
      }
      .mkString(", ")

    s"{$mapEntries}"
  }
}
