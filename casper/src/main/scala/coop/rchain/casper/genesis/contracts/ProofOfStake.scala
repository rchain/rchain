package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.util.Sorting
import coop.rchain.crypto.codec.Base16

//TODO: include other fields relevent to PoS (e.g. rewards channel)
case class ProofOfStakeValidator(id: Array[Byte], stake: Long)

case class ProofOfStakeParams(
    minimumBond: Long,
    maximumBond: Long,
    validators: Seq[ProofOfStakeValidator]
) {
  require(minimumBond <= maximumBond)
  require(validators.nonEmpty)
}

object ProofOfStake {
  def initialBondsCode(validators: Seq[ProofOfStakeValidator]): String = {
    import Sorting.byteArrayOrdering
    val sortedValidators = validators.sortBy(_.id)
    val mapEntries = sortedValidators.iterator.zipWithIndex
      .map {
        case (ProofOfStakeValidator(id, stake), index) =>
          val pk = Base16.encode(id)
          s""" "$pk".hexToBytes() : ($stake, "secp256k1Verify", Nil, ${index + 1})"""
      }
      .mkString(", ")

    s"{$mapEntries}"
  }
}
