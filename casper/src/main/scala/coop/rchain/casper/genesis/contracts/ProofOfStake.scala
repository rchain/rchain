package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.util.Sorting
import coop.rchain.crypto.codec.Base16

//TODO: include other fields relevent to PoS (e.g. rewards channel)
case class ProofOfStakeValidator(id: Array[Byte], stake: Long)

object ProofOfStake {
  def initialBondsCode(validators: Seq[ProofOfStakeValidator]): String = {
    import Sorting.byteArrayOrdering
    val sortedValidators = validators.sortBy(_.id)
    val mapEntries = sortedValidators
      .map {
        case ProofOfStakeValidator(id, stake) =>
          val pk = Base16.encode(id)
          s""" "$pk".hexToBytes() : ($stake, "secp256k1Verify", Nil)"""
      }
      .mkString(", ")

    s"{$mapEntries}"
  }
}
