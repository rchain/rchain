package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16

final case class ProofOfStake(
    minimumBond: Long,
    maximumBond: Long,
    validators: Seq[Validator]
) {
  require(minimumBond <= maximumBond)
  require(validators.nonEmpty)
}

object ProofOfStake {
  def initialBondsCode(validators: Seq[Validator]): String = {
    import coop.rchain.crypto.util.Sorting.publicKeyOrdering
    val sortedValidators = validators.sortBy(_.pk)
    val mapEntries = sortedValidators.iterator.zipWithIndex
      .map {
        case (Validator(pk, stake), index) =>
          val pkString = Base16.encode(pk.bytes)
          s""" "$pkString".hexToBytes() : ($stake, "secp256k1Verify", Nil, ${index + 1})"""
      }
      .mkString(", ")

    s"{$mapEntries}"
  }
}
