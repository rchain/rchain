package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Par
import coop.rchain.rholang.build.{CompiledRholangSource, CompiledRholangTemplate}
import coop.rchain.rholang.interpreter.ParBuilder
import monix.eval.Coeval

import scala.io.Source

// TODO: Eliminate validators argument if unnecessary.
final case class ProofOfStake(minimumBond: Long, maximumBond: Long, validators: Seq[Validator])
    extends CompiledRholangTemplate(
      "PoS.rhox",
      "minimumBond" -> minimumBond,
      "maximumBond" -> maximumBond,
      "initialBonds" -> ProofOfStake.initialBonds(validators)
    ) {

  require(minimumBond <= maximumBond)

  require(validators.nonEmpty)

}

object ProofOfStake {

  // TODO: Determine how the "intial bonds" map can simulate transferring stake into the PoS contract
  //       when this must be done during genesis, under the authority of the genesisPk, which calls the
  //       linear receive in PoS.rho
  def initialBonds(validators : Seq[Validator]): String = {
    import coop.rchain.crypto.util.Sorting.publicKeyOrdering
    val sortedValidators = validators.sortBy(_.pk)
    val mapEntries = sortedValidators.iterator.zipWithIndex
      .map {
        case (Validator(pk, stake), index) =>
          val pkString = Base16.encode(pk.bytes)
          s""" "$pkString".hexToBytes() : ($stake, ${index + 1})"""
      }
      .mkString(", ")
    s"{$mapEntries}"
  }
}