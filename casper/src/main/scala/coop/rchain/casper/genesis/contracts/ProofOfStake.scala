package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.codec.Base16
import coop.rchain.rholang.build.CompiledRholangTemplate
import coop.rchain.rholang.interpreter.NormalizerEnv

// TODO: Eliminate validators argument if unnecessary.
// TODO: eliminate the default for epochLength. Now it is used in order to minimise the impact of adding this parameter
final case class ProofOfStake(
    minimumBond: Long,
    maximumBond: Long,
    validators: Seq[Validator],
    epochLength: Int = 10000,
    quarantineLength: Int = 50000,
    numberOfActiveValidators: Int = 100
) extends CompiledRholangTemplate(
      "PoS.rhox",
      NormalizerEnv.Empty,
      "minimumBond"              -> minimumBond,
      "maximumBond"              -> maximumBond,
      "initialBonds"             -> ProofOfStake.initialBonds(validators),
      "epochLength"              -> epochLength,
      "quarantineLength"         -> quarantineLength,
      "numberOfActiveValidators" -> numberOfActiveValidators
    ) {

  require(minimumBond <= maximumBond)

  require(validators.nonEmpty)

}

object ProofOfStake {
  // TODO: Determine how the "intial bonds" map can simulate transferring stake into the PoS contract
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
