package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.ParBuilder
import monix.eval.Coeval

// TODO: Eliminate validators argument if unnecessary.
final case class ProofOfStake(minimumBond: Long, maximumBond: Long, validators: Seq[Validator])
    extends CompiledRholangSource {

  require(minimumBond <= maximumBond)

  require(validators.nonEmpty)

  // TODO: Determine how the "intial bonds" map can simulate transferring stake into the PoS contract
  //       when this must be done during genesis, under the authority of the genesisPk, which calls the
  //       linear receive in PoS.rho
  val initialBondsCode: String = {
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

  val path: String = "<synthetic in ProofOfStake.scala>"

  val code: String =
    s"""
       | new rl(`rho:registry:lookup`), PoSCh in {
       |   rl!(`rho:id:cnec3pa8prp4out3yc8facon6grm3xbsotpd4ckjfx8ghuw77xadzt`, *PoSCh)
       |   | for (@(_, PoS) <- PoSCh) {
       |     @PoS!($minimumBond, $maximumBond)
       |   }
       | }
     """.stripMargin

  val term: Par = ParBuilder[Coeval].buildNormalizedTerm(code).value()

}
