package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.PublicKey
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.{NormalizerEnv, ParBuilder}
import monix.eval.Coeval

// TODO: Eliminate public key argument if unnecessary
final case class Validator(pk: PublicKey, stake: Long) extends CompiledRholangSource {

  val path: String = "<synthetic in Validator.scala>"

  val code: String =
    s"""
       | new rl(`rho:registry:lookup`), PoSCh in {
       |   rl!(`rho:rchain:pos`, *PoSCh)
       |   | for (@(_, PoS) <- PoSCh) {
       |     @PoS!("bond", $stake, Nil)
       |   }
       | }
     """.stripMargin

  val normalizerEnv: NormalizerEnv = NormalizerEnv.Empty

  lazy val term: Par = ParBuilder[Coeval].buildNormalizedTerm(code, normalizerEnv).value()

}
