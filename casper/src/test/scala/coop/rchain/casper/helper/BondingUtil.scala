package coop.rchain.casper.helper

import cats.Functor
import cats.syntax.functor._
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.shared.Time

object BondingUtil {
  def bondingDeploy[F[_]: Functor: Time](
      amount: Long,
      privateKey: PrivateKey,
      shardId: String
  ): F[Signed[DeployData]] =
    ConstructDeploy
      .sourceDeployNowF(
        s"""
         |new retCh, PoSCh, rl(`rho:registry:lookup`), stdout(`rho:io:stdout`), deployerId(`rho:rchain:deployerId`) in {
         |  rl!(`rho:rchain:pos`, *PoSCh) |
         |  for(@(_, PoS) <- PoSCh) {
         |    @PoS!("bond", *deployerId, 1000, *retCh)
         |  }
         |}
         |""".stripMargin,
        shardId = shardId
      )
}
