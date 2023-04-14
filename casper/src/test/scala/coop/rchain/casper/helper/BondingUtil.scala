package coop.rchain.casper.helper

import cats.Functor
import cats.effect.kernel.Clock
import cats.syntax.functor._
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Signed

object BondingUtil {
  def bondingDeploy[F[_]: Functor: Clock](
      amount: Long,
      privateKey: PrivateKey,
      shardId: String = ""
  ): F[Signed[DeployData]] =
    ConstructDeploy
      .sourceDeployNowF(
        s"""
         |new retCh, PosCh, rl(`rho:registry:lookup`), stdout(`rho:io:stdout`), deployerId(`rho:rchain:deployerId`) in {
         |  rl!(`rho:rchain:pos`, *PosCh) |
         |  for(@(_, Pos) <- PosCh) {
         |    @Pos!("bond", *deployerId, $amount, *retCh)
         |  }
         |}
         |""".stripMargin,
        shardId = shardId,
        sec = privateKey
      )
}
