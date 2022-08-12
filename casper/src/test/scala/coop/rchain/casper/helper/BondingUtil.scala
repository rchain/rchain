package coop.rchain.casper.helper

import cats.Functor
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.shared.Time

object BondingUtil {
  def bondingDeploy[F[_]: Functor: Time](
      amount: Long,
      privateKey: PrivateKey,
      shardId: String = ""
  ): F[Signed[DeployData]] =
    ConstructDeploy
      .sourceDeployNowF(
        s"""
         |new retCh, PosCh, rl(`rho:registry:lookup`), deployerId(`rho:rchain:deployerId`) in {
         |  rl!(`rho:rchain:pos`, *PosCh) |
         |  for(@(_, Pos) <- PosCh) {
         |    @Pos!("bond", *deployerId, $amount, *retCh)
         |  }
         |}
         |""".stripMargin,
        sec = privateKey,
        shardId = shardId
      )

  def addValidatorInBondingWhiteList[F[_]: Functor: Time](
      amount: Long,
      privateKey: PrivateKey,
      newValidatorPubkey: String,
      shardId: String
  ): F[Signed[DeployData]] =
    ConstructDeploy
      .sourceDeployNowF(
        s"""
           |new retCh, PosCh, rl(`rho:registry:lookup`), deployerId(`rho:rchain:deployerId`) in {
           |  rl!(`rho:rchain:pos`, *PosCh) |
           |  for(@(_, Pos) <- PosCh) {
           |    @Pos!("addBondingRequest", *deployerId, "$newValidatorPubkey", $amount, *retCh)
           |  }
           |}
           |""".stripMargin,
        sec = privateKey,
        shardId = shardId
      )
}
