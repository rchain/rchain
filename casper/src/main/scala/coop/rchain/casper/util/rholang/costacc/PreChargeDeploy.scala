package coop.rchain.casper.util.rholang.costacc

import com.google.protobuf.ByteString
import coop.rchain.casper.util.rholang.{SystemDeploy, SystemDeployFailure, SystemDeployUserError}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rholang.interpreter.RhoType._
import coop.rchain.models.NormalizerEnv.ToEnvMap

final class PreChargeDeploy(chargeAmount: Long, pk: PublicKey, rand: Blake2b512Random)
    extends SystemDeploy(rand) {
  import coop.rchain.models._
  import Expr.ExprInstance._
  import GUnforgeable.UnfInstance.GPrivateBody
  import rholang.{implicits => toPar}
  import shapeless._
  import record._
  import syntax.singleton._

  type Output = (RhoBoolean, Either[RhoString, RhoNil])
  type Result = Unit

  val `sys:casper:chargeAmount`      = Witness("sys:casper:chargeAmount")
  val `sys:casper:initialDeployerId` = Witness("sys:casper:initialDeployerId")
  type `sys:casper:chargeAmount`      = `sys:casper:chargeAmount`.T
  type `sys:casper:initialDeployerId` = `sys:casper:initialDeployerId`.T

  type Env =
    (`sys:casper:initialDeployerId` ->> GDeployerId) ::
      (`sys:casper:chargeAmount` ->> GInt) ::
      (`sys:casper:return` ->> GUnforgeable) :: HNil

  import toPar._
  protected override val toEnvMap = ToEnvMap[Env]
  protected val normalizerEnv: NormalizerEnv[Env] =
    new NormalizerEnv(
      ("sys:casper:initialDeployerId" ->> GDeployerId(ByteString.copyFrom(pk.bytes))) ::
        ("sys:casper:chargeAmount" ->> GInt(chargeAmount)) ::
        ("sys:casper:return" ->> GUnforgeable(
          GPrivateBody(GPrivate(ByteString.copyFrom(rand.next())))
        )) :: HNil
    )

  override val source: String =
    """|new rl(`rho:registry:lookup`),
       |poSCh,
       |initialDeployerId(`sys:casper:initialDeployerId`),
       |chargeAmount(`sys:casper:chargeAmount`),
       |return(`sys:casper:return`) in
       |{
       |  rl!(`rho:rchain:pos`, *poSCh) |
       |  for(@(_, PoS) <- poSCh) {
       |    @PoS!("chargeDeploy", *initialDeployerId, *chargeAmount, *return)
       |  }
       |
       |}""".stripMargin

  protected override val extractor = Extractor.derive

  protected def processResult(
      value: (Boolean, Either[String, Unit])
  ): Either[SystemDeployFailure, Unit] =
    value match {
      case (true, _)               => Right(())
      case (false, Left(errorMsg)) => Left(SystemDeployUserError(errorMsg))
      case _                       => Left(SystemDeployUserError("<no cause>"))
    }

  protected override def getReturnChannel(env: Env): Par = toPar(env.get("sys:casper:return"))
}
