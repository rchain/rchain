package coop.rchain.casper.util.rholang.costacc

import coop.rchain.casper.util.rholang.SystemDeployFailure.PreChargeFailed
import coop.rchain.casper.util.rholang.{SystemDeploy, SystemDeployFailure}
import coop.rchain.rholang.interpreter.RhoType._
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import com.google.protobuf.ByteString

object PreChargeDeploy extends SystemDeploy {
  import coop.rchain.models._
  import GUnforgeable.UnfInstance.GPrivateBody
  import Expr.ExprInstance._
  import rholang.{implicits => toPar}
  import shapeless._
  import syntax.singleton._
  import record._

  type Output = (RhoBoolean, RhoString)
  type Result = Unit

  val `sys:casper:chargeAmount` = Witness("sys:casper:chargeAmount")
  type `sys:casper:chargeAmount` = `sys:casper:chargeAmount`.T

  type Env =
    (`sys:casper:initialDeployerId` ->> GDeployerId) :: (`sys:casper:chargeAmount` ->> GInt) :: (`sys:casper:return` ->> GUnforgeable) :: HNil

  def createNormalizerEnv(chargeAmount: Long, deployerPk: PublicKey, rand: Blake2b512Random) =
    new NormalizerEnv(
      ("sys:casper:initialDeployerId" ->> GDeployerId(ByteString.copyFrom(deployerPk.bytes))) ::
        ("sys:casper:chargeAmount" ->> GInt(chargeAmount)) ::
        ("sys:casper:return" ->> GUnforgeable(
          GPrivateBody(GPrivate(ByteString.copyFrom(rand.next())))
        )) :: HNil
    )

  override val source: String =
    """
                                |new rl(`rho:registry:lookup`),
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
  protected def processResult(value: (Boolean, String)): Either[SystemDeployFailure, Unit] =
    value match {
      case (true, _)     => Right(())
      case (_, errorMsg) => Left(PreChargeFailed(errorMsg))
    }

  protected override def getReturnChannel(env: PreChargeDeploy.Env): Par =
    toPar(env.get("sys:casper:return"))
}
