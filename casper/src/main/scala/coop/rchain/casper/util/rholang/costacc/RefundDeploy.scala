package coop.rchain.casper.util.rholang.costacc

import com.google.protobuf.ByteString
import coop.rchain.casper.util.rholang.{SystemDeploy, SystemDeployFailure, SystemDeployUserError}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rholang.interpreter.RhoType._
import coop.rchain.models.NormalizerEnv.ToEnvMap

final class RefundDeploy(refundAmount: Long, rand: Blake2b512Random) extends SystemDeploy(rand) {
  import coop.rchain.models._
  import Expr.ExprInstance._
  import GUnforgeable.UnfInstance.GPrivateBody
  import rholang.{implicits => toPar}
  import shapeless._
  import record._
  import syntax.singleton._

  type Output = (RhoBoolean, Either[RhoString, RhoNil])
  type Result = Unit

  val `sys:casper:refundAmount` = Witness("sys:casper:refundAmount")
  type `sys:casper:refundAmount` = `sys:casper:refundAmount`.T

  type Env = (`sys:casper:refundAmount` ->> GInt) :: (`sys:casper:return` ->> GUnforgeable) :: HNil

  import toPar._
  override protected val toEnvMap = ToEnvMap[Env]
  override protected val normalizerEnv =
    new NormalizerEnv(
      ("sys:casper:refundAmount" ->> GInt(refundAmount)) ::
        ("sys:casper:return" ->> GUnforgeable(
          GPrivateBody(GPrivate(ByteString.copyFrom(rand.next())))
        )) :: HNil
    )

  override val source: String =
    """|new rl(`rho:registry:lookup`),
       |poSCh,
       |refundAmount(`sys:casper:refundAmount`),
       |return(`sys:casper:return`) in
       |{
       |  rl!(`rho:rchain:pos`, *poSCh) |
       |  for(@(_, PoS) <- poSCh) {
       |    @PoS!("refundDeploy", *refundAmount, *return)
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
