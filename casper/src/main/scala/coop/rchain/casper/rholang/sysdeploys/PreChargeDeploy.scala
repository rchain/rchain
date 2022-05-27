package coop.rchain.casper.rholang.sysdeploys

import coop.rchain.casper.rholang.types.{SystemDeploy, SystemDeployUserError}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.NormalizerEnv.{Contains, ToEnvMap}
import coop.rchain.rholang.interpreter.RhoType._

final class PreChargeDeploy(chargeAmount: Long, pk: PublicKey, rand: Blake2b512Random)
    extends SystemDeploy(rand) {
  import coop.rchain.models._
  import Expr.ExprInstance._
  import rholang.{implicits => toPar}
  import shapeless._
  import shapeless.syntax.singleton._

  type Output = (RhoBoolean, Either[RhoString, RhoNil])
  type Result = Unit

  val `sys:casper:chargeAmount` = Witness("sys:casper:chargeAmount")
  type `sys:casper:chargeAmount` = `sys:casper:chargeAmount`.T

  type Env =
    (`sys:casper:deployerId` ->> GDeployerId) :: (`sys:casper:chargeAmount` ->> GInt) :: (`sys:casper:authToken` ->> GSysAuthToken) :: (`sys:casper:return` ->> GUnforgeable) :: HNil

  import toPar._
  protected override val envsReturnChannel = Contains[Env, `sys:casper:return`]
  protected override val toEnvMap          = ToEnvMap[Env]
  protected val normalizerEnv: NormalizerEnv[Env] = new NormalizerEnv(
    mkDeployerId(pk) :: ("sys:casper:chargeAmount" ->> GInt(chargeAmount)) :: mkSysAuthToken :: mkReturnChannel :: HNil
  )

  override val source: String =
    """#new rl(`rho:registry:lookup`),
       #  poSCh,
       #  initialDeployerId(`sys:casper:deployerId`),
       #  chargeAmount(`sys:casper:chargeAmount`),
       #  sysAuthToken(`sys:casper:authToken`),
       #  return(`sys:casper:return`)
       #in {
       #  rl!(`rho:rchain:pos`, *poSCh) |
       #  for(@(_, Pos) <- poSCh) {
       #    @Pos!("chargeDeploy", *initialDeployerId, *chargeAmount, *sysAuthToken, *return)
       #  }
       #}""".stripMargin('#')

  protected override val extractor = Extractor.derive

  protected def processResult(
      value: (Boolean, Either[String, Unit])
  ): Either[SystemDeployUserError, Unit] =
    value match {
      case (true, _)               => Right(())
      case (false, Left(errorMsg)) => Left(SystemDeployUserError(errorMsg))
      case _                       => Left(SystemDeployUserError("<no cause>"))
    }

}
