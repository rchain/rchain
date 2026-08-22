package coop.rchain.casper.rholang.sysdeploys

import coop.rchain.casper.rholang.types.{SystemDeploy, SystemDeployUserError}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.NormalizerEnv.{Contains, ToEnvMap}
import coop.rchain.models.Validator.Validator
import coop.rchain.models.rholang.RhoType._

final case class SlashDeploy(
    slashedValidator: Validator,
    initialRand: Blake2b512Random
) extends SystemDeploy(initialRand) {
  import coop.rchain.models._
  import Expr.ExprInstance._
  import rholang.{implicits => toPar}
  import shapeless._
  import shapeless.syntax.singleton._

  type Output = (RhoBoolean, Either[RhoString, RhoNil])
  type Result = Unit

  val `sys:casper:slashedValidator` = Witness("sys:casper:slashedValidator")
  type `sys:casper:slashedValidator` = `sys:casper:slashedValidator`.T

  type Env =
    (`sys:casper:slashedValidator` ->> GByteArray) :: (`sys:casper:authToken` ->> GSysAuthToken) :: (`sys:casper:return` ->> GUnforgeable) :: HNil

  import toPar._
  protected override val envsReturnChannel = Contains[Env, `sys:casper:return`]
  protected override val toEnvMap          = ToEnvMap[Env]
  protected override val normalizerEnv = new NormalizerEnv(
    ("sys:casper:slashedValidator" ->> GByteArray(slashedValidator))
      :: mkSysAuthToken
      :: mkReturnChannel
      :: HNil
  )

  override val source: String =
    """#new rl(`rho:registry:lookup`),
       #  poSCh,
       #  slashedValidator(`sys:casper:slashedValidator`),
       #  sysAuthToken(`sys:casper:authToken`),
       #  return(`sys:casper:return`)
       #in {
       #  rl!(`rho:rchain:pos`, *poSCh) |
       #  for(@(_, Pos) <- poSCh) {
       #    @Pos!("slash",  *slashedValidator, *sysAuthToken, *return)
       #  }
       #}""".stripMargin('#')

  protected override val extractor = Extractor.derive

  protected def processResult(
      value: (Boolean, Either[String, Unit])
  ): Either[SystemDeployUserError, Unit] =
    value match {
      case (true, _)               => Right(())
      case (false, Left(errorMsg)) => Left(SystemDeployUserError(errorMsg))
      case _                       => Left(SystemDeployUserError("Slashing failed unexpectedly"))
    }

}
