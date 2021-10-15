package coop.rchain.casper.util.rholang.costacc

import coop.rchain.casper.util.rholang.{SystemDeploy, SystemDeployUserError}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.NormalizerEnv.{Contains, ToEnvMap}
import coop.rchain.rholang.interpreter.RhoType._
import coop.rchain.models.BlockHash._

// Currently we use parentHash as initial random seed
final case class CloseBlockDeploy(initialRand: Blake2b512Random) extends SystemDeploy(initialRand) {
  import coop.rchain.models._
  import rholang.{implicits => toPar}
  import shapeless._

  type Output = (RhoBoolean, Either[RhoString, RhoNil])
  type Result = Unit

  val `sys:casper:closeBlock` = Witness("sys:casper:closeBlock")
  type `sys:casper:closeBlock` = `sys:casper:closeBlock`.T

  import toPar._
  type Env =
    (`sys:casper:authToken` ->> GSysAuthToken) :: (`sys:casper:return` ->> GUnforgeable) :: HNil
  protected override val envsReturnChannel = Contains[Env, `sys:casper:return`]
  protected override val toEnvMap          = ToEnvMap[Env]

  protected val normalizerEnv: NormalizerEnv[Env] = new NormalizerEnv(
    mkSysAuthToken :: mkReturnChannel :: HNil
  )

  override val source: String =
    """#new rl(`rho:registry:lookup`),
      #  poSCh,
      #  sysAuthToken(`sys:casper:authToken`),
      #  return(`sys:casper:return`)
      #in {
      #  rl!(`rho:rchain:pos`, *poSCh) |
      #  for(@(_, PoS) <- poSCh) {
      #    @PoS!("closeBlock", *sysAuthToken, *return)
      #  }
      #}""".stripMargin('#')

  protected override val extractor = Extractor.derive

  protected override def processResult(
      value: (Boolean, Either[String, Unit])
  ): Either[SystemDeployUserError, Unit] = value match {
    case (true, _)               => Right(())
    case (false, Left(errorMsg)) => Left(SystemDeployUserError(errorMsg))
    case _                       => Left(SystemDeployUserError("<no cause>"))
  }
}
