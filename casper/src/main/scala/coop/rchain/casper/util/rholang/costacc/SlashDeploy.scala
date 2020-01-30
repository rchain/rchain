package coop.rchain.casper.util.rholang.costacc

import coop.rchain.casper.util.rholang.{SystemDeploy, SystemDeployFailure, SystemDeployUserError}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.BlockHash._
import coop.rchain.models.NormalizerEnv.{Contains, ToEnvMap}
import coop.rchain.rholang.interpreter.RhoType._

final case class SlashDeploy(
    invalidBlockHash: BlockHash,
    pk: PublicKey,
    initialRand: Blake2b512Random
) extends SystemDeploy(initialRand) {
  import coop.rchain.models._
  import Expr.ExprInstance._
  import rholang.{implicits => toPar}
  import shapeless._
  import record._
  import syntax.singleton._

  type Output = RhoBoolean
  type Result = Unit

  val `sys:casper:invalidBlockHash` = Witness("sys:casper:invalidBlockHash")
  type `sys:casper:invalidBlockHash` = `sys:casper:invalidBlockHash`.T

  type Env =
    (`sys:casper:deployerId` ->> GDeployerId) :: (`sys:casper:invalidBlockHash` ->> GString) :: (`sys:casper:authToken` ->> GSysAuthToken) :: (`sys:casper:return` ->> GUnforgeable) :: HNil

  import toPar._
  protected override val envsReturnChannel = Contains[Env, `sys:casper:return`]
  protected override val toEnvMap          = ToEnvMap[Env]
  protected override val normalizerEnv = new NormalizerEnv(
    mkDeployerId(pk) :: ("sys:casper:invalidBlockHash" ->> GString(invalidBlockHash.base16String)) :: mkSysAuthToken :: mkReturnChannel :: HNil
  )

  override val source: String =
    """#new rl(`rho:registry:lookup`),
       #  poSCh,
       #  deployerId(`sys:casper:deployerId`),
       #  invalidBlockHash(`sys:casper:invalidBlockHash`),
       #  sysAuthToken(`sys:casper:authToken`),
       #  return(`sys:casper:return`)
       #in {
       #  rl!(`rho:rchain:pos`, *poSCh) |
       #  for(@(_, PoS) <- poSCh) {
       #    @PoS!("slash",  *deployerId, *invalidBlockHash.hexToBytes(), *sysAuthToken, *return)
       #  }
       #}""".stripMargin('#')

  protected override val extractor = Extractor.derive

  protected def processResult(value: Boolean): Either[SystemDeployFailure, Unit] =
    Either.cond(value, (), SystemDeployUserError("Slashing failed unexpectedly"))

}
