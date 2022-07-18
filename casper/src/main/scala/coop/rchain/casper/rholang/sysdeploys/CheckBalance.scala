package coop.rchain.casper.rholang.sysdeploys

import cats.syntax.all._
import coop.rchain.casper.rholang.types.{SystemDeploy, SystemDeployUserError}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.NormalizerEnv.{Contains, ToEnvMap}
import coop.rchain.models.rholang.RhoType.{Extractor, RhoNumber}

class CheckBalance(pk: PublicKey, rand: Blake2b512Random) extends SystemDeploy(rand) {

  import coop.rchain.models._
  import rholang.{implicits => toPar}
  import shapeless._

  type Output = RhoNumber
  type Result = Long
  type Env =
    (`sys:casper:deployerId` ->> GDeployerId) :: (`sys:casper:return` ->> GUnforgeable) :: HNil

  import toPar._
  protected def toEnvMap                   = ToEnvMap[Env]
  implicit protected val envsReturnChannel = Contains[Env, `sys:casper:return`]
  protected val normalizerEnv              = new NormalizerEnv(mkDeployerId(pk) :: mkReturnChannel :: HNil)
  protected val extractor                  = Extractor.derive

  val source: String =
    """
      # new deployerId(`sys:casper:deployerId`),
      #     return(`sys:casper:return`),
      #     rl(`rho:registry:lookup`),
      #     revAddressOps(`rho:rev:address`),
      #     revAddressCh,
      #     revVaultCh in {
      #   rl!(`rho:rchain:revVault`, *revVaultCh) |
      #   revAddressOps!("fromDeployerId", *deployerId, *revAddressCh) |
      #   for(@userRevAddress <- revAddressCh & @(_, revVault) <- revVaultCh){
      #     new userVaultCh in {
      #       @revVault!("findOrCreate", userRevAddress, *userVaultCh) |
      #       for(@(true, userVault) <- userVaultCh){
      #         @userVault!("balance", *return)
      #       }
      #     }
      #   }
      # }
      #""".stripMargin('#')

  protected def processResult(value: Long): Either[SystemDeployUserError, Result] = value.asRight
}
