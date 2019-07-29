package coop.rchain.rholang.interpreter

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.PublicKey

final case class NormalizerEnv(
    deployId: Option[Array[Byte]],
    deployerPk: Option[PublicKey]
)

object NormalizerEnv {
  def empty = new NormalizerEnv(deployId = None, deployerPk = None)

  def apply(deploy: DeployData): NormalizerEnv =
    new NormalizerEnv(
      deployId = Some(deploy.sig.toByteArray),
      deployerPk = Some(PublicKey(deploy.deployer.toByteArray))
    )
}
