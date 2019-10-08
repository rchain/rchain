package coop.rchain.rholang.interpreter

import coop.rchain.casper.protocol.DeployDataProto
import coop.rchain.crypto.PublicKey

final case class NormalizerEnv(
    deployId: Option[Array[Byte]],
    deployerPk: Option[PublicKey]
)

object NormalizerEnv {
  val Empty = new NormalizerEnv(deployId = None, deployerPk = None)

  def apply(deploy: DeployDataProto): NormalizerEnv =
    new NormalizerEnv(
      deployId = Some(deploy.sig.toByteArray),
      deployerPk = Some(PublicKey(deploy.deployer.toByteArray))
    )
}
