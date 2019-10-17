package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployDataProto
import coop.rchain.crypto.PublicKey
import coop.rchain.models.rholang.implicits._

object NormalizerEnv {
  type UriString     = String
  type NormalizerEnv = Map[UriString, Par]

  val Empty: NormalizerEnv = Map[UriString, Par]()

  def apply(deployId: Array[Byte]): NormalizerEnv =
    Map("rho:rchain:deployId" -> GDeployId(ByteString.copyFrom(deployId)))

  def apply(deployerPk: PublicKey): NormalizerEnv =
    Map("rho:rchain:deployerId" -> GDeployerId(ByteString.copyFrom(deployerPk.bytes)))

  def apply(deploy: DeployDataProto): NormalizerEnv =
    Map(
      "rho:rchain:deployId"   -> GDeployId(deploy.sig),
      "rho:rchain:deployerId" -> GDeployerId(deploy.deployer)
    )
}
