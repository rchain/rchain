package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployDataProto
import coop.rchain.crypto.PublicKey
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.Injection.InjInstance._

object NormalizerEnv {
  type UriString     = String
  type NormalizerEnv = Map[UriString, Injection]

  val Empty: NormalizerEnv = Map[UriString, Injection]()

  def apply(deployId: Array[Byte]): NormalizerEnv =
    Map("rho:rchain:deployId" -> Injection(UnfBody(GDeployId(ByteString.copyFrom(deployId)))))

  def apply(deployerPk: PublicKey): NormalizerEnv =
    Map(
      "rho:rchain:deployerId" -> Injection(
        UnfBody(GDeployerId(ByteString.copyFrom(deployerPk.bytes)))
      )
    )

  def apply(deploy: DeployDataProto): NormalizerEnv =
    Map(
      "rho:rchain:deployId"   -> Injection(UnfBody(GDeployId(deploy.sig))),
      "rho:rchain:deployerId" -> Injection(UnfBody(GDeployerId(deploy.deployer)))
    )
}
