package coop.rchain.casper.util.comm

import coop.rchain.models.GUnforgeable.UnfInstance.{GDeployIdBody, GDeployerIdBody, GPrivateBody}
import coop.rchain.models.syntax.modelsSyntaxString
import coop.rchain.models.{GDeployId, GDeployerId, GPrivate, GUnforgeable, Par}

object ListenAtPar {
  sealed trait Name
  final case class UnforgPrivate(data: String)  extends Name
  final case class UnforgDeploy(data: String)   extends Name
  final case class UnforgDeployer(data: String) extends Name

  def create(name: String, data: String): Option[Name] =
    name match {
      case "UnforgPrivate"  => Some(UnforgPrivate(data))
      case "UnforgDeploy"   => Some(UnforgDeploy(data))
      case "UnforgDeployer" => Some(UnforgDeployer(data))
      case _                => None
    }

  def toPar(name: Name): Par = Par(unforgeables = Seq(GUnforgeable(unforgToUnforgProto(name))))

  private def unforgToUnforgProto(name: Name): GUnforgeable.UnfInstance = name match {
    case UnforgPrivate(data)  => GPrivateBody(GPrivate(data.unsafeHexToByteString))
    case UnforgDeploy(data)   => GDeployIdBody(GDeployId(data.unsafeHexToByteString))
    case UnforgDeployer(data) => GDeployerIdBody(GDeployerId(data.unsafeHexToByteString))
  }
}
