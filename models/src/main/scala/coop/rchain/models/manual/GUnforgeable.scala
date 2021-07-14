package coop.rchain.models.manual

import coop.rchain.models.manual.GUnforgeable.UnfInstance

final case class GUnforgeable(
    unfInstance: UnfInstance = UnfInstance.Empty
)

object GUnforgeable {
  sealed trait UnfInstance {
    def gPrivateBody: Option[GPrivate]           = None
    def gDeployIdBody: Option[GDeployId]         = None
    def gDeployerIdBody: Option[GDeployerId]     = None
    def gSysAuthTokenBody: Option[GSysAuthToken] = None
  }

  object UnfInstance {
    case object Empty                                        extends UnfInstance
    final case class GPrivateBody(value: GPrivate)           extends UnfInstance
    final case class GDeployIdBody(value: GDeployId)         extends UnfInstance
    final case class GDeployerIdBody(value: GDeployerId)     extends UnfInstance
    final case class GSysAuthTokenBody(value: GSysAuthToken) extends UnfInstance
  }
}
