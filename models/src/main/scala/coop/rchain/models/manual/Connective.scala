package coop.rchain.models.manual

import Connective._

final case class Connective(
    connectiveInstance: ConnectiveInstance = ConnectiveInstance.Empty
)

object Connective {
  sealed trait ConnectiveInstance {
    def connAndBody: Option[ConnectiveBody] = None
    def connOrBody: Option[ConnectiveBody]  = None
    def connNotBody: Option[Par]            = None
    def varRefBody: Option[VarRef]          = None
    def connBool: Option[Boolean]           = None
    def connInt: Option[Boolean]            = None
    def connString: Option[Boolean]         = None
    def connUri: Option[Boolean]            = None
    def connByteArray: Option[Boolean]      = None
  }
  object ConnectiveInstance {
    case object Empty                                   extends ConnectiveInstance
    final case class ConnAndBody(value: ConnectiveBody) extends ConnectiveInstance
    final case class ConnOrBody(value: ConnectiveBody)  extends ConnectiveInstance
    final case class ConnNotBody(value: Par)            extends ConnectiveInstance
    final case class VarRefBody(value: VarRef)          extends ConnectiveInstance
    final case class ConnBool(value: Boolean)           extends ConnectiveInstance
    final case class ConnInt(value: Boolean)            extends ConnectiveInstance
    final case class ConnString(value: Boolean)         extends ConnectiveInstance
    final case class ConnUri(value: Boolean)            extends ConnectiveInstance
    final case class ConnByteArray(value: Boolean)      extends ConnectiveInstance
  }
}
