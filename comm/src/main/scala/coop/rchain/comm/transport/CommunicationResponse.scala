package coop.rchain.comm.transport

import coop.rchain.comm.protocol.routing.Protocol

sealed trait CommunicationResponse
case class HandledWithMessage(pm: Protocol) extends CommunicationResponse
case object HandledWitoutMessage            extends CommunicationResponse
case object NotHandled                      extends CommunicationResponse

object CommunicationResponse {
  def handledWithMessage(protocol: Protocol): CommunicationResponse = HandledWithMessage(protocol)
  def handledWitoutMessage: CommunicationResponse                   = HandledWitoutMessage
  def notHandled: CommunicationResponse                             = NotHandled
}
