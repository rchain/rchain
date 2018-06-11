package coop.rchain.comm.transport

import coop.rchain.comm.ProtocolMessage

sealed trait CommunicationResponse
case class HandledWithMessage(pm: ProtocolMessage) extends CommunicationResponse
case object HandledWitoutMessage                   extends CommunicationResponse
case object NotHandled                             extends CommunicationResponse

object CommunicationResponse {
  def handledWithMessage(pm: ProtocolMessage): CommunicationResponse = HandledWithMessage(pm)
  def handledWitoutMessage: CommunicationResponse                    = HandledWitoutMessage
  def notHandled: CommunicationResponse                              = NotHandled
}
