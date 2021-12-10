package coop.rchain.comm.transport

import coop.rchain.comm.protocol.routing.{Header => RHeader, _}
import coop.rchain.comm.protocol.routing.TLResponse.Payload
import coop.rchain.crypto.util.CertificateHelper
import coop.rchain.shared.{Log, LogSource}

import io.grpc._
import javax.net.ssl.SSLSession

class SslSessionClientInterceptor(networkID: String) extends ClientInterceptor {
  def interceptCall[ReqT, RespT](
      method: MethodDescriptor[ReqT, RespT],
      callOptions: CallOptions,
      next: Channel
  ): ClientCall[ReqT, RespT] =
    new SslSessionClientCallInterceptor(next.newCall(method, callOptions), networkID)
}

/**
  * This wart exists because that's how grpc works
  */
@SuppressWarnings(Array("org.wartremover.warts.Var"))
class SslSessionClientCallInterceptor[ReqT, RespT](next: ClientCall[ReqT, RespT], networkID: String)
    extends ClientCall[ReqT, RespT] {
  self =>

  implicit private val logSource: LogSource = LogSource(this.getClass)
  private val log                           = Log.logId

  def cancel(message: String, cause: Throwable): Unit = next.cancel(message, cause)
  def request(numMessages: Int): Unit                 = next.request(numMessages)
  def sendMessage(message: ReqT): Unit                = next.sendMessage(message)
  def halfClose(): Unit                               = next.halfClose()

  override def isReady: Boolean                              = next.isReady
  override def setMessageCompression(enabled: Boolean): Unit = next.setMessageCompression(enabled)
  override def getAttributes: Attributes                     = next.getAttributes

  def start(responseListener: ClientCall.Listener[RespT], headers: Metadata): Unit =
    next.start(new InterceptionListener(responseListener), headers)

  private class InterceptionListener(next: ClientCall.Listener[RespT])
      extends ClientCall.Listener[RespT] {
    @volatile
    private var closeWithStatus = Option.empty[Status]

    override def onClose(status: Status, trailers: Metadata): Unit =
      closeWithStatus.fold(next.onClose(status, trailers))(next.onClose(_, new Metadata()))

    override def onReady(): Unit                    = next.onReady()
    override def onHeaders(headers: Metadata): Unit = next.onHeaders(headers)

    override def onMessage(message: RespT): Unit =
      message match {
        case TLResponse(Payload.Ack(Ack(RHeader(sender, nid)))) =>
          if (nid == networkID) {
            val sslSession: Option[SSLSession] = Option(
              self.getAttributes.get(Grpc.TRANSPORT_ATTR_SSL_SESSION)
            )
            if (sslSession.isEmpty) {
              log.warn("No TLS Session. Closing connection")
              close(Status.UNAUTHENTICATED.withDescription("No TLS Session"))
            } else {
              sslSession.foreach { session =>
                val verified = CertificateHelper
                  .publicAddress(session.getPeerCertificates.head.getPublicKey)
                  .exists(_ sameElements sender.id.toByteArray)
                if (verified)
                  next.onMessage(message)
                else {
                  log.warn("Certificate verification failed. Closing connection")
                  close(Status.UNAUTHENTICATED.withDescription("Certificate verification failed"))
                }
              }
            }
          } else {
            val nidStr = if (nid.isEmpty) "<empty>" else nid
            log.warn(s"Wrong network id '$nidStr'. Closing connection")
            close(Status.PERMISSION_DENIED.withDescription(s"Wrong network id '$nidStr'"))
          }

        case TLResponse(Payload.InternalServerError(_)) =>
          next.onMessage(message)

        case TLResponse(_) =>
          log.warn(s"Malformed response $message")
          close(Status.INVALID_ARGUMENT.withDescription("Malformed message"))
        case _ => next.onMessage(message)
      }

    private def close(status: Status): Unit =
      closeWithStatus = Some(status)
  }
}
