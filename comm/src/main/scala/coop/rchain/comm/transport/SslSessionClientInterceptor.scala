package coop.rchain.comm.transport

import cats.Id

import coop.rchain.catscontrib._
import coop.rchain.comm.ProtocolHelper
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.protocol.routing.TLResponse.Payload
import coop.rchain.shared.{Log, LogSource}

import io.grpc._
import javax.net.ssl.SSLSession

class SslSessionClientInterceptor() extends ClientInterceptor {
  def interceptCall[ReqT, RespT](method: MethodDescriptor[ReqT, RespT],
                                 callOptions: CallOptions,
                                 next: Channel): ClientCall[ReqT, RespT] =
    new SslSessionClientCallInterceptor(next.newCall(method, callOptions))
}

class SslSessionClientCallInterceptor[ReqT, RespT](next: ClientCall[ReqT, RespT])
    extends ClientCall[ReqT, RespT] {
  self =>

  private implicit val logSource: LogSource = LogSource(this.getClass)
  private val log                           = Log.log[Id]

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
    override def onClose(status: Status, trailers: Metadata): Unit = next.onClose(status, trailers)
    override def onReady(): Unit                                   = next.onReady()
    override def onHeaders(headers: Metadata): Unit                = next.onHeaders(headers)

    override def onMessage(message: RespT): Unit =
      message match {
        case TLResponse(Payload.Protocol(Protocol(Some(Header(Some(sender))), msg))) =>
          if (log.isTraceEnabled) {
            val peerNode = ProtocolHelper.toPeerNode(sender)
            val msgType = msg match {
              case m if m.isLookup         => "lookup"
              case m if m.isLookupResponse => "lookup response"
              case m if m.isPing           => "ping"
              case m if m.isPong           => "pong"
              case m if m.isUpstream       => "upstream"
              case m if m.isEmpty          => "empty"
              case _                       => "unknown"
            }
            log.trace(s"Response [$msgType] from peer ${peerNode.toAddress}")
          }
          val sslSession: Option[SSLSession] = Option(
            self.getAttributes.get(Grpc.TRANSPORT_ATTR_SSL_SESSION))
          if (sslSession.isEmpty) {
            log.warn("No TLS Session. Closing connection")
            close()
          } else {
            sslSession.foreach { session =>
              val verified = CertificateHelper
                .publicAddress(session.getPeerCertificates.head.getPublicKey)
                .exists(_ sameElements sender.id.toByteArray)
              if (verified)
                next.onMessage(message)
              else {
                log.warn("Certificate verification failed. Closing connection")
                close()
              }
            }
          }

        case _ => next.onMessage(message)
      }

    private def close(): Unit =
      throw Status.UNAUTHENTICATED.withDescription("Wrong public key").asRuntimeException()

  }
}
