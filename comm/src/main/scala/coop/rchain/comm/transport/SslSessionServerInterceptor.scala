package coop.rchain.comm.transport

import cats.Id

import coop.rchain.catscontrib._
import coop.rchain.comm.ProtocolHelper
import coop.rchain.comm.protocol.routing._
import coop.rchain.shared.{Log, LogSource}

import io.grpc._
import javax.net.ssl.SSLSession

class SslSessionServerInterceptor() extends ServerInterceptor {

  def interceptCall[ReqT, RespT](
      call: ServerCall[ReqT, RespT],
      headers: Metadata,
      next: ServerCallHandler[ReqT, RespT]
  ): ServerCall.Listener[ReqT] = new InterceptionListener(next.startCall(call, headers), call)

  private implicit val logSource: LogSource = LogSource(this.getClass)
  private val log                           = Log.log[Id]

  private class InterceptionListener[ReqT, RespT](next: ServerCall.Listener[ReqT],
                                                  call: ServerCall[ReqT, RespT])
      extends ServerCall.Listener[ReqT] {

    override def onHalfClose(): Unit = next.onHalfClose()
    override def onCancel(): Unit    = next.onCancel()
    override def onComplete(): Unit  = next.onComplete()
    override def onReady(): Unit     = next.onReady()

    override def onMessage(message: ReqT): Unit =
      message match {
        case TLRequest(Some(Protocol(Some(Header(Some(sender))), msg))) =>
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
            log.trace(s"Request [$msgType] from peer ${peerNode.toAddress}")
          }
          val sslSession: Option[SSLSession] = Option(
            call.getAttributes.get(Grpc.TRANSPORT_ATTR_SSL_SESSION))
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
