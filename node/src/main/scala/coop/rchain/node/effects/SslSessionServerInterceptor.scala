package coop.rchain.node.effects

import coop.rchain.comm.protocol.routing._
import coop.rchain.node.CertificateHelper

import io.grpc._
import javax.net.ssl.SSLSession

class SslSessionServerInterceptor() extends ServerInterceptor {

  def interceptCall[ReqT, RespT](
      call: ServerCall[ReqT, RespT],
      headers: Metadata,
      next: ServerCallHandler[ReqT, RespT]
  ): ServerCall.Listener[ReqT] = new InterceptionListener(next.startCall(call, headers), call)

  private class InterceptionListener[ReqT, RespT](next: ServerCall.Listener[ReqT],
                                                  call: ServerCall[ReqT, RespT])
      extends ServerCall.Listener[ReqT] {
    override def onHalfClose(): Unit = next.onHalfClose()
    override def onCancel(): Unit    = next.onCancel()
    override def onComplete(): Unit  = next.onComplete()
    override def onReady(): Unit     = next.onReady()

    override def onMessage(message: ReqT): Unit =
      message match {
        case TLRequest(Some(Protocol(Some(Header(Some(sender), _, _)), _))) =>
          val sslSession: Option[SSLSession] = Option(
            call.getAttributes.get(Grpc.TRANSPORT_ATTR_SSL_SESSION))
          if (sslSession.isEmpty) {
            close()
          } else {
            sslSession.foreach { session =>
              val verified = CertificateHelper
                .publicAddress(session.getPeerCertificates.head.getPublicKey)
                .exists(_ sameElements sender.id.toByteArray)
              if (verified)
                next.onMessage(message)
              else
                close()
            }
          }
        case _ => next.onMessage(message)
      }

    private def close(): Unit =
      throw Status.UNAUTHENTICATED.withDescription("Wrong public key").asRuntimeException()
  }
}
