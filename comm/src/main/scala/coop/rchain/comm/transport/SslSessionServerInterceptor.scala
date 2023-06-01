package coop.rchain.comm.transport

import cats.effect.Async
import cats.effect.std.Dispatcher
import coop.rchain.comm.protocol.routing.{Header => RHeader, _}
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.crypto.util.CertificateHelper
import coop.rchain.shared.{Log, LogSource}
import io.grpc._

import javax.net.ssl.SSLSession

/**
  * This wart exists because that's how gRPC works
  */
@SuppressWarnings(Array("org.wartremover.warts.Var"))
class SslSessionServerInterceptor[F[_]: Async](networkID: String, d: Dispatcher[F])
    extends ServerInterceptor {

  def interceptCall[ReqT, RespT](
      call: ServerCall[ReqT, RespT],
      headers: Metadata,
      next: ServerCallHandler[ReqT, RespT]
  ): ServerCall.Listener[ReqT] = new InterceptionListener(next.startCall(call, headers), call)

  implicit private val logSource: LogSource = LogSource(this.getClass)

  private class InterceptionListener[ReqT, RespT](
      next: ServerCall.Listener[ReqT],
      call: ServerCall[ReqT, RespT]
  ) extends ServerCall.Listener[ReqT] {

    @volatile
    private var closeWithStatus = Option.empty[Status]

    override def onHalfClose(): Unit =
      closeWithStatus.fold(next.onHalfClose())(call.close(_, new Metadata()))

    override def onCancel(): Unit   = next.onCancel()
    override def onComplete(): Unit = next.onComplete()
    override def onReady(): Unit    = next.onReady()

    override def onMessage(message: ReqT): Unit =
      message match {
        case TLRequest(Protocol(RHeader(sender, nid), msg)) =>
          if (nid == networkID) {
            if (d.unsafeRunSync(Log.log[F].isTraceEnabled)) {
              val peerNode = ProtocolHelper.toPeerNode(sender)
              val msgType  = msg.getClass.toString
              val logPure  = Log.log[F].trace(s"Request [$msgType] from peer ${peerNode.toAddress}")
              d.unsafeRunSync(logPure)
            }
            val sslSession: Option[SSLSession] = Option(
              call.getAttributes.get(Grpc.TRANSPORT_ATTR_SSL_SESSION)
            )
            if (sslSession.isEmpty) {
              val logPure = Log.log[F].warn("No TLS Session. Closing connection")
              d.unsafeRunSync(logPure)
              close(Status.UNAUTHENTICATED.withDescription("No TLS Session"))
            } else {
              sslSession.foreach { session =>
                val verified = CertificateHelper
                  .publicAddress(session.getPeerCertificates.head.getPublicKey)
                  .exists(_ sameElements sender.id.toByteArray)
                if (verified)
                  next.onMessage(message)
                else {
                  val logPure =
                    Log.log[F].warn("Certificate verification failed. Closing connection")
                  d.unsafeRunSync(logPure)
                  close(Status.UNAUTHENTICATED.withDescription("Certificate verification failed"))
                }
              }
            }
          } else {
            val nidStr  = if (nid.isEmpty) "<empty>" else nid
            val logPure = Log.log[F].warn(s"Wrong network id '$nidStr'. Closing connection")
            d.unsafeRunSync(logPure)
            close(
              Status.PERMISSION_DENIED
                .withDescription(
                  s"Wrong network id '$nidStr'. This node runs on network '$networkID'"
                )
            )
          }
        case TLRequest(_) =>
          val logPure = Log.log[F].warn(s"Malformed message $message")
          d.unsafeRunSync(logPure)
          close(Status.INVALID_ARGUMENT.withDescription("Malformed message"))
        case _ => next.onMessage(message)
      }

    private def close(status: Status): Unit =
      closeWithStatus = Some(status)
  }
}
