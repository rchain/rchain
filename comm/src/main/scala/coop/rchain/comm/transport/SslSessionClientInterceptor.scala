package coop.rchain.comm.transport

import cats.effect.Async
import cats.effect.std.Dispatcher
import coop.rchain.comm.protocol.routing.{Header => RHeader, _}
import coop.rchain.comm.protocol.routing.TLResponse.Payload
import coop.rchain.crypto.util.CertificateHelper
import coop.rchain.shared.{Log, LogSource}
import io.grpc._

import javax.net.ssl.SSLSession

class SslSessionClientInterceptor[F[_]: Async](networkID: String, d: Dispatcher[F])
    extends ClientInterceptor {
  def interceptCall[ReqT, RespT](
      method: MethodDescriptor[ReqT, RespT],
      callOptions: CallOptions,
      next: Channel
  ): ClientCall[ReqT, RespT] =
    new SslSessionClientCallInterceptor[F, ReqT, RespT](
      next.newCall(method, callOptions),
      networkID,
      d
    )
}

/**
  * This wart exists because that's how grpc works
  */
@SuppressWarnings(Array("org.wartremover.warts.Var"))
class SslSessionClientCallInterceptor[F[_]: Async, ReqT, RespT](
    next: ClientCall[ReqT, RespT],
    networkID: String,
    d: Dispatcher[F]
) extends ClientCall[ReqT, RespT] {
  self =>

  implicit private val logSource: LogSource = LogSource(this.getClass)

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
            close(Status.PERMISSION_DENIED.withDescription(s"Wrong network id '$nidStr'"))
          }

        case TLResponse(Payload.InternalServerError(_)) =>
          next.onMessage(message)

        case TLResponse(_) =>
          val logPure = Log.log[F].warn(s"Malformed response $message")
          d.unsafeRunSync(logPure)
          close(Status.INVALID_ARGUMENT.withDescription("Malformed message"))
        case _ => next.onMessage(message)
      }

    private def close(status: Status): Unit =
      closeWithStatus = Some(status)
  }
}
