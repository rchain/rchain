package coop.rchain.comm.transport

import java.io.File

import scala.concurrent.duration.FiniteDuration

import cats._
import cats.data._
import cats.implicits._

import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.protocol.routing.TransportLayerGrpc.TransportLayerStub
import coop.rchain.comm.CommError.{protocolException, CommErr}

import io.grpc.ManagedChannel
import io.grpc.netty._
import io.netty.handler.ssl.SslContext
import monix.eval.Task

trait TcpClient[F[_]] {
  def send(remote: PeerNode, request: TLRequest, timeout: FiniteDuration): F[CommErr[TLResponse]]

  def sendAndForget(remote: PeerNode, request: TLRequest): F[CommErr[Unit]]
}

object TcpClient {
  def apply[F[_]](implicit C: TcpClient[F]): TcpClient[F] = C
}

sealed abstract class TcpClientInstances {
  implicit def eitherTTcpClient[E, F[_]: Monad](
      implicit evF: TcpClient[F]): TcpClient[EitherT[F, E, ?]] =
    new TcpClient[EitherT[F, E, ?]] {
      def send(remote: PeerNode,
               request: TLRequest,
               timeout: FiniteDuration): EitherT[F, E, CommErr[TLResponse]] =
        EitherT.liftF(evF.send(remote, request, timeout))

      def sendAndForget(remote: PeerNode, request: TLRequest): EitherT[F, E, CommErr[Unit]] =
        EitherT.liftF(evF.sendAndForget(remote, request))
    }
}

class GrpcTcpClient(cert: File, key: File) extends TcpClient[Task] {

  private lazy val clientSslContext: SslContext =
    try {
      val builder = GrpcSslContexts.forClient
      builder.trustManager(HostnameTrustManagerFactory.Instance)
      builder.keyManager(cert, key)
      builder.build
    } catch {
      case e: Throwable =>
        println(e.getMessage)
        throw e
    }

  private def clientChannel(remote: PeerNode): Task[ManagedChannel] =
    Task.delay {
      NettyChannelBuilder
        .forAddress(remote.endpoint.host, remote.endpoint.tcpPort)
        .negotiationType(NegotiationType.TLS)
        .sslContext(clientSslContext)
        .intercept(new SslSessionClientInterceptor())
        .overrideAuthority(remote.id.toString)
        .build()
    }

  private def withClient[A](remote: PeerNode)(f: TransportLayerStub => Task[A]): Task[A] =
    for {
      channel <- clientChannel(remote)
      stub    <- Task.delay(TransportLayerGrpc.stub(channel))
      result  <- f(stub).doOnFinish(_ => Task.pure(channel.shutdown()))
    } yield result

  private def innerSend(remote: PeerNode, request: TLRequest): Task[TLResponse] =
    withClient(remote)(stub => Task.fromFuture(stub.send(request)))

  def send(remote: PeerNode,
           request: TLRequest,
           timeout: FiniteDuration): Task[CommErr[TLResponse]] =
    innerSend(remote, request)
      .timeout(timeout)
      .attempt
      .map(_.leftMap(protocolException))

  def sendAndForget(remote: PeerNode, request: TLRequest): Task[CommErr[Unit]] =
    Task
      .racePair(innerSend(remote, request), Task.unit)
      .attempt
      .map(_.bimap(protocolException, _ => ()))
}
