package coop.rchain.comm.transport

import java.io.ByteArrayInputStream

import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import coop.rchain.shared.{Cell, Log, LogSource}

import scala.concurrent.duration._
import scala.util._
import io.grpc._, io.grpc.netty._
import io.netty.handler.ssl.{ClientAuth, SslContext, SslContextBuilder}
import coop.rchain.comm.protocol.routing.RoutingGrpcMonix.TransportLayerStub
import monix.eval._, monix.execution._
import scala.concurrent.TimeoutException

class TcpTransportLayer(host: String, port: Int, cert: String, key: String, maxMessageSize: Int)(
    implicit scheduler: Scheduler,
    cell: TcpTransportLayer.TransportCell[Task],
    log: Log[Task]
) extends TransportLayer[Task] {

  private val DefaultSendTimeout = 5.seconds

  private implicit val logSource: LogSource = LogSource(this.getClass)

  private def certInputStream = new ByteArrayInputStream(cert.getBytes())
  private def keyInputStream  = new ByteArrayInputStream(key.getBytes())

  private lazy val serverSslContext: SslContext =
    try {
      GrpcSslContexts
        .configure(SslContextBuilder.forServer(certInputStream, keyInputStream))
        .trustManager(HostnameTrustManagerFactory.Instance)
        .clientAuth(ClientAuth.REQUIRE)
        .build()
    } catch {
      case e: Throwable =>
        println(e.getMessage)
        throw e
    }

  private lazy val clientSslContext: SslContext =
    try {
      val builder = GrpcSslContexts.forClient
      builder.trustManager(HostnameTrustManagerFactory.Instance)
      builder.keyManager(certInputStream, keyInputStream)
      builder.build
    } catch {
      case e: Throwable =>
        println(e.getMessage)
        throw e
    }

  private def clientChannel(peer: PeerNode): Task[ManagedChannel] =
    for {
      _ <- log.debug(s"Creating new channel to peer ${peer.toAddress}")
      c <- Task.delay {
            NettyChannelBuilder
              .forAddress(peer.endpoint.host, peer.endpoint.tcpPort)
              .executor(scheduler)
              .maxInboundMessageSize(maxMessageSize)
              .negotiationType(NegotiationType.TLS)
              .sslContext(clientSslContext)
              .intercept(new SslSessionClientInterceptor())
              .overrideAuthority(peer.id.toString)
              .build()
          }
    } yield c

  private def connection(peer: PeerNode, enforce: Boolean): Task[ManagedChannel] =
    cell.modify { s =>
      if (s.shutdown && !enforce)
        Task.raiseError(new RuntimeException("The transport layer has been shut down")).as(s)
      else
        for {
          c <- s.connections.get(peer).fold(clientChannel(peer))(_.pure[Task])
        } yield s.copy(connections = s.connections + (peer -> c))
    } >>= kp(cell.read.map(_.connections.apply(peer)))

  def disconnect(peer: PeerNode): Task[Unit] =
    cell.modify { s =>
      for {
        _ <- s.connections.get(peer) match {
              case Some(c) =>
                log
                  .debug(s"Disconnecting from peer ${peer.toAddress}")
                  .map(kp(Try(c.shutdown())))
                  .void
              case _ => Task.unit // ignore if connection does not exists already
            }
      } yield s.copy(connections = s.connections - peer)
    }

  private object PeerUnavailable {
    def unapply(e: Throwable): Boolean =
      e.isInstanceOf[StatusRuntimeException] &&
        e.asInstanceOf[StatusRuntimeException].getStatus.getCode == Status.Code.UNAVAILABLE
  }

  private def withClient[A](peer: PeerNode, enforce: Boolean)(
      f: TransportLayerStub => Task[A]
  ): Task[A] =
    for {
      channel <- connection(peer, enforce)
      stub    <- Task.delay(RoutingGrpcMonix.stub(channel))
      result <- f(stub).doOnFinish {
                 case Some(PeerUnavailable()) => disconnect(peer)
                 case _                       => Task.unit
               }
      _ <- Task.unit.asyncBoundary // return control to caller thread
    } yield result

  private def transport(peer: PeerNode, enforce: Boolean)(
      f: TransportLayerStub => Task[TLResponse]
  ): Task[CommErr[Option[Protocol]]] =
    withClient(peer, enforce)(f).attempt.map(processResponse(peer, _))

  private def processResponse(
      peer: PeerNode,
      response: Either[Throwable, TLResponse]
  ): CommErr[Option[Protocol]] =
    response
      .leftMap {
        case _: TimeoutException => CommError.timeout
        case PeerUnavailable()   => peerUnavailable(peer)
        case e                   => protocolException(e)
      }
      .flatMap(
        tlr =>
          tlr.payload match {
            case p if p.isProtocol   => Right(Some(tlr.getProtocol))
            case p if p.isNoResponse => Right(None)
            case TLResponse.Payload.InternalServerError(ise) =>
              Left(internalCommunicationError("Got response: " + ise.error.toStringUtf8))
          }
      )

  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): Task[CommErr[Protocol]] =
    transport(peer, enforce = false)(_.ask(TLRequest(msg.some)).nonCancelingTimeout(timeout))
      .map(_.flatMap {
        case Some(p) => Right(p)
        case _       => Left(internalCommunicationError("Was expecting message, nothing arrived"))
      })

  private def innerSend(
      peer: PeerNode,
      msg: Protocol,
      enforce: Boolean = false,
      timeout: FiniteDuration = DefaultSendTimeout
  ): Task[CommErr[Unit]] =
    transport(peer, enforce)(_.ask(TLRequest(msg.some)).nonCancelingTimeout(timeout))
      .map(_.flatMap {
        case Some(p) => Left(internalCommunicationError(s"Was expecting no message. Response: $p"))
        case _       => Right(())
      })

  private def innerBroadcast(
      peers: Seq[PeerNode],
      msg: Protocol,
      enforce: Boolean = false,
      timeOut: FiniteDuration = DefaultSendTimeout
  ): Task[Seq[CommErr[Unit]]] =
    Task.gatherUnordered(peers.map(innerSend(_, msg, enforce, timeOut)))

  def send(peer: PeerNode, msg: Protocol): Task[CommErr[Unit]] =
    innerSend(peer, msg)

  def broadcast(peers: Seq[PeerNode], msg: Protocol): Task[Seq[CommErr[Unit]]] =
    innerBroadcast(peers, msg)

  private def receiveInternal(
      parallelism: Int
  )(dispatch: Protocol => Task[CommunicationResponse]): Task[Cancelable] = {

    def dispatchInternal: ServerMessage => Task[Unit] = {
      // TODO: consider logging on failure (Left)
      case Tell(protocol) => dispatch(protocol).attempt.void
      case Ask(protocol, sender) if !sender.complete =>
        dispatch(protocol).attempt.map {
          case Left(e)         => sender.failWith(e)
          case Right(response) => sender.reply(response)
        }.void
      case _ => Task.unit // sender timeout
    }

    Task.delay {
      new TcpServerObservable(port, serverSslContext, maxMessageSize)
        .mapParallelUnordered(parallelism)(dispatchInternal)
        .subscribe()(Scheduler.computation(parallelism, "tl-dispatcher"))
    }
  }

  def receive(dispatch: Protocol => Task[CommunicationResponse]): Task[Unit] =
    cell.modify { s =>
      for {
        server <- s.server match {
                   case Some(_) =>
                     Task.raiseError(
                       new RuntimeException("TransportLayer server is already started")
                     )
                   case _ =>
                     val parallelism = Math.max(Runtime.getRuntime.availableProcessors(), 2)
                     receiveInternal(parallelism)(dispatch)
                 }
      } yield s.copy(server = Some(server))

    }

  def shutdown(msg: Protocol): Task[Unit] = {
    def shutdownServer: Task[Unit] = cell.modify { s =>
      for {
        _ <- log.info("Shutting down server")
        _ <- s.server.fold(Task.unit)(server => Task.delay(server.cancel()))
      } yield s.copy(server = None, shutdown = true)
    }

    def sendShutdownMessages: Task[Unit] =
      for {
        peers <- cell.read.map(_.connections.keys.toSeq)
        _     <- log.info("Sending shutdown message to all peers")
        _     <- innerBroadcast(peers, msg, enforce = true, timeOut = 500.milliseconds)
        _     <- log.info("Disconnecting from all peers")
        _     <- Task.gatherUnordered(peers.map(disconnect))
      } yield ()

    cell.read.flatMap { s =>
      if (s.shutdown) Task.unit
      else shutdownServer *> sendShutdownMessages
    }
  }
}

object TcpTransportLayer {
  type Connection          = ManagedChannel
  type Connections         = Map[PeerNode, Connection]
  type TransportCell[F[_]] = Cell[F, TransportState]
}

case class TransportState(
    connections: TcpTransportLayer.Connections = Map.empty,
    server: Option[Cancelable] = None,
    shutdown: Boolean = false
)

object TransportState {
  def empty: TransportState = TransportState()
}
