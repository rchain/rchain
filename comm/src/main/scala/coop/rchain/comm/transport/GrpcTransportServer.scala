package coop.rchain.comm.transport

import java.io.ByteArrayInputStream
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import scala.io.Source
import scala.util.{Left, Right}

import cats.data.EitherT
import cats.implicits._

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.CommError
import coop.rchain.shared._

import io.grpc.netty.GrpcSslContexts
import io.netty.handler.ssl._
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}

trait TransportLayerServer[F[_]] {
  def receive(
      dispatch: Protocol => F[CommunicationResponse],
      handleStreamed: Blob => F[Unit]
  ): F[Cancelable]
}

class GrpcTransportServer(
    port: Int,
    cert: String,
    key: String,
    maxMessageSize: Int,
    tempFolder: Path,
    parallelism: Int
)(
    implicit scheduler: Scheduler,
    log: Log[Task]
) extends TransportLayerServer[Task] {
  private def certInputStream = new ByteArrayInputStream(cert.getBytes())
  private def keyInputStream  = new ByteArrayInputStream(key.getBytes())

  private val queueScheduler =
    Scheduler.fixedPool("tl-dispatcher-server", parallelism, reporter = UncaughtExceptionLogger)

  // TODO FIX-ME No throwing exceptions!
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private lazy val serverSslContext: SslContext =
    try {
      GrpcSslContexts
        .configure(SslContextBuilder.forServer(certInputStream, keyInputStream))
        .trustManager(HostnameTrustManagerFactory.Instance)
        .clientAuth(ClientAuth.REQUIRE)
        .build()
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        throw e
    }

  def receive(
      dispatch: Protocol => Task[CommunicationResponse],
      handleStreamed: Blob => Task[Unit]
  ): Task[Cancelable] = {

    val dispatchInternal: ServerMessage => Task[Unit] = {
      // TODO: consider logging on failure (Left)
      case Tell(protocol) => dispatch(protocol).attemptAndLog.void
      case Ask(protocol, handle) if !handle.complete =>
        dispatch(protocol).attempt.map {
          case Left(e)         => handle.failWith(e)
          case Right(response) => handle.reply(response)
        }.void
      case msg: StreamMessage =>
        StreamHandler.restore(msg) >>= {
          case Left(ex) =>
            Log[Task].error("Could not restore data from file while handling stream", ex)
          case Right(blob) => handleStreamed(blob)
        }
      case _ => Task.unit // sender timeout
    }

    Task.delay {
      new TcpServerObservable(
        port,
        serverSslContext,
        maxMessageSize,
        tempFolder = tempFolder
      ).mapParallelUnordered(parallelism)(dispatchInternal)
        .subscribe()(queueScheduler)
    }
  }

}

object GrpcTransportServer {
  def acquireServer(
      port: Int,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int,
      folder: Path,
      parallelism: Int
  )(
      implicit scheduler: Scheduler,
      log: Log[Task]
  ): TransportServer = {
    val cert = Resources.withResource(Source.fromFile(certPath.toFile))(_.mkString)
    val key  = Resources.withResource(Source.fromFile(keyPath.toFile))(_.mkString)
    new TransportServer(
      new GrpcTransportServer(port, cert, key, maxMessageSize, folder, parallelism)
    )
  }
}

class TransportServer(server: GrpcTransportServer) {
  private val ref: AtomicReference[Option[Cancelable]] =
    new AtomicReference[Option[Cancelable]](None)

  def start(
      dispatch: Protocol => Task[CommunicationResponse],
      handleStreamed: Blob => Task[Unit]
  ): Task[Unit] =
    ref.get() match {
      case Some(_) => Task.unit
      case _ =>
        server
          .receive(dispatch, handleStreamed)
          .foreachL { cancelable =>
            ref
              .getAndSet(Some(cancelable))
              .fold(())(c => c.cancel())
          }
    }

  import CommunicationResponse._

  def startWithEffects(
      dispatch: Protocol => EitherT[Task, CommError, CommunicationResponse],
      handleStreamed: Blob => EitherT[Task, CommError, Unit]
  )(implicit log: Log[Task]): EitherT[Task, CommError, Unit] = {
    val dis: Protocol => Task[CommunicationResponse] = msg =>
      dispatch(msg).value.flatMap {
        case Left(err) =>
          Log[Task].error(s"Error while handling message. Error: ${err.message}") *> Task.now(
            notHandled(err)
          )
        case Right(m) => Task.now(m)
      }
    val hb: Blob => Task[Unit] = b =>
      handleStreamed(b).value.flatMap {
        case Left(err) =>
          Log[Task].error(s"Error while handling streamed Packet message. Error: ${err.message}")
        case Right(_) => Task.unit
      }
    EitherT.liftF(start(dis, hb))
  }

  def stop(): Task[Unit] =
    ref
      .getAndSet(None)
      .fold(Task.unit)(c => Task.delay(c.cancel()))
}
