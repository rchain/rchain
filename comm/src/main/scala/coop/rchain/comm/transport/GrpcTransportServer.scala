package coop.rchain.comm.transport

import java.io.ByteArrayInputStream
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import scala.io.Source
import scala.util.{Left, Right}
import cats.implicits._
import coop.rchain.catscontrib.TaskContrib
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.CommMetricsSource
import coop.rchain.metrics.Metrics
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
    networkId: String,
    port: Int,
    cert: String,
    key: String,
    maxMessageSize: Int,
    maxStreamMessageSize: Long,
    tempFolder: Path,
    parallelism: Int
)(
    implicit scheduler: Scheduler,
    rPConfAsk: RPConfAsk[Task],
    log: Log[Task],
    metrics: Metrics[Task]
) extends TransportLayerServer[Task] {
  private def certInputStream = new ByteArrayInputStream(cert.getBytes())
  private def keyInputStream  = new ByteArrayInputStream(key.getBytes())

  implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  private val queueScheduler =
    Scheduler.fixedPool(
      "tl-dispatcher-server-queue",
      parallelism,
      reporter = UncaughtExceptionLogger
    )

  private val serverSslContextTask: Task[SslContext] =
    Task
      .evalOnce {
        GrpcSslContexts
          .configure(SslContextBuilder.forServer(certInputStream, keyInputStream))
          .trustManager(HostnameTrustManagerFactory.Instance)
          .clientAuth(ClientAuth.REQUIRE)
          .build()
      }
      .attempt
      .flatMap {
        case Right(sslContext) => sslContext.pure[Task]
        case Left(t)           => Task.raiseError[SslContext](t)
      }

  def receive(
      dispatch: Protocol => Task[CommunicationResponse],
      handleStreamed: Blob => Task[Unit]
  ): Task[Cancelable] = {

    val dispatchSend: Send => Task[Unit] =
      s => dispatch(s.msg).attemptAndLog >> metrics.incrementCounter("dispatched.messages")

    val dispatchBlob: StreamMessage => Task[Unit] =
      msg =>
        (StreamHandler.restore(msg) >>= {
          case Left(ex) =>
            Log[Task].error("Could not restore data from file while handling stream", ex)
          case Right(blob) =>
            handleStreamed(blob)
        }) >> metrics.incrementCounter("dispatched.packets")

    for {
      serverSslContext <- serverSslContextTask
      tellBuffer       <- Task.delay(buffer.LimitedBufferObservable.dropNew[Send](1024))
      blobBuffer       <- Task.delay(buffer.LimitedBufferObservable.dropNew[StreamMessage](100))
      receiver <- GrpcTransportReceiver.create(
                   networkId: String,
                   port,
                   serverSslContext,
                   maxMessageSize,
                   maxStreamMessageSize,
                   tellBuffer,
                   blobBuffer,
                   tempFolder = tempFolder
                 )
      tellConsumer <- Task.delay(
                       tellBuffer
                         .mapParallelUnordered(parallelism)(dispatchSend)
                         .subscribe()(queueScheduler)
                     )
      blobConsumer <- Task.delay(
                       blobBuffer.mapEval(dispatchBlob).subscribe()(queueScheduler)
                     )
    } yield Cancelable.collection(receiver, tellConsumer, blobConsumer)
  }
}

object GrpcTransportServer {
  def acquireServer(
      networkId: String,
      port: Int,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int,
      maxStreamMessageSize: Long,
      folder: Path,
      parallelism: Int
  )(
      implicit scheduler: Scheduler,
      rPConfAsk: RPConfAsk[Task],
      log: Log[Task],
      metrics: Metrics[Task]
  ): TransportServer = {
    val cert = Resources.withResource(Source.fromFile(certPath.toFile))(_.mkString)
    val key  = Resources.withResource(Source.fromFile(keyPath.toFile))(_.mkString)
    new TransportServer(
      new GrpcTransportServer(
        networkId,
        port,
        cert,
        key,
        maxMessageSize,
        maxStreamMessageSize,
        folder,
        parallelism
      )
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

  def stop(): Task[Unit] =
    ref
      .getAndSet(None)
      .fold(Task.unit)(c => Task.delay(c.cancel()))
}
