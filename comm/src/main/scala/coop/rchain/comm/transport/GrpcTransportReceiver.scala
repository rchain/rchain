package coop.rchain.comm.transport

import java.nio.file.Path

import scala.concurrent.duration._

import cats.implicits._

import coop.rchain.comm.{CommMetricsSource, PeerNode}
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.transport.buffer.LimitedBuffer
import coop.rchain.metrics.Metrics
import coop.rchain.shared._
import coop.rchain.shared.PathOps.PathDelete

import io.grpc.netty.NettyServerBuilder
import io.netty.handler.ssl.SslContext
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable

object GrpcTransportReceiver {

  implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  def create(
      networkId: String,
      port: Int,
      serverSslContext: SslContext,
      maxMessageSize: Int,
      maxStreamMessageSize: Long,
      tellBuffer: LimitedBuffer[Send],
      blobBuffer: LimitedBuffer[StreamMessage],
      askTimeout: FiniteDuration = 5.second,
      ioScheduler: Scheduler,
      tempFolder: Path
  )(
      implicit mainScheduler: Scheduler,
      rPConfAsk: RPConfAsk[Task],
      logger: Log[Task],
      metrics: Metrics[Task]
  ): Task[Cancelable] =
    Task.delay {
      val service = new RoutingGrpcMonix.TransportLayer {

        def send(request: TLRequest): Task[TLResponse] =
          request.protocol
            .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
              rPConfAsk.reader(_.local) >>= (
                  src =>
                    Task
                      .delay(tellBuffer.pushNext(Send(protocol)))
                      .ifM(
                        metrics.incrementCounter("enqueued.messages") >> Task
                          .delay(ack(src)),
                        metrics.incrementCounter("dropped.messages") >> Task
                          .delay(internalServerError("message dropped"))
                      )
                )
            }

        private val circuitBreaker: StreamHandler.CircuitBreaker = streamed =>
          if (streamed.header.exists(_.networkId != networkId))
            Opened(StreamHandler.StreamError.wrongNetworkId)
          else if (streamed.readSoFar > maxStreamMessageSize)
            Opened(StreamHandler.StreamError.circuitOpened)
          else Closed

        def stream(observable: Observable[Chunk]): Task[TLResponse] = {
          import StreamHandler._
          import StreamError.StreamErrorToMessage

          handleStream(tempFolder, observable, circuitBreaker) >>= {
            case Left(error @ StreamError.Unexpected(t)) =>
              logger.error(error.message, t).as(internalServerError(error.message))
            case Left(error) =>
              logger.warn(error.message).as(internalServerError(error.message))
            case Right(msg) =>
              metrics.incrementCounter("received.packets") >>
                Task
                  .delay(blobBuffer.pushNext(msg))
                  .ifM(
                    List(
                      logger.debug(
                        s"Enqueued for handling packet with message ${msg.typeId} " +
                          s"from ${msg.sender.endpoint.host} size of ${msg.contentLength}. Packet ${msg.path}. " +
                          s"Queue length = ${blobBuffer.getQueueLength}"
                      ),
                      metrics.setGauge("stream.buffer.length", blobBuffer.getQueueLength.toLong),
                      metrics.incrementCounter("enqueued.packets")
                    ).sequence,
                    List(
                      logger.debug(s"Dropped packet ${msg.path}"),
                      metrics.incrementCounter("dropped.packets"),
                      msg.path.deleteSingleFile[Task]
                    ).sequence
                  ) >> rPConfAsk.reader(c => ack(c.local))
          }
        }

        // TODO InternalServerError should take msg in constructor
        private def internalServerError(msg: String): TLResponse =
          TLResponse(
            TLResponse.Payload
              .InternalServerError(InternalServerError(ProtocolHelper.toProtocolBytes(msg)))
          )

        private def ack(src: PeerNode): TLResponse =
          TLResponse(
            TLResponse.Payload.Ack(Ack(Some(ProtocolHelper.header(src, networkId))))
          )
      }

      val server = NettyServerBuilder
        .forPort(port)
        .executor(ioScheduler)
        .maxMessageSize(maxMessageSize)
        .sslContext(serverSslContext)
        .addService(RoutingGrpcMonix.bindService(service, mainScheduler))
        .intercept(new SslSessionServerInterceptor(networkId))
        .build
        .start

      () => {
        server.shutdown().awaitTermination()
      }
    }
}
