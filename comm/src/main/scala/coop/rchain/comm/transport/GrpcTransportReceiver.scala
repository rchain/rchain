package coop.rchain.comm.transport

import java.nio.file.Path

import cats.Monad
import cats.effect.concurrent.{Deferred, Ref}

import scala.concurrent.duration._
import cats.implicits._
import coop.rchain.comm.{CommMetricsSource, PeerNode}
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.transport.GrpcTransportReceiver.MessageBuffers
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

  type MessageBuffers  = (LimitedBuffer[Send], LimitedBuffer[StreamMessage], Cancelable)
  type MessageHandlers = (Send => Task[Unit], StreamMessage => Task[Unit])

  def create(
      networkId: String,
      port: Int,
      serverSslContext: SslContext,
      maxMessageSize: Int,
      maxStreamMessageSize: Long,
      buffersMap: Ref[Task, Map[PeerNode, Deferred[Task, MessageBuffers]]],
      messageHandlers: MessageHandlers,
      tempFolder: Path,
      parallelism: Int
  )(
      implicit scheduler: Scheduler,
      rPConfAsk: RPConfAsk[Task],
      log: Log[Task],
      metrics: Metrics[Task]
  ): Task[Cancelable] = {

    val service = new RoutingGrpcMonix.TransportLayer {

      private val queueScheduler =
        Scheduler.fixedPool(
          "tl-dispatcher-server-queue",
          parallelism,
          reporter = UncaughtExceptionLogger
        )

      private val circuitBreaker: StreamHandler.CircuitBreaker = streamed =>
        if (streamed.header.exists(_.networkId != networkId))
          Opened(StreamHandler.StreamError.wrongNetworkId)
        else if (streamed.readSoFar > maxStreamMessageSize)
          Opened(StreamHandler.StreamError.circuitOpened)
        else Closed

      private def getBuffers(peer: PeerNode): Task[MessageBuffers] = {
        def createBuffers: MessageBuffers = {
          val tellBuffer = buffer.LimitedBufferObservable.dropNew[Send](64)
          val blobBuffer = buffer.LimitedBufferObservable.dropNew[StreamMessage](8)
          // TODO cancel queues when peer is lost
          val tellCancellable = tellBuffer
            .mapParallelUnordered(parallelism)(messageHandlers._1)
            .subscribe()(queueScheduler)
          val blobCancellable = blobBuffer
            .mapParallelUnordered(parallelism)(messageHandlers._2)
            .subscribe()(queueScheduler)
          val buffersCancellable = Cancelable.collection(tellCancellable, blobCancellable)

          (tellBuffer, blobBuffer, buffersCancellable)
        }

        for {
          bDefNew <- Deferred[Task, MessageBuffers]
          r <- buffersMap.modify[(Deferred[Task, MessageBuffers], Boolean)] { bMap =>
                val noBuffer = !bMap.exists(c => c._1 equals peer)
                if (noBuffer) {
                  (bMap + (peer -> bDefNew), (bDefNew, true))
                } else {
                  (bMap, (bMap(peer), false))
                }
              }
          (mbDef, isNewPeer) = r
          _ <- if (isNewPeer) for {
                _       <- log.info(s"Creating inbound message queue for ${peer.toAddress}.")
                newBufs <- Task.delay(createBuffers)
                _       <- mbDef.complete(newBufs)
              } yield ()
              else ().pure[Task]
          c <- mbDef.get
        } yield c
      }

      def send(request: TLRequest): Task[TLResponse] =
        request.protocol match {
          case None => internalServerError("Bad TLRequest received").pure[Task]
          case Some(p) =>
            for {
              _                <- metrics.incrementCounter("packets.received")
              self             <- rPConfAsk.reader(_.local)
              peer             = PeerNode.from(p.header.get.sender.get)
              packetDroppedMsg = s"Packet dropped, ${peer.endpoint.host} packet queue overflown."
              targetBuffer     <- getBuffers(peer).map(_._1)
              r <- if (targetBuffer.pushNext(Send(p)))
                    metrics.incrementCounter("packets.enqueued") >>
                      ack(self).pure[Task]
                  else
                    metrics.incrementCounter("packets.dropped") >>
                      internalServerError(packetDroppedMsg).pure[Task]
            } yield r
        }

      def stream(observable: Observable[Chunk]): Task[TLResponse] = {
        import StreamHandler._
        import StreamError.StreamErrorToMessage

        handleStream(tempFolder, observable, circuitBreaker) >>= {
          case Left(error @ StreamError.Unexpected(t)) =>
            log.error(error.message, t).as(internalServerError(error.message))

          case Left(error) =>
            log.warn(error.message).as(internalServerError(error.message))

          case Right(msg) => {
            val msgEnqueued =
              s"Stream chunk pushed to message buffer. Sender ${msg.sender.endpoint.host}, message ${msg.typeId}, " +
                s"size ${msg.contentLength}, file ${msg.path}."
            val msgDropped =
              s"Stream chunk dropped, ${msg.sender.endpoint.host} stream queue overflown."

            for {
              _            <- metrics.incrementCounter("stream.chunks.received")
              self         <- rPConfAsk.reader(_.local)
              targetBuffer <- getBuffers(msg.sender).map(_._2)
              r <- if (targetBuffer.pushNext(msg))
                    metrics.incrementCounter("stream.chunks.enqueued") >>
                      log.debug(msgEnqueued) >>
                      ack(self).pure[Task]
                  else
                    metrics.incrementCounter("stream.chunks.dropped") >>
                      log.debug(msgDropped) >>
                      msg.path.deleteSingleFile[Task] >>
                      internalServerError(msgDropped).pure[Task]
            } yield r
          }
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
      .executor(scheduler)
      .maxInboundMessageSize(maxMessageSize)
      .sslContext(serverSslContext)
      .addService(RoutingGrpcMonix.bindService(service, scheduler))
      .intercept(new SslSessionServerInterceptor(networkId))
      .build
      .start

    Cancelable(() => server.shutdown().awaitTermination()).pure[Task]
  }
}
