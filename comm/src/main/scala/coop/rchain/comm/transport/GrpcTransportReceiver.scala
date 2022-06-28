package coop.rchain.comm.transport

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.transport.buffer.LimitedBuffer
import coop.rchain.comm.{CommMetricsSource, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import io.grpc.netty.NettyServerBuilder
import io.netty.handler.ssl.SslContext
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable

import scala.collection.concurrent.TrieMap

object GrpcTransportReceiver {

  implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  type MessageBuffers        = (LimitedBuffer[Send], LimitedBuffer[StreamMessage], Cancelable)
  type MessageHandlers[F[_]] = (Send => F[Unit], StreamMessage => F[Unit])

  def create[F[_]: Monixable: Concurrent: RPConfAsk: Log: Metrics](
      networkId: String,
      port: Int,
      serverSslContext: SslContext,
      maxMessageSize: Int,
      maxStreamMessageSize: Long,
      buffersMap: Ref[F, Map[PeerNode, Deferred[F, MessageBuffers]]],
      messageHandlers: MessageHandlers[F],
      parallelism: Int,
      cache: TrieMap[String, Array[Byte]]
  )(implicit mainScheduler: Scheduler): F[Cancelable] = {

    val service = new RoutingGrpcMonix.TransportLayer {

      private val circuitBreaker: StreamHandler.CircuitBreaker = streamed =>
        if (streamed.header.exists(_.networkId != networkId))
          Opened(StreamHandler.StreamError.wrongNetworkId)
        else if (streamed.readSoFar > maxStreamMessageSize)
          Opened(StreamHandler.StreamError.circuitOpened)
        else Closed

      private def getBuffers(peer: PeerNode): F[MessageBuffers] = {
        def createBuffers: MessageBuffers = {
          val tellBuffer = buffer.LimitedBufferObservable.dropNew[Send](64)
          val blobBuffer = buffer.LimitedBufferObservable.dropNew[StreamMessage](8)
          // TODO cancel queues when peer is lost
          val tellCancellable = tellBuffer
            .mapParallelUnordered(parallelism)(messageHandlers._1(_).toTask)
            .subscribe()(mainScheduler)
          val blobCancellable = blobBuffer
            .mapParallelUnordered(parallelism)(messageHandlers._2(_).toTask)
            .subscribe()(mainScheduler)
          val buffersCancellable = Cancelable.collection(tellCancellable, blobCancellable)

          (tellBuffer, blobBuffer, buffersCancellable)
        }

        for {
          bDefNew <- Deferred[F, MessageBuffers]
          r <- buffersMap.modify[(Deferred[F, MessageBuffers], Boolean)] { bMap =>
                val noBuffer = !bMap.exists(c => c._1 equals peer)
                if (noBuffer) {
                  (bMap + (peer -> bDefNew), (bDefNew, true))
                } else {
                  (bMap, (bMap(peer), false))
                }
              }
          (mbDef, isNewPeer) = r
          _ <- if (isNewPeer) for {
                _       <- Log[F].info(s"Creating inbound message queue for ${peer.toAddress}.")
                newBufs <- Sync[F].delay(createBuffers)
                _       <- mbDef.complete(newBufs)
              } yield ()
              else ().pure[F]
          c <- mbDef.get
        } yield c
      }

      def send(request: TLRequest): Task[TLResponse] =
        (for {
          _                <- Metrics[F].incrementCounter("packets.received")
          self             <- RPConfAsk[F].reader(_.local)
          peer             = PeerNode.from(request.protocol.header.sender)
          packetDroppedMsg = s"Packet dropped, ${peer.endpoint.host} packet queue overflown."
          targetBuffer     <- getBuffers(peer).map(_._1)
          r <- if (targetBuffer.pushNext(Send(request.protocol)))
                Metrics[F].incrementCounter("packets.enqueued") >>
                  ack(self).pure[F]
              else
                Metrics[F].incrementCounter("packets.dropped") >>
                  internalServerError(packetDroppedMsg).pure[F]
        } yield r).toTask

      def stream(observable: Observable[Chunk]): Task[TLResponse] = {
        import StreamHandler._
        import StreamError.StreamErrorToMessage

        val result = handleStream(observable, circuitBreaker, cache) >>= {
          case Left(error @ StreamError.Unexpected(t)) =>
            Log[F].error(error.message, t).as(internalServerError(error.message))

          case Left(error) =>
            Log[F].warn(error.message).as(internalServerError(error.message))

          case Right(msg) => {
            val msgEnqueued =
              s"Stream chunk pushed to message buffer. Sender ${msg.sender.endpoint.host}, message ${msg.typeId}, " +
                s"size ${msg.contentLength}, file ${msg.key}."
            val msgDropped =
              s"Stream chunk dropped, ${msg.sender.endpoint.host} stream queue overflown."

            for {
              _            <- Metrics[F].incrementCounter("stream.chunks.received")
              self         <- RPConfAsk[F].reader(_.local)
              targetBuffer <- getBuffers(msg.sender).map(_._2)
              r <- if (targetBuffer.pushNext(msg))
                    Metrics[F].incrementCounter("stream.chunks.enqueued") >>
                      Log[F].debug(msgEnqueued) >>
                      ack(self).pure[F]
                  else
                    Metrics[F].incrementCounter("stream.chunks.dropped") >>
                      Log[F].debug(msgDropped) >>
                      Sync[F].delay(cache.remove(msg.key)) >>
                      internalServerError(msgDropped).pure[F]
            } yield r
          }
        }
        result.toTask
      }

      // TODO InternalServerError should take msg in constructor
      private def internalServerError(msg: String): TLResponse =
        TLResponse(
          TLResponse.Payload
            .InternalServerError(InternalServerError(ProtocolHelper.toProtocolBytes(msg)))
        )

      private def ack(src: PeerNode): TLResponse =
        TLResponse(
          TLResponse.Payload.Ack(Ack(ProtocolHelper.header(src, networkId)))
        )
    }

    val server = NettyServerBuilder
      .forPort(port)
      .executor(mainScheduler)
      .maxInboundMessageSize(maxMessageSize)
      .sslContext(serverSslContext)
      .addService(RoutingGrpcMonix.bindService(service, mainScheduler))
      .intercept(new SslSessionServerInterceptor(networkId))
      .build
      .start

    Cancelable(() => server.shutdown().awaitTermination()).pure[F]
  }
}
