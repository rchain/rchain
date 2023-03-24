package coop.rchain.comm.transport

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, ConcurrentEffect, Sync, Timer}
import cats.syntax.all._
import cats.effect.syntax.all._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.{CommMetricsSource, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream
import io.grpc.Metadata
import fs2.concurrent.Queue
import io.grpc.netty.NettyServerBuilder
import io.netty.handler.ssl.SslContext
import monix.execution.{Cancelable, Scheduler}

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.DurationInt

object GrpcTransportReceiver {

  implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  type MessageBuffers[F[_]]  = (Send => F[Boolean], StreamMessage => F[Boolean], Stream[F, Unit])
  type MessageHandlers[F[_]] = (Send => F[Unit], StreamMessage => F[Unit])

  def create[F[_]: Monixable: Concurrent: ConcurrentEffect: RPConfAsk: Log: Metrics: Timer](
      networkId: String,
      port: Int,
      serverSslContext: SslContext,
      maxMessageSize: Int,
      maxStreamMessageSize: Long,
      buffersMap: Ref[F, Map[PeerNode, Deferred[F, MessageBuffers[F]]]],
      messageHandlers: MessageHandlers[F],
      parallelism: Int,
      cache: TrieMap[String, Array[Byte]]
  )(implicit mainScheduler: Scheduler): F[Cancelable] = {

    val service = new TransportLayerFs2Grpc[F, Metadata] {

      private val circuitBreaker: StreamHandler.CircuitBreaker = streamed =>
        if (streamed.header.exists(_.networkId != networkId))
          Opened(StreamHandler.StreamError.wrongNetworkId)
        else if (streamed.readSoFar > maxStreamMessageSize)
          Opened(StreamHandler.StreamError.circuitOpened)
        else Closed

      private def getBuffers(peer: PeerNode): F[MessageBuffers[F]] = {
        def createBuffers(clear: F[Unit]): F[MessageBuffers[F]] =
          for {
            tellBuffer <- Queue.bounded[F, Send](64)
            blobBuffer <- Queue.bounded[F, StreamMessage](8)
            stream = tellBuffer
              .dequeueChunk(1)
              .parEvalMapUnordered(parallelism)(messageHandlers._1(_)) concurrently
              blobBuffer.dequeueChunk(1).parEvalMapUnordered(parallelism)(messageHandlers._2(_))
            // inbound queue lives for 10 minutes TODO synchronize with Kademlia table cleanup
            s = (Stream.fixedDelay(10.minutes) ++ Stream.eval(clear)) concurrently stream
            _ <- Concurrent[F]
                  .start(s.compile.drain)
                  .onError {
                    case err =>
                      Log[F].info(
                        s"Inbound gRPC channel with ${peer.toAddress} failed unexpectedly: $err"
                      )
                  }
                  .onCancel(
                    Log[F].info(
                      s"Inbound gRPC channel with ${peer.toAddress} closed because fiber has been cancelled."
                    )
                  )
          } yield (tellBuffer.offer1 _, blobBuffer.offer1 _, stream)

        for {
          bDefNew <- Deferred[F, MessageBuffers[F]]
          r <- buffersMap.modify[(Deferred[F, MessageBuffers[F]], Boolean)] { bMap =>
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
                newBufs <- createBuffers(buffersMap.update(x => x - peer))
                _       <- mbDef.complete(newBufs)
              } yield ()
              else ().pure[F]
          c <- mbDef.get
        } yield c
      }

      def send(request: TLRequest, c: Metadata): F[TLResponse] =
        for {
          _                <- Metrics[F].incrementCounter("packets.received")
          self             <- RPConfAsk[F].reader(_.local)
          peer             = PeerNode.from(request.protocol.header.sender)
          packetDroppedMsg = s"Packet dropped, ${peer.endpoint.host} packet queue overflown."
          targetBuffer     <- getBuffers(peer).map(_._1)
          r <- targetBuffer(Send(request.protocol)).ifM(
                Metrics[F].incrementCounter("packets.enqueued") >>
                  ack(self).pure[F],
                Metrics[F].incrementCounter("packets.dropped") >>
                  internalServerError(packetDroppedMsg).pure[F]
              )
        } yield r

      def stream(observable: Stream[F, Chunk], c: Metadata): F[TLResponse] = {
        import StreamHandler._
        import StreamError.StreamErrorToMessage

        handleStream(observable, circuitBreaker, cache) >>= {
          case Left(error @ StreamError.Unexpected(t)) =>
            Log[F].error(error.message, t).as(internalServerError(error.message))

          case Left(error) =>
            Log[F].warn(error.message).as(internalServerError(error.message))

          case Right(msg) =>
            val msgEnqueued =
              s"Stream chunk pushed to message buffer. Sender ${msg.sender.endpoint.host}, message ${msg.typeId}, " +
                s"size ${msg.contentLength}, file ${msg.key}."
            val msgDropped =
              s"Stream chunk dropped, ${msg.sender.endpoint.host} stream queue overflown."

            for {
              _            <- Metrics[F].incrementCounter("stream.chunks.received")
              self         <- RPConfAsk[F].reader(_.local)
              targetBuffer <- getBuffers(msg.sender).map(_._2)
              r <- targetBuffer(msg).ifM(
                    Metrics[F].incrementCounter("stream.chunks.enqueued") >>
                      Log[F].debug(msgEnqueued) >>
                      ack(self).pure[F],
                    Metrics[F].incrementCounter("stream.chunks.dropped") >>
                      Log[F].debug(msgDropped) >>
                      Sync[F].delay(cache.remove(msg.key)) >>
                      internalServerError(msgDropped).pure[F]
                  )
            } yield r
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
          TLResponse.Payload.Ack(Ack(ProtocolHelper.header(src, networkId)))
        )
    }

    val server = NettyServerBuilder
      .forPort(port)
      .executor(mainScheduler)
      .maxInboundMessageSize(maxMessageSize)
      .sslContext(serverSslContext)
      .addService(TransportLayerFs2Grpc.bindService(service))
      .intercept(new SslSessionServerInterceptor(networkId))
      .build
      .start

    Cancelable(() => server.shutdown().awaitTermination()).pure[F]
  }
}
