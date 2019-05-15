package coop.rchain.comm.transport

import java.nio.file.Path

import scala.concurrent.duration._

import cats.implicits._

import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.shared._

import io.grpc.netty.NettyServerBuilder
import io.netty.handler.ssl.SslContext
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import monix.reactive.OverflowStrategy._

class TcpServerObservable(
    networkId: String,
    port: Int,
    serverSslContext: SslContext,
    maxMessageSize: Int,
    maxStreamMessageSize: Long,
    tellBufferSize: Int = 1024,
    blobBufferSize: Int = 32,
    askTimeout: FiniteDuration = 5.second,
    tempFolder: Path
)(
    implicit scheduler: Scheduler,
    rPConfAsk: RPConfAsk[Task],
    logger: Log[Task]
) extends Observable[ServerMessage] {

  def unsafeSubscribeFn(subscriber: Subscriber[ServerMessage]): Cancelable = {

    val bufferTell        = buffer.LimitedBufferObservable.dropNew[ServerMessage](tellBufferSize)
    val bufferBlobMessage = buffer.LimitedBufferObservable.dropNew[ServerMessage](blobBufferSize)
    val merged =
      Observable(bufferTell, bufferBlobMessage).mergeMap(identity)(BackPressure(10))

    val service = new RoutingGrpcMonix.TransportLayer {

      def send(request: TLRequest): Task[TLResponse] =
        request.protocol
          .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
            rPConfAsk.reader(_.local) >>= (
                src =>
                  Task.delay(bufferTell.pushNext(Send(protocol))).map {
                    case false => internalServerError("message dropped")
                    case true  => noResponse(src)
                  }
              )
          }

      def stream(observable: Observable[Chunk]): Task[ChunkResponse] = {
        import StreamHandler._
        import StreamError.StreamErrorToMessage

        val circuitBreaker: StreamHandler.CircuitBreaker = _ > maxStreamMessageSize

        (handleStream(networkId, tempFolder, observable, circuitBreaker) >>= {
          case Left(error @ StreamError.Unexpected(t)) => logger.error(error.message, t)
          case Left(error)                             => logger.warn(error.message)
          case Right(msg)                              => Task.delay(bufferBlobMessage.pushNext(msg)).as(())
        }).as(ChunkResponse())
      }

      // TODO InternalServerError should take msg in constructor
      private def internalServerError(msg: String): TLResponse =
        TLResponse(
          TLResponse.Payload
            .InternalServerError(InternalServerError(ProtocolHelper.toProtocolBytes(msg)))
        )

      private def noResponse(src: PeerNode): TLResponse =
        TLResponse(
          TLResponse.Payload.NoResponse(NoResponse(Some(ProtocolHelper.header(src, networkId))))
        )
    }

    val mergeSubscription = merged.subscribe(subscriber)

    val server = NettyServerBuilder
      .forPort(port)
      .executor(scheduler)
      .maxMessageSize(maxMessageSize)
      .sslContext(serverSslContext)
      .addService(RoutingGrpcMonix.bindService(service, scheduler))
      .intercept(new SslSessionServerInterceptor(networkId))
      .build
      .start

    () => {
      server.shutdown().awaitTermination()
      mergeSubscription.cancel()
    }
  }
}
