package coop.rchain.comm.transport

import java.nio.file.Path
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.control.NonFatal

import cats.implicits._

import coop.rchain.shared._, Compression._
import coop.rchain.comm.{CommError, PeerNode}
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper

import com.google.protobuf.ByteString
import io.grpc.netty.NettyServerBuilder
import io.netty.handler.ssl.SslContext
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import monix.reactive.OverflowStrategy._

class TcpServerObservable(
    port: Int,
    serverSslContext: SslContext,
    maxMessageSize: Int,
    tellBufferSize: Int = 1024,
    askBufferSize: Int = 1024,
    blobBufferSize: Int = 32,
    askTimeout: FiniteDuration = 5.second,
    tempFolder: Path
)(implicit scheduler: Scheduler, logger: Log[Task])
    extends Observable[ServerMessage] {

  def unsafeSubscribeFn(subscriber: Subscriber[ServerMessage]): Cancelable = {

    val bufferTell        = buffer.LimitedBufferObservable.dropNew[ServerMessage](tellBufferSize)
    val bufferAsk         = buffer.LimitedBufferObservable.dropNew[ServerMessage](askBufferSize)
    val bufferBlobMessage = buffer.LimitedBufferObservable.dropNew[ServerMessage](blobBufferSize)
    val merged =
      Observable(bufferTell, bufferAsk, bufferBlobMessage).mergeMap(identity)(BackPressure(10))

    val service = new RoutingGrpcMonix.TransportLayer {

      def tell(request: TLRequest): Task[TLResponse] =
        request.protocol
          .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
            Task.delay(bufferTell.pushNext(Tell(protocol))).map {
              case false => internalServerError("message dropped")
              case true  => noResponse
            }
          }

      def ask(request: TLRequest): Task[TLResponse] =
        request.protocol
          .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
            Task
              .create[CommunicationResponse] { (s, cb) =>
                val reply = Reply(cb)
                s.scheduleOnce(askTimeout)(reply.failWith(new TimeoutException))
                bufferAsk.pushNext(Ask(protocol, reply)) match {
                  case false => reply.failWith(new RuntimeException("message dropped"))
                  case true  =>
                }
                Cancelable.empty
              }
              .map {
                case NotHandled(error)            => internalServerError(error.message)
                case HandledWitoutMessage         => noResponse
                case HandledWithMessage(response) => returnProtocol(response)
              }
              .onErrorRecover {
                case _: TimeoutException => internalServerError(CommError.timeout.message)
                case NonFatal(ex)        => internalServerError(ex.getMessage)
              }
          }

      def stream(observable: Observable[Chunk]): Task[ChunkResponse] =
        (StreamHandler.handleStream(tempFolder, observable) >>= {
          case Left(ex)   => logger.error("Could not receive stream! Details: ${ex.getMessage}", ex)
          case Right(msg) => Task.delay(bufferBlobMessage.pushNext(msg))
        }).as(ChunkResponse())

      private def returnProtocol(protocol: Protocol): TLResponse =
        TLResponse(TLResponse.Payload.Protocol(protocol))

      // TODO InternalServerError should take msg in constructor
      private def internalServerError(msg: String): TLResponse =
        TLResponse(
          TLResponse.Payload
            .InternalServerError(InternalServerError(ProtocolHelper.toProtocolBytes(msg)))
        )

      private def noResponse: TLResponse =
        TLResponse(TLResponse.Payload.NoResponse(NoResponse()))
    }

    val mergeSubscription = merged.subscribe(subscriber)

    val server = NettyServerBuilder
      .forPort(port)
      .executor(scheduler)
      .maxMessageSize(maxMessageSize)
      .sslContext(serverSslContext)
      .addService(RoutingGrpcMonix.bindService(service, scheduler))
      .intercept(new SslSessionServerInterceptor())
      .build
      .start

    () => {
      server.shutdown().awaitTermination()
      mergeSubscription.cancel()
    }
  }
}
