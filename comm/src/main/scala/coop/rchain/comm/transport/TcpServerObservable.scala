package coop.rchain.comm.transport

import scala.concurrent.TimeoutException
import scala.concurrent.duration._

import cats.implicits._

import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.CommError
import coop.rchain.comm.rp.ProtocolHelper

import io.grpc.netty.NettyServerBuilder
import io.netty.handler.ssl.SslContext
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import monix.reactive.OverflowStrategy._
import monix.reactive.subjects.ConcurrentSubject

class TcpServerObservable(
    port: Int,
    serverSslContext: SslContext,
    maxMessageSize: Int,
    tellBufferSize: Int = 1024,
    askBufferSize: Int = 64,
    askTimeout: FiniteDuration = 5.second
) extends Observable[ServerMessage] {

  def unsafeSubscribeFn(subscriber: Subscriber[ServerMessage]): Cancelable = {
    implicit val scheduler: Scheduler = Scheduler.cached("tl-grpc", askBufferSize, Int.MaxValue)

    val subjectTell = ConcurrentSubject.publishToOne[ServerMessage](DropNew(tellBufferSize))
    val subjectAsk  = ConcurrentSubject.publishToOne[ServerMessage](DropNew(askBufferSize))
    val merged      = Observable.merge(subjectTell, subjectAsk)(BackPressure(10))

    val service = new RoutingGrpcMonix.TransportLayer {

      def tell(request: TLRequest): Task[TLResponse] =
        request.protocol
          .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
            Task.fromFuture(subjectTell.onNext(Tell(protocol)).map(_ => noResponse))
          }

      def ask(request: TLRequest): Task[TLResponse] =
        request.protocol
          .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
            val p = ReplyPromise(askTimeout)
            val result = for {
              _     <- Task.fromFuture(subjectAsk.onNext(Ask(protocol, p)))
              reply <- p.task
            } yield
              reply match {
                case NotHandled(error)            => internalServerError(error.message)
                case HandledWitoutMessage         => noResponse
                case HandledWithMessage(response) => returnProtocol(response)
              }

            result.onErrorRecover {
              case _: TimeoutException => internalServerError(CommError.timeout.message)
            }
          }

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
