package coop.rchain.comm.transport

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import coop.rchain.comm.PeerNode
import cats.implicits._
import coop.rchain.shared.{Log, LogSource}
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
    askBufferSize: Int = 1024,
    blobBufferSize: Int = 32,
    askTimeout: FiniteDuration = 5.second
)(implicit scheduler: Scheduler, logger: Log[Task])
    extends Observable[ServerMessage] {

  def unsafeSubscribeFn(subscriber: Subscriber[ServerMessage]): Cancelable = {

    implicit val logSource: LogSource = LogSource(this.getClass)

    val subjectTell        = ConcurrentSubject.publishToOne[ServerMessage](DropNew(tellBufferSize))
    val subjectAsk         = ConcurrentSubject.publishToOne[ServerMessage](DropNew(askBufferSize))
    val subjectBlobMessage = ConcurrentSubject.publishToOne[ServerMessage](DropNew(blobBufferSize))
    val merged             = Observable.merge(subjectTell, subjectAsk, subjectBlobMessage)(BackPressure(10))

    val service = new RoutingGrpcMonix.TransportLayer {

      def tell(request: TLRequest): Task[TLResponse] =
        request.protocol
          .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
            Task.delay {
              subjectTell.onNext(Tell(protocol))
              noResponse
            }
          }

      def ask(request: TLRequest): Task[TLResponse] =
        request.protocol
          .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
            Task
              .create[CommunicationResponse] { (s, cb) =>
                val reply = Reply(cb)
                subjectAsk.onNext(Ask(protocol, reply))
                s.scheduleOnce(askTimeout)(reply.failWith(new TimeoutException))
                Cancelable.empty
              }
              .map {
                case NotHandled(error)            => internalServerError(error.message)
                case HandledWitoutMessage         => noResponse
                case HandledWithMessage(response) => returnProtocol(response)
              }
              .onErrorRecover {
                case _: TimeoutException => internalServerError(CommError.timeout.message)
              }
          }

      def stream(observable: Observable[Chunk]): Task[ChunkResponse] = {
        case class PartialBlob(
            peerNode: Option[PeerNode] = None,
            typeId: Option[String] = None,
            content: Option[Array[Byte]] = None
        )
        def collect: Task[PartialBlob] = observable.foldLeftL(PartialBlob()) {
          case (
              PartialBlob(_, _, content),
              Chunk(Chunk.Content.Header(ChunkHeader(sender, typeId)))
              ) =>
            PartialBlob(sender.map(ProtocolHelper.toPeerNode), Some(typeId), content)
          case (
              PartialBlob(sender, typeId, None),
              Chunk(Chunk.Content.Data(ChunkData(newData)))
              ) =>
            PartialBlob(sender, typeId, Some(newData.toByteArray))
          case (
              PartialBlob(sender, typeId, Some(content)),
              Chunk(Chunk.Content.Data(ChunkData(newData)))
              ) =>
            PartialBlob(sender, typeId, Some(content ++ newData.toByteArray))
        }

        (collect >>= {
          case PartialBlob(Some(peerNode), Some(typeId), Some(content)) =>
            Task.fromFuture(
              subjectBlobMessage.onNext(
                StreamMessage(
                  Blob(
                    peerNode,
                    Packet().withTypeId(typeId).withContent(ProtocolHelper.toProtocolBytes(content))
                  )
                )
              )
            )
          case incorrect => logger.error(s"Streamed incorrect blob of data. Received $incorrect")
        }).as(ChunkResponse())
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
