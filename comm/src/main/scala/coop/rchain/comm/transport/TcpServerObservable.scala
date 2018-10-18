package coop.rchain.comm.transport

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import coop.rchain.comm.PeerNode
import cats.implicits._
import coop.rchain.shared.{Compression, Log, LogSource}, Compression._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.CommError
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.catscontrib.ski._
import com.google.protobuf.ByteString
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
            content: Option[(Array[Byte], Int)] = None,
            decompressedLength: Option[Int] = None
        )

        object HeaderReceived {
          def unapply(chunk: Chunk): Option[(Option[PeerNode], String, Boolean, Int)] =
            chunk match {
              case Chunk(Chunk.Content.Header(ChunkHeader(sender, typeId, compressed, dcl))) =>
                Some(sender.map(ProtocolHelper.toPeerNode), typeId, compressed, dcl)
              case _ => None
            }
        }

        object FirstDataReceived {
          def unapply(temp: (PartialBlob, Chunk)): Option[(PartialBlob, Array[Byte], Int)] =
            temp match {
              case (
                  partial @ PartialBlob(_, _, None, Some(dcl)),
                  Chunk(Chunk.Content.Data(ChunkData(newData)))
                  ) =>
                Some((partial, newData.toByteArray, dcl))
              case _ => None
            }
        }

        object NextDataReceived {
          def unapply(
              temp: (PartialBlob, Chunk)
          ): Option[(PartialBlob, Array[Byte], Array[Byte], Int)] =
            temp match {
              case (
                  partial @ PartialBlob(_, _, Some((content, pointer)), _),
                  Chunk(Chunk.Content.Data(ChunkData(newData)))
                  ) =>
                Some(partial, content, newData.toByteArray, pointer)
              case _ => None
            }
        }

        /**
          * This is temporary solution.
          * In order to deal with arbitrary blog sizes, chunks must be stored on disk.
          * This is not implemented, thus temporaryly we do foldLef and gather partial data
          */
        def collect: Task[PartialBlob] = observable.foldLeftL(PartialBlob()) {
          case (_, HeaderReceived(sender, typeId, compressed, dcLength)) =>
            val dcl = if (compressed) Some(dcLength) else None
            PartialBlob(sender, Some(typeId), None, dcl)
          case FirstDataReceived(partial, firstData, dcl) =>
            val data = new Array[Byte](dcl)
            firstData.copyToArray(data)
            partial.copy(content = Some((data, data.length)))
          case NextDataReceived(partial, currentData, newData, pointer) =>
            newData.copyToArray(currentData, pointer)
            partial.copy(content = Some((currentData, pointer + newData.length)))
        }

        (collect >>= {
          case PartialBlob(Some(peerNode), Some(typeId), Some((content, _)), dcLength) =>
            Task.fromFuture(
              subjectBlobMessage.onNext(
                StreamMessage(
                  Blob(
                    peerNode,
                    Packet().withTypeId(typeId).withContent(toContent(content, dcLength))
                  )
                )
              )
            )
          case incorrect => logger.error(s"Streamed incorrect blob of data. Received $incorrect")
        }).as(ChunkResponse())
      }

      private def toContent(raw: Array[Byte], decompressLength: Option[Int]): ByteString = {
        val decompressed = decompressLength.map(raw.decompress(_)).getOrElse(raw)
        ProtocolHelper.toProtocolBytes(decompressed)
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
