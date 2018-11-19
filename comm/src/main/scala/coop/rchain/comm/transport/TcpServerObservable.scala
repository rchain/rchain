package coop.rchain.comm.transport

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.control.NonFatal

import cats.implicits._

import coop.rchain.shared._
import Compression._
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
    askTimeout: FiniteDuration = 5.second
)(implicit scheduler: Scheduler, logger: Log[Task])
    extends Observable[ServerMessage] {

  def unsafeSubscribeFn(subscriber: Subscriber[ServerMessage]): Cancelable = {

    implicit val logSource: LogSource = LogSource(this.getClass)

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

      def stream(observable: Observable[Chunk]): Task[ChunkResponse] = {
        case class PartialBlob(
            peerNode: Option[PeerNode] = None,
            typeId: Option[String] = None,
            content: Option[(Array[Byte], Int)] = None,
            contentLength: Option[Int] = None,
            compressed: Boolean = false
        )

        object HeaderReceived {
          def unapply(chunk: Chunk): Option[(Option[PeerNode], String, Boolean, Int)] =
            chunk match {
              case Chunk(Chunk.Content.Header(ChunkHeader(sender, typeId, compressed, cl))) =>
                Some(sender.map(ProtocolHelper.toPeerNode), typeId, compressed, cl)
              case _ => None
            }
        }

        object FirstDataReceived {
          def unapply(temp: (PartialBlob, Chunk)): Option[(PartialBlob, Array[Byte], Int)] =
            temp match {
              case (
                  partial @ PartialBlob(_, _, None, Some(cl), _),
                  Chunk(Chunk.Content.Data(ChunkData(newData)))
                  ) =>
                Some((partial, newData.toByteArray, cl))
              case _ => None
            }
        }

        object NextDataReceived {
          def unapply(
              temp: (PartialBlob, Chunk)
          ): Option[(PartialBlob, Array[Byte], Array[Byte], Int)] =
            temp match {
              case (
                  partial @ PartialBlob(_, _, Some((content, pointer)), _, _),
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
          case (_, HeaderReceived(sender, typeId, compressed, contentLength)) =>
            PartialBlob(sender, Some(typeId), None, Some(contentLength), compressed)
          case FirstDataReceived(partial, firstData, contentLength) =>
            val data = new Array[Byte](contentLength)
            firstData.copyToArray(data)
            partial.copy(content = Some((data, data.length)))
          case NextDataReceived(partial, currentData, newData, pointer) =>
            newData.copyToArray(currentData, pointer)
            partial.copy(content = Some((currentData, pointer + newData.length)))
        }

        (collect >>= {
          case PartialBlob(
              Some(peerNode),
              Some(typeId),
              Some((content, _)),
              Some(contentLength),
              compressed
              ) =>
            toContent(content, compressed, contentLength).attempt >>= {
              case Left(th) => logger.error(th.getMessage)
              case Right(content) =>
                Task.delay {
                  val packet = Packet()
                    .withTypeId(typeId)
                    .withContent(content)
                  val blob = Blob(peerNode, packet)
                  bufferBlobMessage.pushNext(StreamMessage(blob))
                }
            }
          case incorrect => logger.error(s"Streamed incorrect blob of data. Received $incorrect")
        }).as(ChunkResponse())
      }

      private def toContent(
          raw: Array[Byte],
          compressed: Boolean,
          contentLength: Int
      ): Task[ByteString] = {
        val decompressed: Task[Array[Byte]] = if (compressed) {
          raw
            .decompress(contentLength)
            .fold(Task.raiseError[Array[Byte]](new RuntimeException("Could not decompress data")))(
              _.pure[Task]
            )
        } else raw.pure[Task]
        decompressed map ProtocolHelper.toProtocolBytes
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
