package coop.rchain.comm.transport

import coop.rchain.shared._, Compression._
import coop.rchain.comm.{CommError, PeerNode}
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import monix.reactive.OverflowStrategy._
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.protocol.routing._
import com.google.protobuf.ByteString
import cats.implicits._

object StreamHandler {
  def handleStream(
      observable: Observable[Chunk],
      buff: buffer.LimitedBuffer[ServerMessage]
  )(implicit logger: Log[Task]): Task[ChunkResponse] = {

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
              buff.pushNext(StreamMessage(blob))
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

}
