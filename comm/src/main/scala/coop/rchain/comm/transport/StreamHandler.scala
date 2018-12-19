package coop.rchain.comm.transport

import java.util.UUID
import coop.rchain.shared.GracefulClose._
import coop.rchain.catscontrib.ski._
import java.io.FileOutputStream
import java.nio.file.{Files, Path}
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
import coop.rchain.catscontrib.TaskContrib._

object StreamHandler {

  private case class Streamed(
      sender: Option[PeerNode] = None,
      typeId: Option[String] = None,
      contentLength: Option[Int] = None,
      compressed: Boolean = false,
      path: Path,
      fos: FileOutputStream
  )

  def handleStream(
      folder: Path,
      observable: Observable[Chunk],
      buff: buffer.LimitedBuffer[ServerMessage]
  )(implicit logger: Log[Task]): Task[ChunkResponse] =
    (init(folder).attempt >>= {
      case Left(ex) => logger.error("could not create a file to store incoming stream", ex)
      case Right(initStmd) =>
        (collect(initStmd, observable).attempt >>= {
          case Left(ex)    => logger.error("could not collect incoming streamed data", ex)
          case Right(stmd) => push(stmd, buff)
        }) *> gracefullyClose[Task](initStmd.fos).as(())
    }).as(ChunkResponse())

  private def init(folder: Path): Task[Streamed] =
    for {
      _        <- Task.delay(folder.toFile.mkdirs())
      fileName <- Task.delay(UUID.randomUUID.toString + "_packet_streamed.bts")
      file     = folder.resolve(fileName)
      fos      <- Task.delay(new FileOutputStream(file.toFile))
    } yield Streamed(fos = fos, path = file)

  private def collect(init: Streamed, observable: Observable[Chunk]): Task[Streamed] =
    observable.foldLeftL(init) {
      case (stmd, Chunk(Chunk.Content.Header(ChunkHeader(sender, typeId, compressed, cl)))) =>
        stmd.copy(
          sender = sender.map(ProtocolHelper.toPeerNode(_)),
          typeId = Some(typeId),
          compressed = compressed,
          contentLength = Some(cl)
        )
      case (stmd, Chunk(Chunk.Content.Data(ChunkData(newData)))) =>
        stmd.fos.write(newData.toByteArray)
        stmd.fos.flush()
        stmd
    }

  private def push(stmd: Streamed, buff: buffer.LimitedBuffer[ServerMessage])(
      implicit logger: Log[Task]
  ): Task[Boolean] = stmd match {
    case Streamed(Some(sender), Some(packetType), Some(contentLength), compressed, path, _) =>
      Task.delay {
        // TODO what if returns false?
        buff.pushNext(StreamMessage(sender, packetType, path, compressed, contentLength))
      }
    case stmd =>
      logger
        .warn(
          s"received not full stream message, will not process. $stmd"
        )
        .as(false)
  }

  def restore(msg: StreamMessage)(implicit logger: Log[Task]): Task[Either[Throwable, Blob]] =
    (fetchContent(msg.path).attempt >>= {
      case Left(ex) => logger.error("Could not read streamed data from file", ex).as(Left(ex))
      case Right(content) =>
        decompressContent(content, msg.compressed, msg.contentLength).attempt >>= {
          case Left(ex) => logger.error("Could not decompressed data ").as(Left(ex))
          case Right(decompressedContent) =>
            Right(ProtocolHelper.blob(msg.sender, msg.typeId, decompressedContent)).pure[Task]
        }
    }) >>= (
        res =>
          deleteFile(msg.path).flatMap {
            case Left(ex) => logger.error(s"Was unable to delete file ${msg.sender}", ex).as(res)
            case Right(_) => res.pure[Task]
          }
      )

  private def fetchContent(path: Path): Task[Array[Byte]] = Task.delay(Files.readAllBytes(path))
  private def decompressContent(
      raw: Array[Byte],
      compressed: Boolean,
      contentLength: Int
  ): Task[Array[Byte]] =
    if (compressed) {
      raw
        .decompress(contentLength)
        .fold(Task.raiseError[Array[Byte]](new RuntimeException("Could not decompress data")))(
          _.pure[Task]
        )
    } else raw.pure[Task]

  private def deleteFile(path: Path): Task[Either[Throwable, Unit]] =
    Task.delay(path.toFile.delete).as(()).attempt
}
