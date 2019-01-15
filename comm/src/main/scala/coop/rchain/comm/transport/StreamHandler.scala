package coop.rchain.comm.transport

import java.util.UUID
import coop.rchain.shared.Log
import coop.rchain.shared.GracefulClose._
import coop.rchain.shared.PathOps._
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
import cats._, cats.data._, cats.implicits._
import cats.effect._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.transport.PacketOps._

object StreamHandler {

  type CircuitBreaker = Long => Boolean

  private case class Streamed(
      sender: Option[PeerNode] = None,
      typeId: Option[String] = None,
      contentLength: Option[Int] = None,
      compressed: Boolean = false,
      readSoFar: Long = 0,
      circuitBroken: Boolean = false,
      path: Path,
      fos: FileOutputStream
  )

  def handleStream(
      folder: Path,
      stream: Observable[Chunk],
      circuitBreaker: CircuitBreaker
  )(implicit log: Log[Task]): Task[Either[Throwable, StreamMessage]] =
    (init(folder)
      .bracket { initStmd =>
        (collect(initStmd, stream, circuitBreaker) >>= toResult).value
      }(stmd => gracefullyClose[Task](stmd.fos).as(())))
      .attempt
      .map(_.flatten)

  private def init(folder: Path): Task[Streamed] =
    for {
      packetFile <- createPacketFile[Task](folder, "_packet_streamed.bts")
      file       = packetFile.file
      fos        = packetFile.fos
    } yield Streamed(fos = fos, path = file)

  private def collect(
      init: Streamed,
      stream: Observable[Chunk],
      circuitBreaker: CircuitBreaker
  )(implicit log: Log[Task]): EitherT[Task, Throwable, Streamed] = {

    def collectStream = stream.foldWhileLeftL(init) {
      case (stmd, Chunk(Chunk.Content.Header(ChunkHeader(sender, typeId, compressed, cl)))) =>
        Left(
          stmd.copy(
            sender = sender.map(ProtocolHelper.toPeerNode(_)),
            typeId = Some(typeId),
            compressed = compressed,
            contentLength = Some(cl)
          )
        )
      case (stmd, Chunk(Chunk.Content.Data(ChunkData(newData)))) =>
        val array = newData.toByteArray
        stmd.fos.write(array)
        stmd.fos.flush()
        val readSoFar = stmd.readSoFar + array.length
        if (circuitBreaker(readSoFar))
          Right(stmd.copy(circuitBroken = true))
        else
          Left(stmd.copy(readSoFar = readSoFar))
    }

    EitherT(collectStream.attempt >>= {
      case Right(stmd) if stmd.circuitBroken =>
        stmd.path.deleteSingleFile[Task].as(Left(new RuntimeException("Circuit was broken")))
      case res @ Left(ex) => init.path.deleteSingleFile[Task].as(res)
      case res            => res.pure[Task]
    })

  }

  private def toResult(
      stmd: Streamed
  )(implicit logger: Log[Task]): EitherT[Task, Throwable, StreamMessage] = {
    val notFullError = stmd.path
      .deleteSingleFile[Task]
      .as(
        new RuntimeException(s"received not full stream message, will not process. $stmd")
          .asLeft[StreamMessage]
      )

    EitherT(
      stmd match {
        case Streamed(
            Some(sender),
            Some(packetType),
            Some(contentLength),
            compressed,
            readSoFar,
            _,
            path,
            _
            ) =>
          if (readSoFar == contentLength)
            Right(StreamMessage(sender, packetType, path, compressed, contentLength)).pure[Task]
          else notFullError
        case stmd => notFullError
      }
    )
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
