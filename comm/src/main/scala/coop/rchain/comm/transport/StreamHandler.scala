package coop.rchain.comm.transport

import java.io.FileOutputStream
import java.nio.file.{Files, Path}

import cats.data._
import cats.implicits._

import coop.rchain.shared.{Log, _}
import coop.rchain.shared.GracefulClose._
import coop.rchain.shared.PathOps._
import Compression._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.transport.PacketOps._

import monix.eval.Task
import monix.reactive.Observable

final case class Header(
    sender: PeerNode,
    typeId: String,
    contentLength: Int,
    networkId: String,
    compressed: Boolean
)

sealed trait Circuit {
  val broken: Boolean
}
final case class Opened(error: StreamHandler.StreamError) extends Circuit {
  val broken: Boolean = true
}
final case object Closed extends Circuit {
  val broken: Boolean = false
}

final case class Streamed(
    header: Option[Header] = None,
    readSoFar: Long = 0,
    circuit: Circuit = Closed,
    path: Path,
    fos: FileOutputStream
)

object StreamHandler {

  type CircuitBreaker = Streamed => Circuit

  sealed trait StreamError
  object StreamError {
    type StreamErr[A] = Either[StreamError, A]

    case object WrongNetworkId                        extends StreamError
    case object MaxSizeReached                        extends StreamError
    final case class NotFullMessage(streamed: String) extends StreamError
    final case class Unexpected(error: Throwable)     extends StreamError

    val wrongNetworkId: StreamError                   = WrongNetworkId
    val circuitOpened: StreamError                    = MaxSizeReached
    def notFullMessage(streamed: String): StreamError = NotFullMessage(streamed)
    def unexpected(error: Throwable): StreamError     = Unexpected(error)

    def errorMessage(error: StreamError): String =
      error match {
        case WrongNetworkId => "Could not receive stream! Wrong network id."
        case MaxSizeReached => "Max message size was reached."
        case NotFullMessage(streamed) =>
          s"Received not full stream message, will not process. $streamed"
        case Unexpected(t) => s"Could not receive stream! ${t.getMessage}"
      }

    implicit class StreamErrorToMessage(error: StreamError) {
      val message: String = StreamError.errorMessage(error)
    }
  }

  def handleStream(
      folder: Path,
      stream: Observable[Chunk],
      circuitBreaker: CircuitBreaker
  )(implicit log: Log[Task]): Task[Either[StreamError, StreamMessage]] =
    init(folder)
      .bracketE { initStmd =>
        (collect(initStmd, stream, circuitBreaker) >>= toResult).value
      }({
        // failed while collecting stream
        case (stmd, Right(Left(_))) =>
          Log[Task].warn("Failed collecting stream.") >>
            gracefullyClose[Task](stmd.fos).as(()) >>
            stmd.path.deleteSingleFile[Task]
        // should not happend (errors handled witin bracket) but covered for safety
        case (stmd, Left(_)) =>
          Log[Task].error(
            "Stream collection ended unexpected way. Please contact RNode code maintainer."
          ) >>
            gracefullyClose[Task](stmd.fos).as(()) >>
            stmd.path.deleteSingleFile[Task]
        // succesfully collected
        case (stmd, _) =>
          Log[Task].debug("Stream collected.") >>
            gracefullyClose[Task](stmd.fos).as(())
      })
      .attempt
      .map(_.leftMap(StreamError.unexpected).flatten)

  private def init(folder: Path): Task[Streamed] =
    createPacketFile[Task](folder, "_packet_streamed.bts")
      .map { case (file, fos) => Streamed(fos = fos, path = file) }

  private def collect(
      init: Streamed,
      stream: Observable[Chunk],
      circuitBreaker: CircuitBreaker
  ): EitherT[Task, StreamError, Streamed] = {

    def collectStream: Task[Streamed] =
      stream.foldWhileLeftL(init) {
        case (
            stmd,
            Chunk(
              Chunk.Content
                .Header(ChunkHeader(Some(sender), typeId, compressed, cl, nid))
            )
            ) =>
          val newStmd = stmd.copy(
            header = Some(Header(ProtocolHelper.toPeerNode(sender), typeId, cl, nid, compressed))
          )
          val circuit = circuitBreaker(newStmd)
          if (circuit.broken) Right(newStmd.copy(circuit = circuit))
          else Left(newStmd)

        case (stmd, Chunk(Chunk.Content.Data(ChunkData(newData)))) =>
          val array = newData.toByteArray
          stmd.fos.write(array)
          stmd.fos.flush()
          val readSoFar = stmd.readSoFar + array.length
          val newStmd   = stmd.copy(readSoFar = readSoFar)
          val circuit   = circuitBreaker(newStmd)
          if (circuit.broken)
            Right(newStmd.copy(circuit = circuit))
          else Left(newStmd)
        case (stmd, _) =>
          Right(
            stmd.copy(
              circuit = Opened(StreamHandler.StreamError.notFullMessage("Not all data received"))
            )
          )
      }

    EitherT(collectStream.attempt.map {
      case Right(Streamed(_, _, Opened(error), _, _)) => error.asLeft
      case Right(stmd)                                => stmd.asRight
      case Left(t)                                    => StreamError.unexpected(t).asLeft
    })

  }

  private def toResult(
      stmd: Streamed
  ): EitherT[Task, StreamError, StreamMessage] = {
    val notFullError = StreamError.notFullMessage(stmd.toString).asLeft[StreamMessage]

    EitherT(Task.delay {
      stmd match {
        case Streamed(
            Some(Header(sender, packetType, contentLength, _, compressed)),
            readSoFar,
            _,
            path,
            _
            ) =>
          val result =
            StreamMessage(sender, packetType, path, compressed, contentLength).asRight[StreamError]
          if (!compressed && readSoFar != contentLength) notFullError
          else result

        case _ => notFullError
      }
    })
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
    }) >>= (res => msg.path.deleteSingleFile[Task].as(res))

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
}
