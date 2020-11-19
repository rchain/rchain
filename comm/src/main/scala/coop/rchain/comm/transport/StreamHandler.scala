package coop.rchain.comm.transport

import java.io.FileOutputStream
import java.nio.file.{Files, Path}

import cats.data._
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.transport.PacketOps._
import coop.rchain.monix.Monixable
import coop.rchain.shared.Compression._
import coop.rchain.shared.GracefulClose._
import coop.rchain.shared.PathOps._
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, _}
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

  def handleStream[F[_]: Monixable: Sync: Log](
      folder: Path,
      stream: Observable[Chunk],
      circuitBreaker: CircuitBreaker
  ): F[Either[StreamError, StreamMessage]] =
    init(folder).toTask
      .bracketE { initStmd =>
        (collect(initStmd, stream, circuitBreaker) >>= toResult[F]).value.toTask
      }({
        // failed while collecting stream
        case (stmd, Right(Left(_))) =>
          (Log[F].warn("Failed collecting stream.") >>
            gracefullyClose[F](stmd.fos).as(()) >>
            stmd.path.deleteSingleFile[F]).toTask
        // should not happend (errors handled witin bracket) but covered for safety
        case (stmd, Left(_)) =>
          (Log[F].error(
            "Stream collection ended unexpected way. Please contact RNode code maintainer."
          ) >>
            gracefullyClose[F](stmd.fos).as(()) >>
            stmd.path.deleteSingleFile[F]).toTask
        // succesfully collected
        case (stmd, _) =>
          (Log[F].debug("Stream collected.") >>
            gracefullyClose[F](stmd.fos).void).toTask
      })
      .attempt
      .map { res =>
        import cats.instances.either._
        res.leftMap(StreamError.unexpected).flatten
      }
      .fromTask

  private def init[F[_]: Sync](folder: Path): F[Streamed] =
    createPacketFile[F](folder, "_packet_streamed.bts")
      .map { case (file, fos) => Streamed(fos = fos, path = file) }

  private def collect[F[_]: Monixable: Sync](
      init: Streamed,
      stream: Observable[Chunk],
      circuitBreaker: CircuitBreaker
  ): EitherT[F, StreamError, Streamed] = {

    def collectStream: F[Streamed] =
      stream
        .foldWhileLeftL(init) {
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
        .fromTask

    EitherT(collectStream.attempt.map {
      case Right(Streamed(_, _, Opened(error), _, _)) => error.asLeft
      case Right(stmd)                                => stmd.asRight
      case Left(t)                                    => StreamError.unexpected(t).asLeft
    })

  }

  private def toResult[F[_]: Sync](
      stmd: Streamed
  ): EitherT[F, StreamError, StreamMessage] = {
    val notFullError = StreamError.notFullMessage(stmd.toString).asLeft[StreamMessage]

    EitherT(Sync[F].delay {
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

  def restore[F[_]: Sync: Log](msg: StreamMessage): F[Either[Throwable, Blob]] =
    (fetchContent[F](msg.path).attempt flatMap {
      case Left(ex) =>
        Log[F].error("Could not read streamed data from file", ex).as(ex.asLeft[Blob])
      case Right(content) =>
        decompressContent[F](content, msg.compressed, msg.contentLength).attempt flatMap {
          case Left(ex) =>
            Log[F].error("Could not decompressed data ").as(ex.asLeft[Blob])
          case Right(decompressedContent) =>
            ProtocolHelper
              .blob(msg.sender, msg.typeId, decompressedContent)
              .asRight[Throwable]
              .pure[F]
        }
    }) >>= (res => msg.path.deleteSingleFile[F].as(res))

  private def fetchContent[F[_]: Sync](path: Path): F[Array[Byte]] =
    Sync[F].delay(Files.readAllBytes(path))

  private def decompressContent[F[_]: Sync](
      raw: Array[Byte],
      compressed: Boolean,
      contentLength: Int
  ): F[Array[Byte]] =
    if (compressed) {
      raw
        .decompress(contentLength)
        .fold(Sync[F].raiseError[Array[Byte]](new RuntimeException("Could not decompress data")))(
          _.pure[F]
        )
    } else raw.pure[F]
}
