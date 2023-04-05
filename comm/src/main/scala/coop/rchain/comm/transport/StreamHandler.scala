package coop.rchain.comm.transport

import cats.data._
import cats.effect.Sync
import cats.effect.implicits.catsEffectSyntaxBracket
import cats.syntax.all._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.transport.PacketOps._
import coop.rchain.shared.Compression._
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream

import scala.collection.concurrent.TrieMap

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
    key: String
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

  def handleStream[F[_]: Sync: Log](
      stream: Stream[F, Chunk],
      circuitBreaker: CircuitBreaker,
      cache: TrieMap[String, Array[Byte]]
  ): F[Either[StreamError, StreamMessage]] =
    init(cache)
      .flatMap { initStmd =>
        (collect(initStmd, stream, circuitBreaker, cache) >>= toResult[F]).value.flatTap(
          _.leftMap(_ => cache.remove(initStmd.key)).pure
        )
      }
      .attempt
      .map(_.leftMap(StreamError.unexpected).flatten)

  private def init[F[_]: Sync](cache: TrieMap[String, Array[Byte]]): F[Streamed] =
    createCacheEntry[F]("packet_send/", cache) map (key => Streamed(key = key))

  private def collect[F[_]: Sync](
      init: Streamed,
      stream: Stream[F, Chunk],
      circuitBreaker: CircuitBreaker,
      cache: TrieMap[String, Array[Byte]]
  ): EitherT[F, StreamError, Streamed] = {

    def collectStream: F[Streamed] =
      stream
        .scan(init.asLeft[Streamed]) {
          case (
              Left(stmd),
              Chunk(Chunk.Content.Header(ChunkHeader(sender, typeId, compressed, cl, nid)))
              ) =>
            val newStmd = stmd.copy(
              header = Some(Header(ProtocolHelper.toPeerNode(sender), typeId, cl, nid, compressed))
            )
            val circuit = circuitBreaker(newStmd)
            if (circuit.broken) Right(newStmd.copy(circuit = circuit))
            else Left(newStmd)

          case (Left(stmd), Chunk(Chunk.Content.Data(ChunkData(newData)))) =>
            val receivedBytes = newData.toByteArray

            // Write data to cache
            val existingBytes = cache(stmd.key)
            val bytes         = existingBytes ++ receivedBytes
            cache.update(stmd.key, bytes)

            val readSoFar = stmd.readSoFar + receivedBytes.length
            val newStmd   = stmd.copy(readSoFar = readSoFar)
            val circuit   = circuitBreaker(newStmd)
            if (circuit.broken)
              Right(newStmd.copy(circuit = circuit))
            else Left(newStmd)
          case (Left(stmd), _) =>
            Right(
              stmd.copy(
                circuit = Opened(StreamHandler.StreamError.notFullMessage("Not all data received"))
              )
            )
          case (x @ Right(_), _) => x
        }
        .takeThrough(x => x.isLeft)
        .map(_.merge)
        .compile
        .lastOrError

    EitherT(collectStream.attempt.map {
      case Right(Streamed(_, _, Opened(error), _)) => error.asLeft
      case Right(stmd)                             => stmd.asRight
      case Left(t)                                 => StreamError.unexpected(t).asLeft
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
            key
            ) =>
          val result =
            StreamMessage(sender, packetType, key, compressed, contentLength).asRight[StreamError]
          if (!compressed && readSoFar != contentLength) notFullError
          else result

        case _ => notFullError
      }
    })
  }

  def restore[F[_]: Sync: Log](
      msg: StreamMessage,
      cache: TrieMap[String, Array[Byte]]
  ): F[Either[Throwable, Blob]] =
    (Sync[F].delay(cache(msg.key)).attempt >>= {
      case Left(ex) =>
        Log[F]
          .error(s"Could not read streamed data from cache (key: ${msg.key})", ex)
          .as(ex.asLeft[Blob])
      case Right(content) =>
        decompressContent[F](content, msg.compressed, msg.contentLength).attempt flatMap {
          case Left(ex) =>
            Log[F].error(s"Could not decompressed data (key: ${msg.key})").as(ex.asLeft[Blob])
          case Right(decompressedContent) =>
            ProtocolHelper
              .blob(msg.sender, msg.typeId, decompressedContent)
              .asRight[Throwable]
              .pure[F]
        }
    }) <* Sync[F].delay(cache.remove(msg.key))

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
