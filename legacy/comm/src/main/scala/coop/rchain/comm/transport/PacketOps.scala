package coop.rchain.comm.transport

import java.text.SimpleDateFormat
import java.util.Date
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.shared.Base16

import scala.collection.concurrent.TrieMap
import scala.util.Random

object PacketOps {

  def restore[F[_]: Sync](key: String, cache: TrieMap[String, Array[Byte]]): F[CommErr[Packet]] =
    Sync[F]
      .delay(cache(key))
      .map(Packet.parseFrom)
      .attempt
      .map(_.leftMap(unableToRestorePacket(key, _)))

  implicit class RichPacket(packet: Packet) {
    def store[F[_]: Sync](cache: TrieMap[String, Array[Byte]]): F[CommErr[String]] =
      createCacheEntry[F]("packet_receive/", cache)
        .flatMap { key =>
          // Write packet data to cache
          Sync[F].delay(cache.update(key, packet.toByteArray)).as(key)
        }
        .attempt
        .map(_.leftMap(unableToStorePacket(packet, _)))
  }

  /**
    * Generates key and put empty data in streaming cache.
    *
    * @param prefix for generated key
    * @param cache streaming cache
    * @return key
    */
  def createCacheEntry[F[_]: Sync](
      prefix: String,
      cache: TrieMap[String, Array[Byte]]
  ): F[String] = {
    val key = s"$prefix/$timestamp"
    Sync[F]
      .delay(cache.put(key, Array[Byte]()))
      .as(key)
  }

  private val TS_FORMAT = "yyyyMMddHHmmss"

  private def timestamp: String = {
    val dateFormat = new SimpleDateFormat(TS_FORMAT)
    val bytes      = Array.ofDim[Byte](4)
    Random.nextBytes(bytes)
    val date = dateFormat.format(new Date())
    val hex  = Base16.encode(bytes)
    s"${date}_$hex"
  }
}
