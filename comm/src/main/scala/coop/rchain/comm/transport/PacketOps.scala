package coop.rchain.comm.transport

import java.io._
import java.nio.file._
import java.text.SimpleDateFormat
import java.util.Date

import scala.util.Random

import cats.effect.{Resource, Sync}
import cats.implicits._

import coop.rchain.comm.CommError
import CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.crypto.codec.Base16

object PacketOps {

  def restore[F[_]: Sync](file: Path): F[CommErr[Packet]] =
    Resource
      .fromAutoCloseable(Sync[F].delay(new FileInputStream(file.toFile)))
      .use(fin => Sync[F].delay(Packet.parseFrom(fin)))
      .attempt
      .map(_.fold(unableToRestorePacket(file, _).asLeft, _.asRight))

  implicit class RichPacket(packet: Packet) {
    def store[F[_]: Sync](folder: Path): F[CommErr[Path]] =
      Resource
        .make(createPacketFile[F](folder, s"_packet_${packet.extra}.bts")) {
          case (_, fos) => Sync[F].delay(fos.close())
        }
        .use {
          case (file, fos) =>
            Sync[F].delay {
              fos.write(packet.toByteArray)
              fos.flush()
              file
            }
        }
        .attempt
        .map(_.fold(unableToStorePacket(packet, _).asLeft, _.asRight))
  }

  def createPacketFile[F[_]: Sync](folder: Path, postfix: String): F[(Path, FileOutputStream)] =
    for {
      _    <- Sync[F].delay(folder.toFile.mkdirs())
      file <- Sync[F].delay(folder.resolve(timestamp + postfix))
    } yield (file, new FileOutputStream(file.toFile))

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
