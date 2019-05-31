package coop.rchain.comm.transport

import java.io._
import java.nio.file._
import java.text.SimpleDateFormat
import java.util.Date

import scala.util.Random

import cats.data._
import cats.effect.Sync
import cats.implicits._

import coop.rchain.catscontrib.ski.kp
import coop.rchain.comm.CommError
import coop.rchain.shared.GracefulClose._
import CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.crypto.codec.Base16

object PacketOps {

  def restore[F[_]: Sync](file: Path): F[CommErr[Packet]] = {
    def fileInputStream: F[CommErr[FileInputStream]] =
      Sync[F]
        .delay(new FileInputStream(file.toFile))
        .attempt
        .map(_.fold(unableToRestorePacket(file, _).asLeft, _.asRight))

    def parseFrom(fin: FileInputStream): F[CommErr[Packet]] =
      for {
        packetErr <- Sync[F].delay(Packet.parseFrom(fin)).attempt
        _         <- gracefullyClose(fin)
      } yield packetErr.fold(unableToRestorePacket(file, _).asLeft, _.asRight)

    (for {
      fin    <- EitherT(fileInputStream)
      packet <- EitherT(parseFrom(fin))
    } yield packet).value
  }

  implicit class RichPacket(packet: Packet) {
    def store[F[_]: Sync](folder: Path): F[CommErr[Path]] =
      for {
        packetFile <- createPacketFile[F](folder, "_packet.bts")
        fos        = packetFile.fos
        orErr <- Sync[F].delay {
                  fos.write(packet.toByteArray)
                  fos.flush()
                }.attempt
        closeErr <- gracefullyClose(fos)
      } yield closeErr
        .map(kp(orErr))
        .joinRight
        .fold(unableToStorePacket(packet, _).asLeft, kp(packetFile.file.asRight))
  }

  final case class PacketFile(file: Path, fos: FileOutputStream)

  def createPacketFile[F[_]: Sync](folder: Path, postfix: String): F[PacketFile] =
    for {
      _        <- Sync[F].delay(folder.toFile.mkdirs())
      fileName <- Sync[F].delay(timestamp + postfix)
      file     <- Sync[F].delay(folder.resolve(fileName))
      fos      <- Sync[F].delay(new FileOutputStream(file.toFile))
    } yield PacketFile(file, fos)

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
