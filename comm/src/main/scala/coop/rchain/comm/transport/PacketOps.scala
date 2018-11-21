package coop.rchain.comm.transport

import coop.rchain.comm.{CommError, PeerNode}, CommError._
import coop.rchain.comm.protocol.routing._
import java.nio.file._
import com.google.protobuf.CodedOutputStream
import java.io._
import cats._, cats.data._, cats.implicits._
import cats.effect.Sync
import java.util.UUID

object PacketOps {

  def restore[F[_]: Sync](file: Path): F[CommErr[Packet]] =
    for {
      fin       <- Sync[F].delay(new FileInputStream(file.toFile))
      packetErr <- Sync[F].delay(Packet.parseFrom(fin)).attempt
      resErr <- packetErr match {
                 case Left(th) =>
                   gracefullyClose(fin) *> Left(unabletoRestorePacket(file, th)).pure[F]
                 case Right(packet) => gracefullyClose(fin) *> Right(packet).pure[F]
               }
    } yield resErr

  def gracefullyClose[F[_]: Sync](closable: AutoCloseable): F[Either[Throwable, Unit]] =
    Sync[F].delay(closable.close).attempt

  implicit class RichPacket(packet: Packet) {
    def store[F[_]: Sync](folder: Path): F[CommErr[Path]] =
      for {
        _        <- Sync[F].delay(folder.toFile.mkdirs())
        fileName <- Sync[F].delay(UUID.randomUUID.toString + "_packet.bts")
        file     = folder.resolve(fileName)
        fos      <- Sync[F].delay(new FileOutputStream(file.toFile))
        orErr <- Sync[F].delay {
                  fos.write(packet.toByteArray)
                  fos.flush()
                }.attempt
        resErr <- orErr match {
                   case Left(th) =>
                     gracefullyClose(fos) *> Left(unableToStorePacket(packet, th)).pure[F]
                   case Right(_) =>
                     gracefullyClose(fos) map {
                       case Left(th) => Left(unableToStorePacket(packet, th))
                       case Right(_) => Right(file)
                     }
                 }
      } yield resErr
  }
}
