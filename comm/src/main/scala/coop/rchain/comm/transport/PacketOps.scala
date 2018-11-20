package coop.rchain.comm.transport

import coop.rchain.comm.{CommError, PeerNode}, CommError.CommErr
import coop.rchain.comm.protocol.routing._
import java.nio.file._
import com.google.protobuf.CodedOutputStream
import java.io._
import cats._, cats.data._, cats.implicits._
import cats.effect.Sync
import java.util.UUID

object PacketOps {

  def restore[F[_]: Sync](file: Path): F[Packet] = Sync[F].delay {
    val fin    = new FileInputStream(file.toFile)
    val packet = Packet.parseFrom(fin)
    fin.close()
    packet
  }

  implicit class RichPacket(packet: Packet) {
    def store[F[_]: Sync](folder: Path): F[Path] =
      Sync[F].delay {
        // FIX-ME proper exception handling
        val fileName = UUID.randomUUID.toString + "_packet.bts"
        val file     = folder.resolve(fileName)
        val fos      = new FileOutputStream(file.toFile)
        fos.write(packet.toByteArray)
        fos.flush()
        fos.close()
        file
      }
  }

}
