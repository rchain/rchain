package coop.rchain.comm.transport

import cats.effect.Sync
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.shared.Compression._

object Chunker {

  def chunkIt[F[_]: Sync](networkId: String, blob: Blob, maxMessageSize: Int): F[Iterator[Chunk]] =
    Sync[F].delay {
      val raw      = blob.packet.content.toByteArray
      val kb500    = 1024 * 500
      val compress = raw.length > kb500
      val content  = if (compress) raw.compress else raw

      def header: Chunk =
        Chunk().withHeader(
          ChunkHeader()
            .withCompressed(compress)
            .withContentLength(raw.length)
            .withSender(ProtocolHelper.node(blob.sender))
            .withTypeId(blob.packet.typeId)
            .withNetworkId(networkId)
        )
      val buffer    = 2 * 1024 // 2 kbytes for protobuf related stuff
      val chunkSize = maxMessageSize - buffer
      def data: Iterator[Chunk] =
        content.sliding(chunkSize, chunkSize).map { data =>
          Chunk().withData(ChunkData().withContentData(ProtocolHelper.toProtocolBytes(data)))
        }

      Iterator(header) ++ data
    }

}
