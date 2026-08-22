package coop.rchain.casper.reporting

import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.codecs
import coop.rchain.casper.protocol.BlockEventInfo
import coop.rchain.shared.Compression
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import net.jpountz.lz4.{LZ4CompressorWithLength, LZ4DecompressorWithLength}
import scodec.bits.ByteVector

object ReportStore {

  val compressor = new LZ4CompressorWithLength(Compression.factory.fastCompressor())
  // val compressor = new LZ4CompressorWithLength(factory.highCompressor(17)) // Max compression
  val decompressor = new LZ4DecompressorWithLength(Compression.factory.fastDecompressor())

  def compressBytes(bytes: Array[Byte]): Array[Byte] = compressor.compress(bytes)

  type ReportStore[F[_]] = KeyValueTypedStore[F, ByteString, BlockEventInfo]

  val blockEventInfoCodecCompressed =
    scodec.codecs.bytes.xmap[BlockEventInfo](
      bv => BlockEventInfo.parseFrom(decompressor.decompress(bv.toArray)),
      bei => ByteVector(compressor.compress(bei.toByteArray))
    )
  def store[F[_]: Sync](kvm: KeyValueStoreManager[F]): F[ReportStore[F]] =
    kvm.database("reporting-cache", codecs.codecByteString, blockEventInfoCodecCompressed)
}
