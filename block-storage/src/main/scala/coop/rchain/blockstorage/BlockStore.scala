package coop.rchain.blockstorage

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.dag.codecs.{codecBlockHash, codecBlockMessage}
import coop.rchain.casper.protocol.{BlockMessage, BlockMessageProto}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Compression
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import net.jpountz.lz4.{LZ4CompressorWithLength, LZ4DecompressorWithLength}

object BlockStore {
  type BlockStore[F[_]] = KeyValueTypedStore[F, BlockHash, BlockMessage]

  def apply[F[_]: Sync](
      kvm: KeyValueStoreManager[F]
  ): F[KeyValueTypedStore[F, BlockHash, BlockMessage]] =
    kvm
      .store("blocks")
      .map(_.toTypedStore[BlockHash, BlockMessage](codecBlockHash, codecBlockMessage))

  def bytesToBlockMessage(bytes: Array[Byte]): BlockMessage =
    BlockMessage
      .from(BlockMessageProto.parseFrom(decompressBytes(bytes)))
      .getOrElse(BlockMessage.empty)

  def blockMessageToBytes(blockMessage: BlockMessage): Array[Byte] =
    compressBytes(blockMessage.toProto.toByteArray)

  // Compression

  val compressor = new LZ4CompressorWithLength(Compression.factory.fastCompressor())
  // val compressor = new LZ4CompressorWithLength(factory.highCompressor(17)) // Max compression
  val decompressor = new LZ4DecompressorWithLength(Compression.factory.fastDecompressor())

  def compressBytes(bytes: Array[Byte]): Array[Byte]   = compressor.compress(bytes)
  def decompressBytes(bytes: Array[Byte]): Array[Byte] = decompressor.decompress(bytes)
}
