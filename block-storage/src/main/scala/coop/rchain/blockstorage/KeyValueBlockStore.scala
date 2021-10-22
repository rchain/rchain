package coop.rchain.blockstorage

import java.nio.ByteBuffer

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.{ByteString, CodedInputStream}
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.ByteStringOps.RichByteString
import coop.rchain.shared.ByteVectorOps.RichByteVector
import coop.rchain.shared.Compression
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager}
import net.jpountz.lz4.{LZ4CompressorWithLength, LZ4DecompressorWithLength}
import scodec.bits.ByteVector

class KeyValueBlockStore[F[_]: Sync](
    private val store: KeyValueStore[F],
    private val storeApprovedBlock: KeyValueStore[F]
) extends BlockStore[F] {
  import KeyValueBlockStore._

  private def errorBlock(hash: BlockHash)(cause: String) = BlockStoreFatalError(
    s"Block decoding error, hash ${PrettyPrinter.buildString(hash)}. Cause: $cause"
  )

  override def get(blockHash: BlockHash): F[Option[BlockMessage]] = {
    import cats.instances.option._
    for {
      // Optional serialized block from the store
      bytes <- store.get1(blockHash.toDirectByteBuffer, ByteVector(_))
      // Decode protobuf message / throws if fail
      proto <- bytes.map(_.toArray).traverse(bytesToBlockProto[F])
      block <- proto.traverse(BlockMessage.from(_).leftMap(errorBlock(blockHash)).liftTo[F])
    } yield block
  }

  override def put(data: => (BlockHash, BlockMessage)): F[Unit] = Sync[F].defer {
    def toBuffer(b: BlockMessage) = blockProtoToBuffer(b.toProto)
    val (hash, block)             = data
    val keyBuffer                 = hash.toDirectByteBuffer
    store.put1(keyBuffer, block, toBuffer)
  }

  // ApprovedBlock store

  private def errorApprovedBlock(cause: String) = BlockStoreFatalError(
    s"Approved block decoding error. Cause: $cause"
  )

  override def getApprovedBlock: F[Option[ApprovedBlock]] = {
    import cats.instances.option._
    for {
      // Optional block message from the store
      proto <- storeApprovedBlock.get1(
                ByteVector(approvedBlockKey).toDirectByteBuffer,
                bufferToApprovedBlockProto
              )
      // Decode protobuf message / throw if fail
      block <- proto.traverse(ApprovedBlock.from(_).leftMap(errorApprovedBlock).liftTo[F])
    } yield block
  }

  override def putApprovedBlock(block: ApprovedBlock): F[Unit] = Sync[F].defer {
    def toBuffer(b: ApprovedBlock) = b.toProto.toByteString.toDirectByteBuffer
    val keyBuffer                  = ByteVector(approvedBlockKey).toDirectByteBuffer
    storeApprovedBlock.put1(keyBuffer, block, toBuffer)
  }
}

object KeyValueBlockStore {
  def apply[F[_]: Sync](
      store: KeyValueStore[F],
      storeApprovedBlock: KeyValueStore[F]
  ): F[BlockStore[F]] = Sync[F].delay(new KeyValueBlockStore[F](store, storeApprovedBlock))

  def apply[F[_]: Sync](kvm: KeyValueStoreManager[F]): F[BlockStore[F]] =
    for {
      store              <- kvm.store("blocks")
      storeApprovedBlock <- kvm.store("blocks-approved")
      blockStore         <- KeyValueBlockStore(store, storeApprovedBlock)
    } yield blockStore

  /**
    * This is fatal error from which Block Store can not be recovered.
    */
  final case class BlockStoreFatalError(message: String) extends Exception(message)

  // We store only one approved block so its key is constant.
  val approvedBlockKey = Array[Byte](42)

  // Encoding BlockMessage

  def bytesToBlockProto[F[_]: Sync](bytes: Array[Byte]): F[BlockMessageProto] =
    decompressBytes[F](bytes) map BlockMessageProto.parseFrom

  def blockProtoToBytes(blockProto: BlockMessageProto): Array[Byte] =
    compressBytes(blockProto.toByteArray)

  private def blockProtoToBuffer(blockProto: BlockMessageProto): ByteBuffer = {
    val bytes              = blockProtoToBytes(blockProto)
    val buffer: ByteBuffer = ByteBuffer.allocateDirect(bytes.length)
    buffer.put(bytes).flip
  }

  // Encoding ApprovedBlock

  private def bufferToApprovedBlockProto(buff: ByteBuffer): ApprovedBlockProto = {
    val inputStream = CodedInputStream.newInstance(buff)
    ApprovedBlockProto.parseFrom(inputStream)
  }

  // Compression

  val compressor = new LZ4CompressorWithLength(Compression.factory.fastCompressor())
  // val compressor = new LZ4CompressorWithLength(factory.highCompressor(17)) // Max compression
  val decompressor = new LZ4DecompressorWithLength(Compression.factory.fastDecompressor())

  def compressBytes(bytes: Array[Byte]): Array[Byte] = compressor.compress(bytes)

  def decompressBytes[F[_]: Sync](bytes: Array[Byte]): F[Array[Byte]] =
    Sync[F].delay(decompressor.decompress(bytes)).handleErrorWith { ex =>
      new Exception("Decompress of block failed.", ex).raiseError
    }

}
