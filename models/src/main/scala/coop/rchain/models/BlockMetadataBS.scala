package coop.rchain.models

import com.google.protobuf.ByteString
import scodec.TransformSyntax
import scodec.bits.ByteVector

final case class JustificationBS(validator: ByteString, latestBlockHash: ByteString)

object BlockMetadataScodecBS {
  import scodec.Codec
  import scodec.codecs._

  private val codecByteString =
    variableSizeBytesLong(uint32, bytes)
      .xmap[ByteString](bv => ByteString.copyFrom(bv.toArray), bs => ByteVector(bs.toByteArray))

  private val codecParents = listOfN(int32, codecByteString)

  private val codecJustification  = (codecByteString :: codecByteString).as[JustificationBS]
  private val codecJustifications = listOfN(int32, codecJustification)

  private val tupleCodec: Codec[(ByteString, Long)] = codecByteString.pairedWith(int64)
  private val codecWeightMap =
    listOfN(int32, tupleCodec).xmap[Map[ByteString, Long]](_.toMap, _.toList)

  private val codecMetadata =
    (("hash" | codecByteString) :: ("parents" | codecParents) ::
      ("sender" | codecByteString) :: ("justifications" | codecJustifications) ::
      ("weightMap" | codecWeightMap) :: ("blockNum" | int64) :: ("seqNum" | int32) :: ("invalid" | bool) ::
      ("df" | bool) :: ("finalized" | bool)).as[BlockMetadataBS]

  def encode(block: BlockMetadataBS): ByteVector =
    codecMetadata.encode(block).require.toByteVector

  def encodeToArray(block: BlockMetadataBS): Array[Byte] = encode(block).toArray

  def decodeFromArray(bytes: Array[Byte]): BlockMetadataBS = decode(ByteVector(bytes))
  def decode(serializedBlock: ByteVector): BlockMetadataBS =
    codecMetadata.decode(serializedBlock.toBitVector).require.value
}

final case class BlockMetadataBS(
    blockHash: ByteString,
    parents: List[ByteString],
    sender: ByteString,
    justifications: List[JustificationBS],
    weightMap: Map[ByteString, Long],
    blockNum: Long,
    seqNum: Int,
    invalid: Boolean,
    directlyFinalized: Boolean,
    finalized: Boolean
) {
  def toByteVector: ByteVector = BlockMetadataScodecBS.encode(block = this)
  def toBytes: Array[Byte]     = this.toByteVector.toArray
}

object BlockMetadataBS {
  def toByteVector(block: BlockMetadataBS): ByteVector =
    block.toByteVector
  def fromBytes(bytes: Array[Byte]): BlockMetadataBS =
    BlockMetadataScodecBS.decodeFromArray(bytes)
  def fromByteVector(byteVector: ByteVector): BlockMetadataBS =
    BlockMetadataScodecBS.decode(byteVector)
}
