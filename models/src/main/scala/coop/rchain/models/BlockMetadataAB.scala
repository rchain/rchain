package coop.rchain.models

import scodec.TransformSyntax
import scodec.bits.ByteVector

final case class JustificationBA(validator: Array[Byte], latestBlockHash: Array[Byte]) {
  def ==(justification: JustificationBA): Boolean =
    validator.sameElements(justification.validator) &&
      latestBlockHash.sameElements(justification.latestBlockHash)
}

object BlockMetadataScodecAB {
  import scodec.Codec
  import scodec.codecs._

  private val codecByteArray =
    variableSizeBytesLong(uint32, bytes).xmap[Array[Byte]](_.toArray, ByteVector(_))

  private val codecParents = listOfN(int32, codecByteArray)

  private val codecJustification  = (codecByteArray :: codecByteArray).as[JustificationBA]
  private val codecJustifications = listOfN(int32, codecJustification)

  private val tupleCodec: Codec[(Array[Byte], Long)] = codecByteArray.pairedWith(int64)
  private val codecWeightMap =
    listOfN(int32, tupleCodec).xmap[Map[Array[Byte], Long]](_.toMap, _.toList)

  private val codecMetadata =
    (("hash" | codecByteArray) :: ("parents" | codecParents) ::
      ("sender" | codecByteArray) :: ("justifications" | codecJustifications) ::
      ("weightMap" | codecWeightMap) :: ("blockNum" | int64) :: ("seqNum" | int32) :: ("invalid" | bool) ::
      ("df" | bool) :: ("finalized" | bool)).as[BlockMetadataAB]

  def encode(block: BlockMetadataAB): ByteVector =
    codecMetadata.encode(block).require.toByteVector

  def encodeToArray(block: BlockMetadataAB): Array[Byte] = encode(block).toArray

  def decodeFromArray(bytes: Array[Byte]): BlockMetadataAB = decode(ByteVector(bytes))
  def decode(serializedBlock: ByteVector): BlockMetadataAB =
    codecMetadata.decode(serializedBlock.toBitVector).require.value
}

final case class BlockMetadataAB(
    blockHash: Array[Byte],
    parents: List[Array[Byte]],
    sender: Array[Byte],
    justifications: List[JustificationBA],
    weightMap: Map[Array[Byte], Long],
    blockNum: Long,
    seqNum: Int,
    invalid: Boolean,
    directlyFinalized: Boolean,
    finalized: Boolean
) {
  def toByteVector: ByteVector = BlockMetadataScodecAB.encode(block = this)
  def toBytes: Array[Byte]     = this.toByteVector.toArray

  //  We should use this method to compare 2 blocks
  //  (operator `==` is incorrect here for Array[Byte])
  def isEqualTo(block: BlockMetadataAB): Boolean = {
    def compareWeightMaps(): Boolean = {
      val reformatMap    = weightMap.map { case (k, v)       => ByteVector(k) -> v }
      val reformatRefMap = block.weightMap.map { case (k, v) => ByteVector(k) -> v }
      reformatMap == reformatRefMap
    }

    val hashesEqual = blockHash.sameElements(block.blockHash)
    val parentsEqual = (parents zip block.parents).forall {
      case (a, b) => a.sameElements(b)
    }
    val senderEqual = sender.sameElements(block.sender)
    val justificationsEqual = (justifications zip block.justifications).forall {
      case (a, b) => a == b
    }

    val weightMapEqual = compareWeightMaps()

    hashesEqual && parentsEqual && senderEqual && justificationsEqual && weightMapEqual &&
    blockNum == block.blockNum && seqNum == block.seqNum && invalid == block.invalid &&
    directlyFinalized == block.directlyFinalized && finalized == block.finalized
  }
}

object BlockMetadataAB {
  def toByteVector(block: BlockMetadataAB): ByteVector =
    block.toByteVector
  def fromBytes(bytes: Array[Byte]): BlockMetadataAB =
    BlockMetadataScodecAB.decodeFromArray(bytes)
  def fromByteVector(byteVector: ByteVector): BlockMetadataAB =
    BlockMetadataScodecAB.decode(byteVector)
}

final case class JustificationBV(validator: ByteVector, latestBlockHash: ByteVector)

object BlockMetadataScodecBV {
  import scodec.Codec
  import scodec.codecs._

  private val codecByteVector = variableSizeBytesLong(uint32, bytes)
  private val codecParents    = listOfN(int32, codecByteVector)

  private val codecJustification  = (codecByteVector :: codecByteVector).as[JustificationBV]
  private val codecJustifications = listOfN(int32, codecJustification)

  private val tupleCodec: Codec[(ByteVector, Long)] = codecByteVector.pairedWith(int64)
  private val codecWeightMap =
    listOfN(int32, tupleCodec).xmap[Map[ByteVector, Long]](_.toMap, _.toList)

  private val codecMetadata =
    (("hash" | codecByteVector) :: ("parents" | codecParents) ::
      ("sender" | codecByteVector) :: ("justifications" | codecJustifications) ::
      ("weightMap" | codecWeightMap) :: ("blockNum" | int64) :: ("seqNum" | int32) :: ("invalid" | bool) ::
      ("df" | bool) :: ("finalized" | bool)).as[BlockMetadataBV]

  def encode(block: BlockMetadataBV): ByteVector =
    codecMetadata.encode(block).require.toByteVector

  def encodeToArray(block: BlockMetadataBV): Array[Byte] = encode(block).toArray

  def decodeFromArray(bytes: Array[Byte]): BlockMetadataBV = decode(ByteVector(bytes))
  def decode(serializedBlock: ByteVector): BlockMetadataBV =
    codecMetadata.decode(serializedBlock.toBitVector).require.value
}

final case class BlockMetadataBV(
    blockHash: ByteVector,
    parents: List[ByteVector],
    sender: ByteVector,
    justifications: List[JustificationBV],
    weightMap: Map[ByteVector, Long],
    blockNum: Long,
    seqNum: Int,
    invalid: Boolean,
    directlyFinalized: Boolean,
    finalized: Boolean
) {
  def toByteVector: ByteVector = BlockMetadataScodecBV.encode(block = this)
  def toBytes: Array[Byte]     = this.toByteVector.toArray
}

object BlockMetadataBV {
  def toByteVector(block: BlockMetadataBV): ByteVector =
    block.toByteVector
  def fromBytes(bytes: Array[Byte]): BlockMetadataBV =
    BlockMetadataScodecBV.decodeFromArray(bytes)
  def fromByteVector(byteVector: ByteVector): BlockMetadataBV =
    BlockMetadataScodecBV.decode(byteVector)
}
