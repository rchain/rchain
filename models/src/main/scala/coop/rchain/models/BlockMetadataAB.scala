package coop.rchain.models

import scodec.TransformSyntax
import scodec.bits.ByteVector

final case class JustificationArr(validator: Array[Byte], latestBlockHash: Array[Byte]) {
  def ==(justification: JustificationArr): Boolean =
    validator.sameElements(justification.validator) &&
      latestBlockHash.sameElements(justification.latestBlockHash)
}

object BlockMetadataScodec {
  import scodec.Codec
  import scodec.codecs._

  private val codecByteArray = {
    variableSizeBytes(uint8, bytes).xmap[Array[Byte]](_.toArray, ByteVector(_))
  }

  //  Codecs of Lists and Map should be created through the flatZip method
  //  (these codecs must know exact count of elements in List/Map containers)
  private val codecParentsBase = uint16 flatZip { count =>
    listOfN(provide(count), codecByteArray)
  }
  private val codecParents = codecParentsBase.xmap[List[Array[Byte]]]({ case (_, lst) => lst }, {
    sourceList =>
      (sourceList.size, sourceList)
  })

  private val codecJustification = (codecByteArray :: codecByteArray).as[JustificationArr]
  private val codecJustificationsBase = uint16 flatZip { count =>
    listOfN(provide(count), codecJustification)
  }

  private val codecJustifications = codecJustificationsBase.xmap[List[JustificationArr]]({
    case (_, lst) => lst
  }, { sourceList =>
    (sourceList.size, sourceList)
  })

  private val tupleCodec: Codec[(Array[Byte], Long)] = codecByteArray.pairedWith(int64)
  private val codecWeightMapBase = uint16 flatZip { count =>
    val tuplesList = listOfN(provide(count), tupleCodec)
    tuplesList.xmap[Map[Array[Byte], Long]](_.toMap, _.toList)
  }
  private val codecWeightMap = codecWeightMapBase.xmap[Map[Array[Byte], Long]](
    { case (_, sourceMap) => sourceMap }, { sourceMap =>
      (sourceMap.size, sourceMap)
    }
  )

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
    justifications: List[JustificationArr],
    weightMap: Map[Array[Byte], Long],
    blockNum: Long,
    seqNum: Int,
    invalid: Boolean,
    directlyFinalized: Boolean,
    finalized: Boolean
) {
  def toByteVector: ByteVector = BlockMetadataScodec.encode(block = this)
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
    BlockMetadataScodec.decodeFromArray(bytes)
  def fromByteVector(byteVector: ByteVector): BlockMetadataAB =
    BlockMetadataScodec.decode(byteVector)
}
