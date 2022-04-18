package coop.rchain.models

import scodec.TransformSyntax
import scodec.bits.ByteVector

final case class JustificationArr(validator: Array[Byte], latestBlockHash: Array[Byte])

object MetadataScodec {
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
      ("df" | bool) :: ("finalized" | bool)).as[BlockMetadataScodec]

  def encode(block: BlockMetadataScodec): ByteVector =
    codecMetadata.encode(block).require.toByteVector

  def decode(serializedBlock: ByteVector): BlockMetadataScodec =
    codecMetadata.decode(serializedBlock.toBitVector).require.value
}

final case class BlockMetadataScodec(
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
  def toByteString = MetadataScodec.encode(block = this)
}
