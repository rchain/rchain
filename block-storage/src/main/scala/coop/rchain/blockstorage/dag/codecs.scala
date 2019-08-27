package coop.rchain.blockstorage.dag

import com.google.protobuf.ByteString
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.{BlockHash, BlockMetadata, Validator}
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

object codecs {
  private def xmapToByteString(codec: Codec[ByteVector]): Codec[ByteString] =
    codec.xmap[ByteString](
      byteVector => ByteString.copyFrom(byteVector.toArray),
      byteString => ByteVector(byteString.toByteArray)
    )

  val codecDeployId = xmapToByteString(variableSizeBytes(uint8, bytes))

  val codecBlockHash = xmapToByteString(bytes(BlockHash.Length))

  val codecBlockMetadata = variableSizeBytes(uint16, bytes).xmap[BlockMetadata](
    byteVector => BlockMetadata.fromBytes(byteVector.toArray),
    blockMetadata => ByteVector(blockMetadata.toByteString.toByteArray)
  )

  val codecValidator = xmapToByteString(bytes(Validator.Length))

  val codecSeqNum = int32

  val codecBlockHashSet = listOfN(int32, codecBlockHash).xmap[Set[BlockHash]](_.toSet, _.toList)
}
