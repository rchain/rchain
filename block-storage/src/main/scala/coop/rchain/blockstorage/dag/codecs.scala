package coop.rchain.blockstorage.dag

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.{DagFringe, DeployId}
import coop.rchain.casper.protocol.{DeployChain, DeployData, DeployDataProto}
import coop.rchain.blockstorage.casper.Casper.FinalizationFringe
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash
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

  val codecDeployChain =
    listOfN(int32, codecDeployId).xmap[DeployChain](l => DeployChain(l), dc => dc.deploys)

  val codecBlockHash = xmapToByteString(bytes(BlockHash.Length))
  val codecStateHash = xmapToByteString(bytes(StateHash.Length))

  val codecBlockMetadata = variableSizeBytes(uint16, bytes).xmap[BlockMetadata](
    byteVector => BlockMetadata.fromBytes(byteVector.toArray),
    blockMetadata => ByteVector(blockMetadata.toByteString.toByteArray)
  )

  val codecValidator = xmapToByteString(bytes(Validator.Length))

  val codecSeqNum = int64

  val codecBlockHashSet = listOfN(int32, codecBlockHash).xmap[Set[BlockHash]](_.toSet, _.toList)

  val codecSignedDeployData = variableSizeBytes(int32, bytes).xmap[Signed[DeployData]](
    byteVector => DeployData.from(DeployDataProto.parseFrom(byteVector.toArray)).right.get,
    signedDeployData => ByteVector(DeployData.toProto(signedDeployData).toByteArray)
  )

  val codecByteString = xmapToByteString(variableSizeBytes(uint8, bytes))

  val codecDeploySignature = xmapToByteString(bytes)

  val codecFinalizationFringe: Codec[FinalizationFringe[Validator, BlockHash]] =
    listOfN(int32, codecValidator ~ codecBlockHashSet)

  val codecDagFringe: Codec[DagFringe] =
    (codecFinalizationFringe :: codecStateHash :: int64).as[DagFringe]
}
