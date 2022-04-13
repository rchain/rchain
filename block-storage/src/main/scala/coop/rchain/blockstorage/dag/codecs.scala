package coop.rchain.blockstorage.dag

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.blockStore.{blockMessageToBytes, bytesToBlockMessage}
import coop.rchain.casper.protocol._
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.{BlockHash, BlockMetadata, Validator}
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.{Attempt, Codec, Err}

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

  val codecBlockMessage = bytes.exmap[BlockMessage](
    byteVector => Attempt.fromEither(bytesToBlockMessage(byteVector.toArray).leftMap(Err(_))),
    blockMessage => Attempt.successful(ByteVector(blockMessageToBytes(blockMessage)))
  )

  val codecValidator = xmapToByteString(bytes(Validator.Length))

  val codecSeqNum = int32

  val codecBlockHashSet = listOfN(int32, codecBlockHash).xmap[Set[BlockHash]](_.toSet, _.toList)

  val codecSignedDeployData = variableSizeBytes(int32, bytes).xmap[Signed[DeployData]](
    byteVector => DeployData.from(DeployDataProto.parseFrom(byteVector.toArray)).right.get,
    signedDeployData => ByteVector(DeployData.toProto(signedDeployData).toByteArray)
  )

  val codecByteString = xmapToByteString(variableSizeBytes(uint8, bytes))

  val codecDeploySignature = xmapToByteString(bytes)
}
