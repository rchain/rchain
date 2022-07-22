package coop.rchain.blockstorage.dag

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.{blockMessageToBytes, bytesToBlockMessage}
import coop.rchain.blockstorage.approvedStore.{bytesToFringe, fringeToBytes}
import coop.rchain.casper.protocol.{BlockMessage, DeployData, DeployDataProto, FinalizedFringe}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.{BlockHash, BlockMetadata, FringeData}
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.{Attempt, Codec, Err}

object codecs {
  private def xmapToByteString(codec: Codec[ByteVector]): Codec[ByteString] =
    codec.xmap[ByteString](
      byteVector => ByteString.copyFrom(byteVector.toArray),
      byteString => ByteVector(byteString.toByteArray)
    )

  val codecByteString = xmapToByteString(bytes)

  val codecBlockHash = xmapToByteString(bytes(BlockHash.Length))

  val codecBlockMetadata = bytes.xmap[BlockMetadata](
    byteVector => BlockMetadata.fromBytes(byteVector.toArray),
    blockMetadata => ByteVector(BlockMetadata.toBytes(blockMetadata))
  )

  val codecFringeData = bytes.xmap[FringeData](
    byteVector => FringeData.fromBytes(byteVector.toArray),
    fringeData => ByteVector(FringeData.toBytes(fringeData))
  )

  val codecBlockMessage = bytes.exmap[BlockMessage](
    byteVector => Attempt.fromEither(bytesToBlockMessage(byteVector.toArray).leftMap(Err(_))),
    blockMessage => Attempt.successful(ByteVector(blockMessageToBytes(blockMessage)))
  )

  val codecFringe = bytes.xmap[FinalizedFringe](
    byteVector => bytesToFringe(byteVector.toArray),
    fringe => ByteVector(fringeToBytes(fringe))
  )

  val codecSignedDeployData = bytes.xmap[Signed[DeployData]](
    byteVector => DeployData.from(DeployDataProto.parseFrom(byteVector.toArray)).right.get,
    signedDeployData => ByteVector(DeployData.toProto(signedDeployData).toByteArray)
  )
}
