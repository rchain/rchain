package coop.rchain.casper.helper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil.hashSignedBlock
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.SignaturesAlg
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.{BlockHash, Validator}

import scala.util.Random

object BlockUtil {
  def resignBlock(b: BlockMessage, sk: PrivateKey): BlockMessage = {
    def signFunction: (Array[Byte], PrivateKey) => Array[Byte] =
      SignaturesAlg(b.sigAlgorithm).get.sign

    val blockHash =
      hashSignedBlock(b)
    val sig = ByteString.copyFrom(signFunction(blockHash.toByteArray, sk))
    b.copy(blockHash = blockHash, sig = sig)
  }

  def generateValidator(prefix: String = ""): ByteString = {
    val array = Array.ofDim[Byte](Validator.Length)
    Random.nextBytes(array)
    ByteString.copyFrom(array)
  }

  def generateHash(prefix: String = ""): BlockHash = {
    val array = Array.ofDim[Byte](BlockHash.Length)
    Random.nextBytes(array)
    ByteString.copyFrom(array)
  }
}
