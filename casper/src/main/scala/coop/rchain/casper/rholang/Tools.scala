package coop.rchain.casper.rholang

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployDataProto
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random

object Tools {
  def unforgeableNameRng(deployer: PublicKey, timestamp: Long): Blake2b512Random = {
    val seed =
      DeployDataProto().withDeployer(ByteString.copyFrom(deployer.bytes)).withTimestamp(timestamp)
    Blake2b512Random(DeployDataProto.toByteArray(seed))
  }

  def rng(signature: Array[Byte]): Blake2b512Random =
    Blake2b512Random(signature)
}
