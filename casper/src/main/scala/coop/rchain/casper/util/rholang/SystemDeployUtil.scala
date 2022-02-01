package coop.rchain.casper.util.rholang

import com.google.protobuf.{ByteString}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import com.google.protobuf.wrappers.Int32Value
import coop.rchain.casper.genesis.contracts.Validator
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Signed
import coop.rchain.crypto.PublicKey

object SystemDeployUtil {
  // Currently we have 4 system deploys -> refund, preCharge, closeBlock, Slashing
  // In every user deploy, the rnode would do the preCharge first, then execute the
  // user deploy and do the refund at last.
  //
  // The refund and preCharge system deploy
  // would use user deploy signature to generate the system deploy. The random seed of
  // the refund and preCharge has to be exactly the same to make sure replay the user
  // deploy would come out the exact same result.
  //
  // As for closeBlock and slashing, the rnode would execute closeBlock system deploy in every block.
  // So for a block seed it would be enough to have:
  // PREFIX ++ PublicKey ++ seqNum serialized with protobuf.
  // This way we can be completely sure that collision cannot happen.
  // (Quote: https://github.com/rchain/rchain/pull/2879#discussion_r378948921)
  import coop.rchain.models.Validator.Validator

  val SYSTEM_DEPLOY_PREFIX = 1

  private def serializeInt32Fixed(value: Int): Array[Byte] = {
    val stream = new ByteArrayOutputStream
    val proto  = CodedOutputStream.newInstance(stream)
    proto.writeFixed32NoTag(value)
    proto.flush()
    stream.toByteArray
  }

  private def serializeInt64Fixed(value: Long): Array[Byte] = {
    val stream = new ByteArrayOutputStream
    val proto  = CodedOutputStream.newInstance(stream)
    proto.writeFixed64NoTag(value)
    proto.flush()
    stream.toByteArray
  }
  def generateSystemDeployRandomSeed(sender: Validator, seqNum: Long): Blake2b512Random =
    Tools.rng(
      serializeInt32Fixed(SYSTEM_DEPLOY_PREFIX) ++ sender
        .toByteArray() ++ serializeInt64Fixed(seqNum)
    )

  def generateCloseDeployRandomSeed(sender: Validator, seqNum: Long): Blake2b512Random =
    generateSystemDeployRandomSeed(sender, seqNum).splitByte(0)

  def generateCloseDeployRandomSeed(pk: PublicKey, seqNum: Long): Blake2b512Random = {
    val sender = ByteString.copyFrom(pk.bytes)
    generateCloseDeployRandomSeed(sender, seqNum)
  }

  def generateSlashDeployRandomSeed(sender: Validator, seqNum: Long): Blake2b512Random =
    generateSystemDeployRandomSeed(sender, seqNum).splitByte(1)

  def generatePreChargeDeployRandomSeed(deploy: Signed[DeployData]): Blake2b512Random =
    Tools.rng(deploy.sig.toByteArray).splitByte(0)

  def generateRefundDeployRandomSeed(deploy: Signed[DeployData]): Blake2b512Random =
    Tools.rng(deploy.sig.toByteArray).splitByte(1)

}
