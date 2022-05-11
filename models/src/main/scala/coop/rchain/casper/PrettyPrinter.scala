package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Base16

object PrettyPrinter {

  def buildStringNoLimit(b: Array[Byte]): String = Base16.encode(b)
  def buildStringNoLimit(b: ByteString): String  = Base16.encode(b.toByteArray)

  def buildString(t: CasperMessage, short: Boolean = false): String =
    t match {
      case b: BlockMessage => buildString(b, short)
      case _               => "Unknown consensus protocol message"
    }

  private def buildString(b: BlockMessage, short: Boolean): String =
    if (short) {
      s"#${b.blockNumber} (${buildString(b.blockHash)})"
    } else {
      s"Block #${b.blockNumber} (${buildString(b.blockHash)}) " +
        s"-- Sender ID ${buildString(b.sender)} " +
        s"-- Contents ${buildString(b.body.state)}" +
        s"-- Shard ID ${limit(b.shardId, maxLength = 10)}" +
        s"-- Justifications ${buildJustifications(b.justifications)} "
    }

  private def buildJustifications(hashes: List[BlockHash]): String =
    hashes.map(h => Base16.encode(h.toByteArray)).map(limit(_, maxLength = 5)).mkString(" ")

  def buildString(bh: BlockHashMessage): String =
    s"Block hash: ${buildString(bh.blockHash)}"

  private def limit(str: String, maxLength: Int): String =
    if (str.length > maxLength) {
      str.substring(0, maxLength) + "..."
    } else {
      str
    }

  def buildString(d: ProcessedDeploy): String =
    s"User: ${buildStringNoLimit(d.deploy.pk.bytes)}, Cost: ${d.cost.toString} " +
      s"${buildString(d.deploy)}"

  def buildString(b: ByteString): String =
    limit(Base16.encode(b.toByteArray), 10)

  def buildStringSig(b: ByteString): String = {
    val bytes = b.toByteArray
    val str1  = Base16.encode(bytes.take(10))
    val str2  = Base16.encode(bytes.takeRight(10))
    s"${str1}...${str2}"
  }

  def buildString(sd: Signed[DeployData]): String =
    s"${buildString(sd.data)}, Sig: ${buildStringSig(sd.sig)}, SigAlgorithm: ${sd.sigAlgorithm.name}, ValidAfterBlockNumber: ${sd.data.validAfterBlockNumber}"

  def buildString(d: DeployData): String =
    s"DeployData #${d.timestamp} -- ${d.term}"

  def buildString(r: RChainState): String =
    buildString(r.postStateHash)

  def buildString(hashes: Traversable[BlockHash]): String =
    hashes.map(PrettyPrinter.buildString).mkString("[", " ", "]")
}
