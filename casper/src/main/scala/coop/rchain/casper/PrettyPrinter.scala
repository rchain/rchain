package coop.rchain.casper

import com.google.protobuf.ByteString

import coop.rchain.casper.protocol._
import scalapb.GeneratedMessage
import coop.rchain.crypto.codec._
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.{PrettyPrinter => RholangPP}

object PrettyPrinter {

  def buildStringNoLimit(b: Array[Byte]): String = Base16.encode(b)
  def buildStringNoLimit(b: ByteString): String  = Base16.encode(b.toByteArray)

  def buildString(t: CasperMessage): String =
    t match {
      case b: BlockMessage => buildString(b)
      case _               => "Unknown consensus protocol message"
    }

  // TODO shouldn header.parentsHashList be nonempty list?
  private def buildString(b: BlockMessage): String = {
    val blockString = for {
      mainParent <- b.header.parentsHashList.headOption
    } yield s"Block #${b.body.state.blockNumber} (${buildString(b.blockHash)}) " +
      s"-- Sender ID ${buildString(b.sender)} " +
      s"-- M Parent Hash ${buildString(mainParent)} " +
      s"-- Contents ${buildString(b.body.state)}" +
      s"-- Shard ID ${limit(b.shardId, 10)}"
    blockString match {
      case Some(str) => str
      case None      => s"Block ${buildString(b.blockHash)} with missing elements"
    }
  }

  private def limit(str: String, maxLength: Int): String =
    if (str.length > maxLength) {
      str.substring(0, maxLength) + "..."
    } else {
      str
    }

  def buildString(d: ProcessedDeploy): String =
    s"User: ${buildStringNoLimit(d.deploy.deployer)}, Cost: ${d.cost.toString} " +
      s"${buildString(d.deploy)}"

  def buildString(b: ByteString): String =
    limit(Base16.encode(b.toByteArray), 10)

  def buildString(d: DeployData): String =
    s"DeployData #${d.timestamp} -- ${d.term}}"

  def buildString(r: RChainState): String =
    buildString(r.postStateHash)

  def buildString(b: Bond): String =
    s"${buildStringNoLimit(b.validator)}: ${b.stake.toString}"
}
