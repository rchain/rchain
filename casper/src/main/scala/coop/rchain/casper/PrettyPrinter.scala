package coop.rchain.casper

import com.google.protobuf.ByteString

import coop.rchain.casper.protocol._
import scalapb.GeneratedMessage
import coop.rchain.crypto.codec._
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.{PrettyPrinter => RholangPP}

object PrettyPrinter {
  private val rpp = RholangPP()

  def buildStringNoLimit(b: ByteString): String = Base16.encode(b.toByteArray)

  def buildString(t: GeneratedMessage): String =
    t match {
      case b: BlockMessage => buildString(b)
      case d: Deploy       => buildString(d)
      case _               => "Unknown consensus protocol message"
    }

  private def buildString(b: BlockMessage): String = {
    val blockString = for {
      header     <- b.header
      mainParent <- header.parentsHashList.headOption
      body       <- b.body
      postState  <- body.state
    } yield
      s"Block #${postState.blockNumber} (${buildString(b.blockHash)}) " +
        s"-- Sender ID ${buildString(b.sender)} " +
        s"-- M Parent Hash ${buildString(mainParent)} " +
        s"-- Contents ${buildString(postState)}" +
        s"-- Shard ID ${limit(b.shardId, 10)}"
    blockString match {
      case Some(str) => str
      case None      => "Block with missing elements"
    }
  }

  private def limit(str: String, maxLength: Int): String =
    if (str.length > maxLength) {
      str.substring(0, maxLength) + "..."
    } else {
      str
    }

  def buildString(b: ByteString): String =
    limit(Base16.encode(b.toByteArray), 10)

  private def buildString(par: Option[Par]): String =
    par.map(p => limit(rpp.buildString(p), 25)).getOrElse("")

  private def buildString(d: Deploy): String =
    s"Deploy #${d.raw.fold(0L)(_.timestamp)} -- ${buildString(d.term)}"

  private def buildString(r: RChainState): String =
    buildString(r.postStateHash)
}
