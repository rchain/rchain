package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Resource.ResourceClass.{
  ConsumeResource,
  ProduceResource,
  StoppedResource
}
import coop.rchain.casper.protocol._
import scalapb.GeneratedMessage
import coop.rchain.crypto.codec._

object PrettyPrinter {
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
      postState  <- body.postState
    } yield
      s"Block #${postState.blockNumber} (${buildString(b.blockHash)}) " +
        s"-- Sender ID ${buildString(b.sender)} " +
        s"-- M Parent Hash ${buildString(mainParent)} " +
        s"-- Contents ${buildString(postState)}"
    blockString match {
      case Some(str) => str
      case None      => "Block with missing elements"
    }
  }
  private def buildString(b: ByteString): String = {
    val MAX_LENGTH = 10
    val str        = Base16.encode(b.toByteArray)
    if (str.length > MAX_LENGTH) {
      str.substring(0, MAX_LENGTH) + "..."
    } else {
      str
    }
  }
  private def buildString(d: Deploy): String = {
    val deployString = for {
      resource <- d.resource
    } yield
      s"Deploy #${d.nonce} " +
        s"-- ${buildString(resource)}"
    deployString match {
      case Some(str) => str
      case None      => "Deploy with missing elements"
    }
  }
  private def buildString(r: Resource): String =
    r match {
      case Resource(resourceClass) =>
        resourceClass match {
          case ProduceResource(produce) => s"Produce ${produce.id}"
          case ConsumeResource(consume) => s"Consume ${consume.id}"
          case _                        => ""
        }
    }
  private def buildString(r: RChainState): String =
    r.resources.toList
      .map(buildString)
      .mkString("[", ", ", "]")
}
