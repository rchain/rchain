package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Resource.ResourceClass.{
  ConsumeResource,
  ProduceResource,
  StoppedResource
}
import coop.rchain.casper.protocol.{BlockMessage, Resource, Stopped}
import scalapb.GeneratedMessage
import coop.rchain.crypto.codec._

class PrettyPrinter {
  def buildString(b: ByteString): String = {
    val MAX_LENGTH = 10
    val str        = Base16.encode(b.toByteArray)
    if (str.length > MAX_LENGTH) {
      str.substring(0, MAX_LENGTH) + "..."
    } else {
      str
    }
  }
  def buildString(t: GeneratedMessage): String =
    t match {
      case b: BlockMessage =>
        val blockString = for {
          header     <- b.header
          mainParent <- header.parentsHashList.headOption
          body       <- b.body
          postState  <- body.postState
        } yield
          s"Block #${postState.blockNumber} (${buildString(b.blockHash)}) " +
            s"-- Creator ID ${buildString(b.sig)} " +
            s"-- M Parent Hash ${buildString(mainParent)} " +
            s"-- Contents ${postState.resources.toList
              .map {
                case Resource(resourceClass) =>
                  resourceClass match {
                    case ProduceResource(produce) => s"Produce ${produce.id}"
                    case ConsumeResource(consume) => s"Consume ${consume.id}"
                    case StoppedResource(_)       => ""
                  }
              }
              .mkString("[", ",", "]")}"
        blockString match {
          case Some(str) => str
          case None      => "Block with missing elements"
        }
      case _ => "Unknown consensus protocol message"
    }
}
