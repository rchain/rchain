package coop.rchain.casper

import coop.rchain.casper.protocol.Resource.ResourceClass.{
  ConsumeResource,
  ProduceResource,
  StoppedResource
}
import coop.rchain.casper.protocol.{BlockMessage, Resource, Stopped}
import scalapb.GeneratedMessage

class PrettyPrinter {
  def buildString(t: GeneratedMessage): String =
    t match {
      case b: BlockMessage =>
        val blockString = for {
          header     <- b.header
          mainParent <- header.parentsHashList.headOption
          body       <- b.body
          postState  <- body.postState
        } yield
          s"Block #${postState.blockNumber} (${b.blockHash.toString}) " +
            s"-- creator ${b.sig} " +
            s"-- m parent ${mainParent.toString} " +
            s"-- contents ${postState.resources.toList
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
