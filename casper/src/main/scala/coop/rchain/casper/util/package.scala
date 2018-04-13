package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Bond, RChainState}

import scala.annotation.tailrec
import scala.collection

package object util {
  /*
   * c is in the blockchain of b iff c == b or c is in the blockchain of the main parent of b
   */
  // TODO: Move into BlockDAG and remove corresponding param once that is moved over from simulator
  @tailrec
  def isInMainChain(blocks: collection.Map[ByteString, BlockMessage],
                    candidate: BlockMessage,
                    target: BlockMessage): Boolean =
    if (candidate == target) {
      true
    } else {
      val mainParent = for {
        hdr        <- target.header
        parentHash <- hdr.parentsHashList.headOption
        mainParent <- blocks.get(parentHash)
      } yield mainParent
      mainParent match {
        case Some(parent) => isInMainChain(blocks, candidate, parent)
        case None         => false
      }
    }
}
