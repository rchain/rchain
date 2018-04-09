package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Bond, RChainState}

import scala.annotation.tailrec
import scala.collection.mutable

package object util {
  /*
   * c is in the blockchain of b iff c == b or c is in the blockchain of the main parent of b
   *
   * This implies that the safety oracle must be run from the latest blocks towards the genesis
   * for determining finality or else you might have cases in which blocks that are not in the main DAG
   * will be not finalized.
   */
  // TODO: Move into BlockDAG and remove corresponding param once that is moved over from simulator
  @tailrec
  def isInMainChain(blocks: mutable.Map[ByteString, BlockMessage],
                    candidate: BlockMessage,
                    target: BlockMessage): Boolean =
    if (candidate == target) {
      true
    } else {
      target.header match {
        case Some(header) =>
          val mainParentHash: Option[ByteString] = header.parentsHashList.headOption
          mainParentHash match {
            case Some(parentHash) =>
              val mainParent: BlockMessage = blocks(parentHash)
              isInMainChain(blocks, candidate, mainParent)
            case None =>
              false // No parent blocks (Genesis block)
          }
        case None => false // Should never happen
      }
    }
}
