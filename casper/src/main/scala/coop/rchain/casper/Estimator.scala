package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.buildScoresMap
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.DagOperations
import coop.rchain.casper.util.ProtoUtil.{mainParent, parents, weightMap}

import scala.annotation.tailrec
import scala.collection.mutable

object Estimator {
  type BlockHash = ByteString
  type Validator = ByteString

  private val decreasingOrder = Ordering[Int].reverse

  def tips(childMap: mutable.HashMap[BlockHash, mutable.HashSet[BlockHash]],
           blockLookup: mutable.HashMap[BlockHash, BlockMessage],
           latestMessages: mutable.HashMap[Validator, BlockHash],
           genesis: BlockMessage): IndexedSeq[BlockMessage] = {
    @tailrec
    def sortChildren[A <: (BlockHash) => Int](blocks: IndexedSeq[BlockHash],
                                              scores: A): IndexedSeq[BlockHash] = {
      val newBlocks =
        blocks.flatMap(replaceBlockHashWithChildren).distinct.sortBy(scores)(decreasingOrder)
      if (stillSame(blocks, newBlocks)) {
        blocks
      } else {
        sortChildren(newBlocks, scores)
      }
    }
    def replaceBlockHashWithChildren(b: BlockHash) = {
      val empty                         = new mutable.HashSet[BlockHash]()
      val c: mutable.HashSet[BlockHash] = childMap.getOrElse(b, empty)
      if (c.nonEmpty) {
        c.toIndexedSeq
      } else {
        IndexedSeq(b)
      }
    }
    def stillSame(blocks: IndexedSeq[BlockHash], newBlocks: IndexedSeq[BlockHash]) =
      newBlocks == blocks

    val scoresMap = buildScoresMap(childMap, blockLookup, latestMessages)
    sortChildren(IndexedSeq(genesis.blockHash), scoresMap).map(blockLookup)
  }

  def buildScoresMap(
      childMap: mutable.HashMap[BlockHash, mutable.HashSet[BlockHash]],
      blockLookup: mutable.HashMap[BlockHash, BlockMessage],
      latestMessages: mutable.HashMap[Validator, BlockHash]): mutable.HashMap[BlockHash, Int] = {
    def hashParents(blockLookup: mutable.HashMap[BlockHash, BlockMessage],
                    hash: BlockHash): Iterator[BlockHash] = {
      val b = blockLookup(hash)
      parents(b).iterator
    }

    val result = new mutable.HashMap[BlockHash, Int]() {
      final override def default(hash: BlockHash): Int = 0
    }

    latestMessages.foreach {
      case (validator, lhash) =>
        //propagate scores for each validator along the dag they support
        DagOperations
          .bfTraverse[BlockHash](Some(lhash))(hashParents(blockLookup, _))
          .foreach(hash => {
            val b         = blockLookup(hash)
            val currScore = result.getOrElse(hash, 0)

            val validatorWeight = mainParent(blockLookup, b)
              .map(weightMap(_).getOrElse(validator, 0))
              .getOrElse(weightMap(b).getOrElse(validator, 0)) //no parents means genesis -- use itself

            result.update(hash, currScore + validatorWeight)
          })

        //add scores to the blocks implicitly supported through
        //including a latest block as a "step parent"
        childMap
          .get(lhash)
          .foreach(children => {
            children.foreach(cHash => {
              val c = blockLookup(cHash)
              if (parents(c).size > 1 && c.sender != validator) {
                val currScore       = result(cHash)
                val validatorWeight = weightMap(c).getOrElse(validator, 0)
                result.update(cHash, currScore + validatorWeight)
              }
            })
          })
    }
    result
  }
}
