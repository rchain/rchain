package coop.rchain.casper

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.DagOperations
import coop.rchain.casper.util.ProtoUtil.{mainParent, parents, weightMap}

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, HashSet}

import coop.rchain.catscontrib.ListContrib

object Estimator {
  type BlockHash = ByteString
  type Validator = ByteString

  implicit val decreasingOrder = Ordering[Int].reverse

  def tips(childMap: HashMap[BlockHash, HashSet[BlockHash]],
           blockLookup: HashMap[BlockHash, BlockMessage],
           latestMessages: HashMap[Validator, BlockHash],
           genesis: BlockMessage): IndexedSeq[BlockMessage] = {
    @tailrec
    def sortChildren(blocks: IndexedSeq[BlockHash],
                     scores: collection.Map[BlockHash, Int]): IndexedSeq[BlockHash] = {
      // TODO: This ListContrib.sortBy will be improved on Thursday with Pawels help
      val newBlocks =
        ListContrib
          .sortBy[BlockHash, Int](blocks.flatMap(replaceBlockHashWithChildren).distinct, scores)
      if (stillSame(blocks, newBlocks)) {
        blocks
      } else {
        sortChildren(newBlocks, scores)
      }
    }
    def replaceBlockHashWithChildren(b: BlockHash) = {
      val empty                 = new HashSet[BlockHash]()
      val c: HashSet[BlockHash] = childMap.getOrElse(b, empty)
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

  def buildScoresMap(childMap: HashMap[BlockHash, HashSet[BlockHash]],
                     blockLookup: HashMap[BlockHash, BlockMessage],
                     latestMessages: HashMap[Validator, BlockHash]): HashMap[BlockHash, Int] = {
    def hashParents(blockLookup: HashMap[BlockHash, BlockMessage],
                    hash: BlockHash): Iterator[BlockHash] = {
      val b = blockLookup(hash)
      parents(b).iterator
    }

    def addValidatorWeightDownSupportingChain(scoreMap: HashMap[BlockHash, Int],
                                              validator: Validator,
                                              latestBlockHash: BlockHash) =
      DagOperations
        .bfTraverse[BlockHash](Some(latestBlockHash))(hashParents(blockLookup, _))
        .foldLeft(scoreMap) {
          case (acc, hash) =>
            val b         = blockLookup(hash)
            val currScore = acc.getOrElse(hash, 0)

            val validatorWeight = mainParent(blockLookup, b)
              .map(weightMap(_).getOrElse(validator, 0))
              .getOrElse(weightMap(b).getOrElse(validator, 0)) //no parents means genesis -- use itself

            acc.updated(hash, currScore + validatorWeight)
        }

    //add scores to the blocks implicitly supported through
    //including a latest block as a "step parent"
    def addValidatorWeightToImplicitlySupported(scoreMap: HashMap[BlockHash, Int],
                                                validator: Validator,
                                                latestBlockHash: BlockHash) =
      childMap
        .get(latestBlockHash)
        .foldLeft(scoreMap) {
          case (acc, children) =>
            children.foldLeft(acc) {
              case (acc, cHash) =>
                val c = blockLookup(cHash)
                if (parents(c).size > 1 && c.sender != validator) {
                  val currScore       = acc.getOrElse(cHash, 0)
                  val validatorWeight = weightMap(c).getOrElse(validator, 0)
                  acc.updated(cHash, currScore + validatorWeight)
                } else {
                  acc
                }
            }
        }

    def addValidatorWeightToBlockScore(acc: HashMap[BlockHash, Int],
                                       validator: Validator,
                                       latestBlockHash: BlockHash) = {
      val scoreMap = addValidatorWeightDownSupportingChain(acc, validator, latestBlockHash)
      addValidatorWeightToImplicitlySupported(scoreMap, validator, latestBlockHash)
    }

    latestMessages.foldLeft(HashMap.empty[BlockHash, Int]) {
      case (acc, (validator: Validator, latestBlockHash: BlockHash)) =>
        addValidatorWeightToBlockScore(acc, validator, latestBlockHash)
    }
  }
}
