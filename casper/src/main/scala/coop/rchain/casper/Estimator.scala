package coop.rchain.casper

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.DagOperations
import coop.rchain.casper.util.ProtoUtil.{parents, weightFromValidator, weightMap}

import scala.annotation.tailrec
import scala.collection.immutable.{Map, Set}

import coop.rchain.catscontrib.ListContrib

object Estimator {
  type BlockHash = ByteString
  type Validator = ByteString

  implicit val decreasingOrder = Ordering[Int].reverse

  def tips(blockDag: BlockDag, genesis: BlockMessage): IndexedSeq[BlockMessage] = {
    @tailrec
    def sortChildren(blocks: IndexedSeq[BlockHash],
                     childMap: Map[BlockHash, Set[BlockHash]],
                     scores: Map[BlockHash, Int]): IndexedSeq[BlockHash] = {
      // TODO: This ListContrib.sortBy will be improved on Thursday with Pawels help
      val newBlocks =
        ListContrib
          .sortBy[BlockHash, Int](
            blocks.flatMap(replaceBlockHashWithChildren(childMap, _, scores)).distinct,
            scores)
      if (stillSame(blocks, newBlocks)) {
        blocks
      } else {
        sortChildren(newBlocks, childMap, scores)
      }
    }
    def replaceBlockHashWithChildren(childMap: Map[BlockHash, Set[BlockHash]],
                                     b: BlockHash,
                                     scores: Map[BlockHash, Int]) = {
      //Only include children that have been scored,
      //this ensures that the search does not go beyond
      //the messages defined by blockDag.latestmessages
      val c: Set[BlockHash] = childMap.getOrElse(b, Set.empty[BlockHash]).filter(scores.contains)
      if (c.nonEmpty) {
        c.toIndexedSeq
      } else {
        IndexedSeq(b)
      }
    }
    def stillSame(blocks: IndexedSeq[BlockHash], newBlocks: IndexedSeq[BlockHash]) =
      newBlocks == blocks

    val scoresMap = buildScoresMap(blockDag)
    sortChildren(IndexedSeq(genesis.blockHash), blockDag.childMap, scoresMap)
      .map(blockDag.blockLookup)
  }

  def buildScoresMap(blockDag: BlockDag): Map[BlockHash, Int] = {
    def hashParents(blockLookup: Map[BlockHash, BlockMessage],
                    hash: BlockHash): Iterator[BlockHash] = {
      val b = blockLookup(hash)
      parents(b).iterator
    }

    def addValidatorWeightDownSupportingChain(scoreMap: Map[BlockHash, Int],
                                              blockLookup: Map[BlockHash, BlockMessage],
                                              validator: Validator,
                                              latestBlockHash: BlockHash) =
      DagOperations
        .bfTraverse[BlockHash](Some(latestBlockHash))(hashParents(blockLookup, _))
        .foldLeft(scoreMap) {
          case (acc, hash) =>
            val b         = blockLookup(hash)
            val currScore = acc.getOrElse(hash, 0)

            val validatorWeight = weightFromValidator(b, validator, blockLookup)

            acc.updated(hash, currScore + validatorWeight)
        }

    //add scores to the blocks implicitly supported through
    //including a latest block as a "step parent"
    def addValidatorWeightToImplicitlySupported(scoreMap: Map[BlockHash, Int],
                                                blockLookup: Map[BlockHash, BlockMessage],
                                                childMap: Map[BlockHash, Set[BlockHash]],
                                                validator: Validator,
                                                latestBlockHash: BlockHash) =
      childMap
        .get(latestBlockHash)
        .foldLeft(scoreMap) {
          case (acc, children) =>
            children.filter(scoreMap.contains).foldLeft(acc) {
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

    def addValidatorWeightToBlockScore(acc: Map[BlockHash, Int],
                                       blockLookup: Map[BlockHash, BlockMessage],
                                       childMap: Map[BlockHash, Set[BlockHash]],
                                       validator: Validator,
                                       latestBlockHash: BlockHash) = {
      val scoreMap =
        addValidatorWeightDownSupportingChain(acc, blockLookup, validator, latestBlockHash)
      addValidatorWeightToImplicitlySupported(scoreMap,
                                              blockLookup,
                                              childMap,
                                              validator,
                                              latestBlockHash)
    }

    blockDag.latestMessages.foldLeft(Map.empty[BlockHash, Int]) {
      case (acc, (validator: Validator, latestBlockHash: BlockHash)) =>
        addValidatorWeightToBlockScore(acc,
                                       blockDag.blockLookup,
                                       blockDag.childMap,
                                       validator,
                                       latestBlockHash)
    }
  }
}
