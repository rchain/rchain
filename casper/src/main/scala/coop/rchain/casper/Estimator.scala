package coop.rchain.casper

import cats.Id
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
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

  def tips(blockDag: BlockDag,
           internalMap: Map[BlockHash, BlockMessage],
           genesis: BlockMessage): IndexedSeq[BlockMessage] = {
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

    val scoresMap = buildScoresMap(blockDag, internalMap)
    sortChildren(IndexedSeq(genesis.blockHash), blockDag.childMap, scoresMap)
      .map(internalMap)
  }

  def buildScoresMap(blockDag: BlockDag,
                     internalMap: Map[BlockHash, BlockMessage]): Map[BlockHash, Int] = {
    def hashParents(internalMap: Map[BlockHash, BlockMessage],
                    hash: BlockHash): Iterator[BlockHash] = {
      val b = internalMap(hash)
      parents(b).iterator
    }

    def addValidatorWeightDownSupportingChain(scoreMap: Map[BlockHash, Int],
                                              internalMap: Map[BlockHash, BlockMessage],
                                              validator: Validator,
                                              latestBlockHash: BlockHash) =
      DagOperations
        .bfTraverse[BlockHash](Some(latestBlockHash))(hashParents(internalMap, _))
        .foldLeft(scoreMap) {
          case (acc, hash) =>
            val b         = internalMap(hash)
            val currScore = acc.getOrElse(hash, 0)

            val validatorWeight = weightFromValidator(b, validator, internalMap)

            acc.updated(hash, currScore + validatorWeight)
        }

    //add scores to the blocks implicitly supported through
    //including a latest block as a "step parent"
    def addValidatorWeightToImplicitlySupported(scoreMap: Map[BlockHash, Int],
                                                internalMap: Map[BlockHash, BlockMessage],
                                                childMap: Map[BlockHash, Set[BlockHash]],
                                                validator: Validator,
                                                latestBlockHash: BlockHash) =
      childMap
        .get(latestBlockHash)
        .foldLeft(scoreMap) {
          case (acc, children) =>
            children.filter(scoreMap.contains).foldLeft(acc) {
              case (acc, cHash) =>
                val c = internalMap(cHash)
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
                                       internalMap: Map[BlockHash, BlockMessage],
                                       childMap: Map[BlockHash, Set[BlockHash]],
                                       validator: Validator,
                                       latestBlockHash: BlockHash) = {
      val scoreMap =
        addValidatorWeightDownSupportingChain(acc, internalMap, validator, latestBlockHash)
      addValidatorWeightToImplicitlySupported(scoreMap,
                                              internalMap,
                                              childMap,
                                              validator,
                                              latestBlockHash)
    }

    blockDag.latestMessages.foldLeft(Map.empty[BlockHash, Int]) {
      case (acc, (validator: Validator, latestBlockHash: BlockHash)) =>
        addValidatorWeightToBlockScore(acc,
                                       internalMap,
                                       blockDag.childMap,
                                       validator,
                                       latestBlockHash)
    }
  }
}
