package coop.rchain.casper

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.DagOperations
import coop.rchain.casper.util.ProtoUtil.{parents, weightFromValidator}

import scala.annotation.tailrec
import scala.collection.immutable.{Map, Set}
import coop.rchain.catscontrib.ListContrib

object Estimator {
  type BlockHash = ByteString
  type Validator = ByteString

  implicit val decreasingOrder = Ordering[Int].reverse

  def tips[F[_]: Monad: BlockStore](blockDag: BlockDag,
                                    genesis: BlockMessage): F[IndexedSeq[BlockMessage]] = {
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
                                     scores: Map[BlockHash, Int]): IndexedSeq[BlockHash] = {
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

    for {
      scoresMap           <- buildScoresMap[F](blockDag)
      sortedChildrenHash  = sortChildren(IndexedSeq(genesis.blockHash), blockDag.childMap, scoresMap)
      maybeSortedChildren <- sortedChildrenHash.toList.traverse(BlockStore[F].get)
      sortedChildren      = maybeSortedChildren.flatten.toVector
    } yield sortedChildren
  }

  def buildScoresMap[F[_]: Monad: BlockStore](blockDag: BlockDag): F[Map[BlockHash, Int]] = {
    def hashParents(internalMap: Map[BlockHash, BlockMessage],
                    hash: BlockHash): Iterator[BlockHash] = {
      val b = internalMap(hash)
      parents(b).iterator
    }

    def addValidatorWeightDownSupportingChain(scoreMap: Map[BlockHash, Int],
                                              internalMap: Map[BlockHash, BlockMessage],
                                              validator: Validator,
                                              latestBlockHash: BlockHash): Map[BlockHash, Int] =
      DagOperations
        .bfTraverse[BlockHash](Some(latestBlockHash))(hashParents(internalMap, _))
        .foldLeft(scoreMap) {
          case (acc, hash) =>
            val b               = internalMap(hash)
            val currScore       = acc.getOrElse(hash, 0)
            val validatorWeight = weightFromValidator(b, validator, internalMap)
            acc.updated(hash, currScore + validatorWeight)
        }

    for {
      internalMap <- BlockStore[F].asMap()
      scoresMap = blockDag.latestMessages.foldLeft(Map.empty[BlockHash, Int]) {
        case (acc, (validator: Validator, latestBlockHash: BlockHash)) =>
          addValidatorWeightDownSupportingChain(acc, internalMap, validator, latestBlockHash)
      }
    } yield scoresMap
  }
}
