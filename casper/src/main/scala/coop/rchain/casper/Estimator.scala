package coop.rchain.casper

import cats.{Foldable, Monad}
import cats.implicits._
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.DagOperations
import coop.rchain.casper.util.ProtoUtil.{parentHashes, unsafeGetBlock, weightFromValidator}

import scala.annotation.tailrec
import scala.collection.immutable.{Map, Set}
import coop.rchain.catscontrib.ListContrib
import coop.rchain.shared.StreamT

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
    def hashParents(hash: BlockHash): F[List[BlockHash]] =
      for {
        b <- unsafeGetBlock[F](hash)
      } yield parentHashes(b).toList

    def addValidatorWeightDownSupportingChain(scoreMap: Map[BlockHash, Int],
                                              validator: Validator,
                                              latestBlockHash: BlockHash): F[Map[BlockHash, Int]] =
      for {
        dag <- DagOperations.bfTraverseF[F, BlockHash](List(latestBlockHash))(hashParents).toList
        updatedScoreMap <- Foldable[List].foldM(dag, scoreMap) {
                            case (acc, hash) =>
                              for {
                                b               <- unsafeGetBlock[F](hash)
                                currScore       = acc.getOrElse(hash, 0)
                                validatorWeight <- weightFromValidator[F](b, validator)
                              } yield acc.updated(hash, currScore + validatorWeight)
                          }
      } yield updatedScoreMap

    for {
      scoresMap <- Foldable[List].foldM(blockDag.latestMessages.toList, Map.empty[BlockHash, Int]) {
                    case (acc, (validator: Validator, latestBlockHash: BlockHash)) =>
                      addValidatorWeightDownSupportingChain(acc, validator, latestBlockHash)
                  }
    } yield scoresMap
  }
}
