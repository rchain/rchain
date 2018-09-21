package coop.rchain.casper

import cats.{Foldable, Monad}
import cats.implicits._
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.{parentHashes, unsafeGetBlock, weightFromValidator}

import scala.annotation.tailrec
import scala.collection.immutable.{Map, Set}
import coop.rchain.catscontrib.ListContrib
import coop.rchain.shared.StreamT

object Estimator {
  type BlockHash = ByteString
  type Validator = ByteString

  implicit val decreasingOrder = Ordering[Long].reverse

  /**
    * When the BlockDag has an empty latestMessages, tips will return IndexedSeq(genesis)
    */
  def tips[F[_]: Monad: BlockStore](
      blockDag: BlockDag,
      genesis: BlockMessage
  ): F[IndexedSeq[BlockMessage]] = {
    @tailrec
    def sortChildren(
        blocks: IndexedSeq[BlockHash],
        childMap: Map[BlockHash, Set[BlockHash]],
        scores: Map[BlockHash, Long]
    ): IndexedSeq[BlockHash] = {
      // TODO: This ListContrib.sortBy will be improved on Thursday with Pawels help
      val newBlocks =
        ListContrib
          .sortBy[BlockHash, Long](
            blocks.flatMap(replaceBlockHashWithChildren(childMap, _, scores)).distinct,
            scores
          )
      if (stillSame(blocks, newBlocks)) {
        blocks
      } else {
        sortChildren(newBlocks, childMap, scores)
      }
    }

    /**
      * Only include children that have been scored,
      * this ensures that the search does not go beyond
      * the messages defined by blockDag.latestMessages
      */
    def replaceBlockHashWithChildren(
        childMap: Map[BlockHash, Set[BlockHash]],
        b: BlockHash,
        scores: Map[BlockHash, Long]
    ): IndexedSeq[BlockHash] = {
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

  // TODO: Fix to stop at genesis/LFB
  def buildScoresMap[F[_]: Monad: BlockStore](blockDag: BlockDag): F[Map[BlockHash, Long]] = {
    def hashParents(hash: BlockHash): F[List[BlockHash]] =
      for {
        b <- unsafeGetBlock[F](hash)
      } yield parentHashes(b).toList

    def addValidatorWeightDownSupportingChain(
        scoreMap: Map[BlockHash, Long],
        validator: Validator,
        latestBlockHash: BlockHash
    ): F[Map[BlockHash, Long]] =
      for {
        updatedScoreMap <- DagOperations
                            .bfTraverseF[F, BlockHash](List(latestBlockHash))(hashParents)
                            .foldLeftF(scoreMap) {
                              case (acc, hash) =>
                                for {
                                  b               <- unsafeGetBlock[F](hash)
                                  currScore       = acc.getOrElse(hash, 0L)
                                  validatorWeight <- weightFromValidator[F](b, validator)
                                } yield acc.updated(hash, currScore + validatorWeight)
                            }
      } yield updatedScoreMap

    /**
      * Add scores to the blocks implicitly supported through
      * including a latest block as a "step parent"
      *
      * TODO: Add test where this matters
      */
    def addValidatorWeightToImplicitlySupported(
        scoreMap: Map[BlockHash, Long],
        childMap: Map[BlockHash, Set[BlockHash]],
        validator: Validator,
        latestBlockHash: BlockHash
    ) =
      childMap
        .get(latestBlockHash)
        .toList
        .foldM(scoreMap) {
          case (acc, children) =>
            children.filter(scoreMap.contains).toList.foldM(acc) {
              case (acc2, cHash) =>
                for {
                  c <- ProtoUtil.unsafeGetBlock[F](cHash)
                  result = if (ProtoUtil.parentHashes(c).size > 1 && c.sender != validator) {
                    val currScore       = acc2.getOrElse(cHash, 0L)
                    val validatorWeight = ProtoUtil.weightMap(c).getOrElse(validator, 0L)
                    acc2.updated(cHash, currScore + validatorWeight)
                  } else {
                    acc2
                  }
                } yield result
            }
        }

    for {
      scoresMap <- Foldable[List]
                    .foldM(blockDag.latestMessages.toList, Map.empty[BlockHash, Long]) {
                      case (acc, (validator: Validator, latestBlock: BlockMessage)) =>
                        for {
                          postValidatorWeightScoreMap <- addValidatorWeightDownSupportingChain(
                                                          acc,
                                                          validator,
                                                          latestBlock.blockHash
                                                        )
                          postImplicitlySupportedScoreMap <- addValidatorWeightToImplicitlySupported(
                                                              postValidatorWeightScoreMap,
                                                              blockDag.childMap,
                                                              validator,
                                                              latestBlock.blockHash
                                                            )
                        } yield postImplicitlySupportedScoreMap
                    }
    } yield scoresMap
  }
}
