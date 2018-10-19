package coop.rchain.casper

import cats.Monad
import cats.implicits._
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockStore}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.DagOperations
import coop.rchain.casper.util.ProtoUtil.weightFromValidatorByDag

import scala.collection.immutable.{Map, Set}
import coop.rchain.catscontrib.ListContrib

object Estimator {
  type BlockHash = ByteString
  type Validator = ByteString

  implicit val decreasingOrder = Ordering[Long].reverse

  def tips[F[_]: Monad: BlockStore](
      blockDag: BlockDagRepresentation[F],
      lastFinalizedBlockHash: BlockHash
  ): F[IndexedSeq[BlockMessage]] =
    for {
      latestMessageHashes <- blockDag.latestMessageHashes
      result              <- Estimator.tips[F](blockDag, lastFinalizedBlockHash, latestMessageHashes)
    } yield result

  /**
    * When the BlockDag has an empty latestMessages, tips will return IndexedSeq(genesis)
    */
  def tips[F[_]: Monad: BlockStore](
      blockDag: BlockDagRepresentation[F],
      lastFinalizedBlockHash: BlockHash,
      latestMessagesHashes: Map[Validator, BlockHash]
  ): F[IndexedSeq[BlockMessage]] = {
    def sortChildren(
        blocks: List[BlockHash],
        blockDag: BlockDagRepresentation[F],
        scores: Map[BlockHash, Long]
    ): F[List[BlockHash]] =
      // TODO: This ListContrib.sortBy will be improved on Thursday with Pawels help
      for {
        unsortedNewBlocks <- blocks.flatTraverse(replaceBlockHashWithChildren(_, blockDag, scores))
        newBlocks = ListContrib.sortBy[BlockHash, Long](
          unsortedNewBlocks.distinct,
          scores
        )
        result <- if (stillSame(blocks, newBlocks)) {
                   blocks.pure[F]
                 } else {
                   sortChildren(newBlocks, blockDag, scores)
                 }
      } yield result

    /**
      * Only include children that have been scored,
      * this ensures that the search does not go beyond
      * the messages defined by blockDag.latestMessages
      */
    def replaceBlockHashWithChildren(
        b: BlockHash,
        blockDag: BlockDagRepresentation[F],
        scores: Map[BlockHash, Long]
    ): F[List[BlockHash]] =
      for {
        c <- blockDag.children(b).map(_.getOrElse(Set.empty[BlockHash]).filter(scores.contains))
      } yield if (c.nonEmpty) c.toList else List(b)

    def stillSame(blocks: List[BlockHash], newBlocks: List[BlockHash]) =
      newBlocks == blocks

    for {
      scoresMap <- buildScoresMap(blockDag, latestMessagesHashes, lastFinalizedBlockHash)
      sortedChildrenHash <- sortChildren(
                             List(lastFinalizedBlockHash),
                             blockDag,
                             scoresMap
                           )
      maybeSortedChildren <- sortedChildrenHash.traverse(BlockStore[F].get)
      sortedChildren      = maybeSortedChildren.flatten.toVector
    } yield sortedChildren
  }

  def buildScoresMap[F[_]: Monad](
      blockDag: BlockDagRepresentation[F],
      latestMessagesHashes: Map[Validator, BlockHash],
      lastFinalizedBlockHash: BlockHash
  ): F[Map[BlockHash, Long]] = {
    def hashParents(hash: BlockHash, lastFinalizedBlockNumber: Long): F[List[BlockHash]] =
      for {
        currentBlockNumber <- blockDag.lookup(hash).map(_.get.blockNum)
        result <- if (currentBlockNumber < lastFinalizedBlockNumber)
                   List.empty[BlockHash].pure[F]
                 else
                   blockDag.lookup(hash).map(_.get.parents)
      } yield result

    def addValidatorWeightDownSupportingChain(
        scoreMap: Map[BlockHash, Long],
        validator: Validator,
        latestBlockHash: BlockHash
    ): F[Map[BlockHash, Long]] =
      for {
        lastFinalizedBlockNum <- blockDag.lookup(lastFinalizedBlockHash).map(_.get.blockNum)
        result <- DagOperations
                   .bfTraverseF[F, BlockHash](List(latestBlockHash))(
                     hashParents(_, lastFinalizedBlockNum)
                   )
                   .foldLeftF(scoreMap) {
                     case (acc, hash) =>
                       val currScore = acc.getOrElse(hash, 0L)
                       weightFromValidatorByDag(blockDag, hash, validator).map { validatorWeight =>
                         acc.updated(hash, currScore + validatorWeight)
                       }
                   }
      } yield result

    /**
      * Add scores to the blocks implicitly supported through
      * including a latest block as a "step parent"
      *
      * TODO: Add test where this matters
      */
    def addValidatorWeightToImplicitlySupported(
        blockDag: BlockDagRepresentation[F],
        scoreMap: Map[BlockHash, Long],
        validator: Validator,
        latestBlockHash: BlockHash
    ): F[Map[BlockHash, Long]] =
      blockDag.children(latestBlockHash).flatMap {
        _.toList
          .foldLeftM(scoreMap) {
            case (acc, children) =>
              children.filter(scoreMap.contains).toList.foldLeftM(acc) {
                case (acc2, cHash) =>
                  for {
                    blockMetadataOpt <- blockDag.lookup(cHash)
                    result = blockMetadataOpt match {
                      case Some(blockMetaData)
                          if blockMetaData.parents.size > 1 && blockMetaData.sender != validator =>
                        val currScore       = acc2.getOrElse(cHash, 0L)
                        val validatorWeight = blockMetaData.weightMap.getOrElse(validator, 0L)
                        acc2.updated(cHash, currScore + validatorWeight)
                      case _ => acc2
                    }
                  } yield result
              }
          }
      }

    latestMessagesHashes.toList.foldLeftM(Map.empty[BlockHash, Long]) {
      case (acc, (validator: Validator, latestBlockHash: BlockHash)) =>
        for {
          postValidatorWeightScoreMap <- addValidatorWeightDownSupportingChain(
                                          acc,
                                          validator,
                                          latestBlockHash
                                        )
          postImplicitlySupportedScoreMap <- addValidatorWeightToImplicitlySupported(
                                              blockDag,
                                              postValidatorWeightScoreMap,
                                              validator,
                                              latestBlockHash
                                            )
        } yield postImplicitlySupportedScoreMap
    }
  }
}
