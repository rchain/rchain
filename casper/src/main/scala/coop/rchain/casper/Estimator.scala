package coop.rchain.casper

import cats.Monad
import cats.implicits._
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.util.BlockMessageUtil
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockStore}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.weightFromValidatorByDag

import scala.collection.immutable.{Map, Set}
import coop.rchain.catscontrib.ListContrib

object Estimator {
  type BlockHash = ByteString
  type Validator = ByteString

  implicit val decreasingOrder = Ordering[Long].reverse

  def tips[F[_]: Monad: BlockStore](
      blockDag: BlockDagRepresentation[F],
      genesis: BlockMessage
  ): F[IndexedSeq[BlockMessage]] =
    for {
      latestMessageHashes <- blockDag.latestMessageHashes
      result              <- Estimator.tips[F](blockDag, genesis, latestMessageHashes)
    } yield result

  /**
    * When the BlockDag has an empty latestMessages, tips will return IndexedSeq(genesis)
    *
    * TODO: Remove lastFinalizedBlockHash in follow up PR
    */
  def tips[F[_]: Monad: BlockStore](
      blockDag: BlockDagRepresentation[F],
      genesis: BlockMessage,
      latestMessagesHashes: Map[Validator, BlockHash]
  ): F[IndexedSeq[BlockMessage]] =
    for {
      gca       <- calculateGca(blockDag, genesis, latestMessagesHashes)
      scoresMap <- buildScoresMap(blockDag, latestMessagesHashes, gca)
      sortedChildrenHash <- sortChildren(
                             List(gca),
                             blockDag,
                             scoresMap
                           )
      maybeSortedChildren <- sortedChildrenHash.traverse(BlockStore[F].get)
      sortedChildren      = maybeSortedChildren.flatten.toVector
    } yield sortedChildren

  private def calculateGca[F[_]: Monad: BlockStore](
      blockDag: BlockDagRepresentation[F],
      genesis: BlockMessage,
      latestMessagesHashes: Map[Validator, BlockHash]
  ): F[BlockHash] =
    for {
      latestMessages <- latestMessagesHashes.values.toStream
                         .traverse(hash => ProtoUtil.unsafeGetBlock[F](hash))
      result <- if (latestMessages.isEmpty) {
                 genesis.pure[F]
               } else {
                 latestMessages.foldM(latestMessages.head) {
                   case (acc, latestMessage) =>
                     DagOperations.greatestCommonAncestorF[F](acc, latestMessage, genesis, blockDag)
                 }
               }
    } yield result.blockHash

  private def buildScoresMap[F[_]: Monad](
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
                case (acc2, childHash) =>
                  for {
                    blockMetadataOpt <- blockDag.lookup(childHash)
                    result = blockMetadataOpt match {
                      case Some(blockMetaData)
                          if blockMetaData.parents.size > 1 && blockMetaData.sender != validator =>
                        val currScore       = acc2.getOrElse(childHash, 0L)
                        val validatorWeight = blockMetaData.weightMap.getOrElse(validator, 0L)
                        acc2.updated(childHash, currScore + validatorWeight)
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

  private def sortChildren[F[_]: Monad](
      blocks: List[BlockHash],
      blockDag: BlockDagRepresentation[F],
      scores: Map[BlockHash, Long]
  ): F[List[BlockHash]] =
    // TODO: This ListContrib.sortBy will be improved on Thursday with Pawels help
    for {
      unsortedNewBlocks <- blocks.flatTraverse(replaceBlockHashWithChildren[F](_, blockDag, scores))
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
  private def replaceBlockHashWithChildren[F[_]: Monad](
      b: BlockHash,
      blockDag: BlockDagRepresentation[F],
      scores: Map[BlockHash, Long]
  ): F[List[BlockHash]] =
    for {
      children <- blockDag
                   .children(b)
                   .map(maybeChildren => maybeChildren.getOrElse(Set.empty[BlockHash]))
      scoredChildren = children.filter(scores.contains)
      result = if (scoredChildren.nonEmpty) {
        scoredChildren.toList
      } else {
        List(b)
      }
    } yield result

  private def stillSame(blocks: List[BlockHash], newBlocks: List[BlockHash]): Boolean =
    newBlocks == blocks
}
