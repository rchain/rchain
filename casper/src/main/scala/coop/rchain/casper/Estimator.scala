package coop.rchain.casper

import cats.Monad
import cats.data.OptionT
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
import coop.rchain.models.BlockMetadata

object Estimator {
  implicit val decreasingOrder = Ordering[Long].reverse

  def tips[F[_]: Monad](
      blockDag: BlockDagRepresentation[F],
      genesis: BlockMessage
  ): F[IndexedSeq[BlockHash]] =
    for {
      latestMessageHashes <- blockDag.latestMessageHashes
      result              <- Estimator.tips[F](blockDag, genesis, latestMessageHashes)
    } yield result

  /**
    * When the BlockDag has an empty latestMessages, tips will return IndexedSeq(genesis.blockHash)
    */
  def tips[F[_]: Monad](
      blockDag: BlockDagRepresentation[F],
      genesis: BlockMessage,
      latestMessagesHashes: Map[Validator, BlockHash]
  ): F[IndexedSeq[BlockHash]] =
    for {
      lca                        <- calculateLCA(blockDag, BlockMetadata.fromBlock(genesis, false), latestMessagesHashes)
      scoresMap                  <- buildScoresMap(blockDag, latestMessagesHashes, lca)
      rankedLatestMessagesHashes <- rankForkchoices(List(lca), blockDag, scoresMap)
    } yield rankedLatestMessagesHashes

  private def calculateLCA[F[_]: Monad](
      blockDag: BlockDagRepresentation[F],
      genesis: BlockMetadata,
      latestMessagesHashes: Map[Validator, BlockHash]
  ): F[BlockHash] =
    for {
      latestMessages <- latestMessagesHashes.values.toStream
                         .traverse(hash => blockDag.lookup(hash))
                         .map(_.flatten)
      result <- if (latestMessages.isEmpty) {
                 genesis.blockHash.pure[F]
               } else {
                 latestMessages
                   .foldM(latestMessages.head) {
                     case (acc, latestMessage) =>
                       // TODO: Change to mainParentLCA
                       DagOperations.lowestCommonAncestorF[F](
                         acc,
                         latestMessage,
                         blockDag
                       )
                   }
                   .map(_.blockHash)
               }
    } yield result

  private def buildScoresMap[F[_]: Monad](
      blockDag: BlockDagRepresentation[F],
      latestMessagesHashes: Map[Validator, BlockHash],
      lowestCommonAncestor: BlockHash
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
        lcaBlockNum <- blockDag.lookup(lowestCommonAncestor).map(_.get.blockNum)
        result <- DagOperations
                   .bfTraverseF[F, BlockHash](List(latestBlockHash))(
                     hashParents(_, lcaBlockNum)
                   )
                   .foldLeftF(scoreMap) {
                     case (acc, hash) =>
                       val currScore = acc.getOrElse(hash, 0L)
                       weightFromValidatorByDag(blockDag, hash, validator).map { validatorWeight =>
                         acc.updated(hash, currScore + validatorWeight)
                       }
                   }
      } yield result

    // TODO: Since map scores are additive it should be possible to do this in parallel
    latestMessagesHashes.toList.foldLeftM(Map.empty[BlockHash, Long]) {
      case (acc, (validator: Validator, latestBlockHash: BlockHash)) =>
        addValidatorWeightDownSupportingChain(
          acc,
          validator,
          latestBlockHash
        )
    }
  }

  private def rankForkchoices[F[_]: Monad](
      blocks: List[BlockHash],
      blockDag: BlockDagRepresentation[F],
      scores: Map[BlockHash, Long]
  ): F[IndexedSeq[BlockHash]] =
    // TODO: This ListContrib.sortBy will be improved on Thursday with Pawels help
    for {
      unsortedNewBlocks <- blocks.flatTraverse(replaceBlockHashWithChildren[F](_, blockDag, scores))
      newBlocks = ListContrib.sortBy[BlockHash, Long](
        unsortedNewBlocks.distinct,
        scores
      )
      result <- if (stillSame(blocks, newBlocks)) {
                 blocks.toVector.pure[F]
               } else {
                 rankForkchoices(newBlocks, blockDag, scores)
               }
    } yield result

  private def toNonEmptyList[T](elements: Set[T]): Option[List[T]] =
    if (elements.isEmpty) None
    else Some(elements.toList)

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
    blockDag
      .children(b)
      .map(
        maybeChildren =>
          maybeChildren
            .flatMap { children =>
              toNonEmptyList(children.filter(scores.contains))
            }
            .getOrElse(List(b))
      )

  private def stillSame(blocks: List[BlockHash], newBlocks: List[BlockHash]): Boolean =
    newBlocks == blocks
}
