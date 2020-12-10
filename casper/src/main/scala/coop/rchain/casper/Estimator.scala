package coop.rchain.casper

import scala.collection.immutable.{Map, Set}
import cats.Monad
import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.weightFromValidatorByDag
import coop.rchain.catscontrib.ListContrib
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockMetadata
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.shared.{DagOps, Log}

// Tips of the DAG, ranked against LCA
final case class ForkChoice(tips: IndexedSeq[BlockHash], lca: BlockHash)

final class Estimator[F[_]: Sync: Log: Metrics: Span](
    maxNumberOfParents: Int,
    maxParentDepthOpt: Option[Int]
) {

  implicit val decreasingOrder = Ordering.Tuple2(
    Ordering[Long].reverse,
    Ordering.by((b: ByteString) => b.toByteArray.toIterable)
  )

  private val EstimatorMetricsSource: Metrics.Source =
    Metrics.Source(CasperMetricsSource, "estimator")
  private val Tips0MetricsSource: Metrics.Source = Metrics.Source(EstimatorMetricsSource, "tips0")
  private val Tips1MetricsSource: Metrics.Source = Metrics.Source(EstimatorMetricsSource, "tips1")

  def tips(
      dag: BlockDagRepresentation[F],
      genesis: BlockMessage
  ): F[ForkChoice] =
    Span[F].trace(Tips0MetricsSource) {
      for {
        latestMessageHashes <- dag.latestMessageHashes
        _                   <- Span[F].mark("latest-message-hashes")
        result              <- tips(dag, genesis, latestMessageHashes)
      } yield result
    }

  /**
    * When the BlockDag has an empty latestMessages, tips will return IndexedSeq(genesis.blockHash)
    */
  def tips(
      dag: BlockDagRepresentation[F],
      genesis: BlockMessage,
      latestMessagesHashes: Map[Validator, BlockHash]
  ): F[ForkChoice] = Span[F].trace(Tips1MetricsSource) {
    for {
      invalidLatestMessages        <- dag.invalidLatestMessages(latestMessagesHashes)
      filteredLatestMessagesHashes = latestMessagesHashes -- invalidLatestMessages.keys
      lca <- calculateLCA(
              dag,
              BlockMetadata.fromBlock(genesis, false),
              filteredLatestMessagesHashes
            )
      _                          <- Span[F].mark("lca")
      scoresMap                  <- buildScoresMap(dag, filteredLatestMessagesHashes, lca)
      _                          <- Span[F].mark("score-map")
      rankedLatestMessagesHashes <- rankForkchoices(List(lca), dag, scoresMap)
      _                          <- Span[F].mark("ranked-latest-messages-hashes")
      rankedShallowHashes        <- filterDeepParents(rankedLatestMessagesHashes, dag)
      _                          <- Span[F].mark("filtered-deep-parents")
    } yield ForkChoice(rankedShallowHashes.take(maxNumberOfParents), lca)
  }

  private def filterDeepParents(
      rankedLatestHashes: Vector[BlockHash],
      dag: BlockDagRepresentation[F]
  ): F[Vector[BlockHash]] =
    maxParentDepthOpt match {
      case Some(maxParentDepth) =>
        val mainHash +: secondaryHashes = rankedLatestHashes

        for {
          maxBlockNumber   <- dag.lookupUnsafe(mainHash).map(_.blockNum)
          secondaryParents <- secondaryHashes.traverse(dag.lookupUnsafe)
          shallowParents = secondaryParents.filter(
            p => maxBlockNumber - p.blockNum <= maxParentDepth
          )
        } yield mainHash +: shallowParents.map(_.blockHash)
      case None =>
        rankedLatestHashes.pure[F]
    }

  private def calculateLCA(
      blockDag: BlockDagRepresentation[F],
      genesis: BlockMetadata,
      latestMessagesHashes: Map[Validator, BlockHash]
  ): F[BlockHash] =
    for {
      latestMessages <- latestMessagesHashes.values.toStream
                         .traverse(hash => blockDag.lookup(hash))
                         .map(_.flatten)
      topBlockNumber <- blockDag.latestBlockNumber
      filteredLM = latestMessages.filter(
        _.blockNum > topBlockNumber - Estimator.latestMessageMaxDepth
      )
      result <- if (filteredLM.isEmpty) {
                 genesis.blockHash.pure[F]
               } else {
                 filteredLM
                   .foldM(filteredLM.head) {
                     case (acc, latestMessage) =>
                       // TODO: Change to mainParentLCA
                       DagOperations.lowestUniversalCommonAncestorF[F](
                         acc,
                         latestMessage,
                         blockDag
                       )
                   }
                   .map(_.blockHash)
               }
    } yield result

  private def buildScoresMap(
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
        result <- DagOps
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

  private def rankForkchoices(
      blocks: List[BlockHash],
      blockDag: BlockDagRepresentation[F],
      scores: Map[BlockHash, Long]
  ): F[Vector[BlockHash]] =
    // TODO: This ListContrib.sortBy will be improved on Thursday with Pawels help
    for {
      unsortedNewBlocks <- blocks.flatTraverse(replaceBlockHashWithChildren(_, blockDag, scores))
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
  private def replaceBlockHashWithChildren(
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

object Estimator {
  val UnlimitedParents = Int.MaxValue

  // Latest messages with block number more then this value before the tip of the DAG
  // won't be taken into account when calculating LCA
  // TODO remove when proper fix for https://github.com/rchain/rchain/issues/3094 is implemented
  //  Have to be removed before block merge
  val latestMessageMaxDepth = 1000L

  def apply[F[_]](implicit ev: Estimator[F]): Estimator[F] =
    ev

  def apply[F[_]: Sync: Log: Metrics: Span](
      maxNumberOfParents: Int,
      maxParentDepthOpt: Option[Int]
  ): Estimator[F] =
    new Estimator(maxNumberOfParents, maxParentDepthOpt)
}
