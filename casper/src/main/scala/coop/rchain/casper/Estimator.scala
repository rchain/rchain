package coop.rchain.casper

import cats.{Foldable, Id, Monad}
import cats.implicits._
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.{parentHashes, weightFromValidatorByDag}

import scala.annotation.tailrec
import scala.collection.immutable.{Map, Set}
import coop.rchain.catscontrib.ListContrib
import coop.rchain.shared.StreamT

object Estimator {
  type BlockHash = ByteString
  type Validator = ByteString

  implicit val decreasingOrder = Ordering[Long].reverse

  def tips[F[_]: Monad: BlockStore](
      blockDag: BlockDag,
      lastFinalizedBlockHash: BlockHash
  ): F[IndexedSeq[BlockMessage]] =
    Estimator.tips[F](blockDag, lastFinalizedBlockHash, blockDag.latestMessages.map {
      case (validator, block) => (validator, block.blockHash)
    })

  /**
    * When the BlockDag has an empty latestMessages, tips will return IndexedSeq(genesis)
    */
  def tips[F[_]: Monad: BlockStore](
      blockDag: BlockDag,
      lastFinalizedBlockHash: BlockHash,
      latestMessagesHashes: Map[Validator, BlockHash]
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
      scoresMap <- buildScoresMap(blockDag, latestMessagesHashes, lastFinalizedBlockHash).pure[F]
      sortedChildrenHash = sortChildren(
        IndexedSeq(lastFinalizedBlockHash),
        blockDag.childMap,
        scoresMap
      )
      maybeSortedChildren <- sortedChildrenHash.toList.traverse(BlockStore[F].get)
      sortedChildren      = maybeSortedChildren.flatten.toVector
    } yield sortedChildren
  }

  def buildScoresMap(
      blockDag: BlockDag,
      latestMessagesHashes: Map[Validator, BlockHash],
      lastFinalizedBlockHash: BlockHash
  ): Map[BlockHash, Long] = {
    def hashParents(hash: BlockHash, lastFinalizedBlockNumber: Long): List[BlockHash] = {
      val currentBlockNumber = blockDag.dataLookup(hash).blockNum
      if (currentBlockNumber < lastFinalizedBlockNumber)
        List.empty[BlockHash]
      else
        blockDag.dataLookup(hash).parents
    }

    def addValidatorWeightDownSupportingChain(
        scoreMap: Map[BlockHash, Long],
        validator: Validator,
        latestBlockHash: BlockHash
    ): Map[BlockHash, Long] =
      DagOperations
        .bfTraverseF[Id, BlockHash](List(latestBlockHash))(
          hashParents(_, blockDag.dataLookup(lastFinalizedBlockHash).blockNum)
        )
        .foldLeft(scoreMap) {
          case (acc, hash) =>
            val currScore = acc.getOrElse(hash, 0L)
            val validatorWeight =
              weightFromValidatorByDag(blockDag, hash, validator)
            acc.updated(hash, currScore + validatorWeight)
        }

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
    ): Map[BlockHash, Long] =
      childMap
        .get(latestBlockHash)
        .toList
        .foldLeft(scoreMap) {
          case (acc, children) =>
            children.filter(scoreMap.contains).toList.foldLeft(acc) {
              case (acc2, cHash) =>
                blockDag.dataLookup.get(cHash) match {
                  case Some(blockMetaData)
                      if blockMetaData.parents.size > 1 && blockMetaData.sender != validator =>
                    val currScore       = acc2.getOrElse(cHash, 0L)
                    val validatorWeight = blockMetaData.weightMap.getOrElse(validator, 0L)
                    acc2.updated(cHash, currScore + validatorWeight)
                  case _ => acc2
                }
            }
        }

    latestMessagesHashes.toList.foldLeft(Map.empty[BlockHash, Long]) {
      case (acc, (validator: Validator, latestBlockHash: BlockHash)) =>
        val postValidatorWeightScoreMap = addValidatorWeightDownSupportingChain(
          acc,
          validator,
          latestBlockHash
        )
        val postImplicitlySupportedScoreMap = addValidatorWeightToImplicitlySupported(
          postValidatorWeightScoreMap,
          blockDag.childMap,
          validator,
          latestBlockHash
        )
        postImplicitlySupportedScoreMap
    }
  }
}
