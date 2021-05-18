package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.util.EventConverter
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.merger._
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.trace.Produce

import scala.collection.concurrent.TrieMap

final case class BlockIndex(blockHash: BlockHash, deployChains: Vector[DeployChainIndex])

object BlockIndex {

  // TODO make proper storage for block indices
  val cache = TrieMap.empty[BlockHash, BlockIndex]

  def apply[F[_]: Concurrent, C, P, A, K](
      blockHash: BlockHash,
      processedDeploys: List[ProcessedDeploy],
      preStateHash: StateHash,
      postStateHash: StateHash,
      historyRepository: HistoryRepository[F, C, P, A, K]
  ): F[BlockIndex] = {
    implicit val channelStore: HistoryRepository[F, C, P, A, K] = historyRepository

    val createEventLogIndex = (d: ProcessedDeploy) => {
      val preStateReader =
        historyRepository.getHistoryReader(Blake2b256Hash.fromByteString(preStateHash))
      val produceExistsInPreState = (p: Produce) =>
        preStateReader.getDataFromChannelHash(p.channelsHash).map(_.exists(_.source == p))
      val produceTouchesPreStateJoin = (p: Produce) =>
        preStateReader.getJoinsFromChannelHash(p.channelsHash).map(_.exists(_.size > 1))
      EventLogIndex.apply(
        d.deployLog.map(EventConverter.toRspaceEvent),
        produceExistsInPreState,
        produceTouchesPreStateJoin
      )
    }

    for {
      deployIndices <- processedDeploys
                        .filterNot(_.isFailed)
                        .traverse(DeployIndex(_, createEventLogIndex))

      /** Here deploys from a single block are examined. Atm deploys in block are executed sequentially,
        * so all conflicts are resolved according to order of sequential execution.
        * Therefore there won't be any conflicts between event logs. But there can be dependencies. */
      deployChains = computeRelatedSets[DeployIndex](
        deployIndices,
        (t, s) => MergingLogic.depends(t.eventLogIndex, s.eventLogIndex)
      )
      index <- deployChains.toVector
                .traverse(
                  DeployChainIndex(
                    _,
                    preStateHash,
                    postStateHash,
                    historyRepository
                  )
                )
    } yield BlockIndex(blockHash, index)
  }
}
