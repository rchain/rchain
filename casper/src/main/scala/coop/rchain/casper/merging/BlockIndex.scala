package coop.rchain.casper.merging

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.casper.merging.DeployIndex._
import coop.rchain.casper.protocol.ProcessedSystemDeploy.Succeeded
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.EventConverter
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.merger.MergingLogic.computeRelatedSets
import coop.rchain.rspace.merger._
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.trace.Produce

import scala.collection.concurrent.TrieMap

final case class BlockIndex(blockHash: BlockHash, deployChains: Vector[DeployChainIndex])

object BlockIndex {

  // TODO make proper storage for block indices
  val cache = TrieMap.empty[BlockHash, BlockIndex]

  def createEventLogIndex[F[_]: Concurrent, C, P, A, K](
      events: List[Event],
      historyRepository: HistoryRepository[F, C, P, A, K],
      preStateHash: Blake2b256Hash
  ): F[EventLogIndex] = {
    val preStateReader =
      historyRepository.getHistoryReader(preStateHash)
    val produceExistsInPreState = (p: Produce) =>
      preStateReader.getData(p.channelsHash).map(_.exists(_.source == p))
    val produceTouchesPreStateJoin = (p: Produce) =>
      preStateReader.getJoins(p.channelsHash).map(_.exists(_.size > 1))
    EventLogIndex.apply(
      events.map(EventConverter.toRspaceEvent),
      produceExistsInPreState,
      produceTouchesPreStateJoin
    )
  }

  def apply[F[_]: Concurrent, C, P, A, K](
      blockHash: BlockHash,
      usrProcessedDeploys: List[ProcessedDeploy],
      sysProcessedDeploys: List[ProcessedSystemDeploy],
      preStateHash: Blake2b256Hash,
      postStateHash: Blake2b256Hash,
      indexDeployLogF: (List[Event], Blake2b256Hash) => F[EventLogIndex],
      computeStateChangeF: (EventLogIndex, Blake2b256Hash, Blake2b256Hash) => F[StateChange]
  ): F[BlockIndex] =
    for {
      usrDeployIndices <- usrProcessedDeploys.toVector
                           .filterNot(_.isFailed)
                           .traverse { d =>
                             DeployIndex(
                               d.deploy.sig,
                               d.cost.cost,
                               d.deployLog,
                               indexDeployLogF(_, preStateHash)
                             )
                           }
      sysDeploysData = sysProcessedDeploys.toVector
        .collect {
          case Succeeded(log, SlashSystemDeployData(_, _)) =>
            (SYS_SLASH_DEPLOY_ID, SYS_SLASH_DEPLOY_COST, log)
          case Succeeded(log, CloseBlockSystemDeployData) =>
            (SYS_CLOSE_BLOCK_DEPLOY_ID, SYS_CLOSE_BLOCK_DEPLOY_COST, log)
          case Succeeded(log, Empty) =>
            (SYS_EMPTY_DEPLOY_ID, SYS_EMPTY_DEPLOY_COST, log)
        }
      sysDeployIndices <- sysDeploysData.traverse {
                           case (sig, cost, log) =>
                             DeployIndex(
                               sig,
                               cost,
                               log,
                               indexDeployLogF(_, preStateHash)
                             )
                         }

      /** Here deploys from a single block are examined. Atm deploys in block are executed sequentially,
        * so all conflicts are resolved according to order of sequential execution.
        * Therefore there won't be any conflicts between event logs. But there can be dependencies. */
      deployChains = computeRelatedSets[DeployIndex](
        (usrDeployIndices ++ sysDeployIndices).toSet,
        (l, r) => MergingLogic.depends(l.eventLogIndex, r.eventLogIndex)
      )
      index <- deployChains.toVector.traverse(
                DeployChainIndex(_, preStateHash, postStateHash, computeStateChangeF)
              )
    } yield BlockIndex(blockHash, index)
}
