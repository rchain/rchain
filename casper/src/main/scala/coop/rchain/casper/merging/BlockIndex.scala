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
import coop.rchain.rspace.merger.MergingLogic.{computeRelatedSets, NumberChannelsDiff}
import coop.rchain.rspace.merger._
import coop.rchain.rspace.trace.Produce

import scala.collection.concurrent.TrieMap

final case class BlockIndex(blockHash: BlockHash, deployChains: Vector[DeployChainIndex])

object BlockIndex {

  // TODO make proper storage for block indices
  val cache = TrieMap.empty[BlockHash, BlockIndex]

  def createEventLogIndex[F[_]: Concurrent, C, P, A, K](
      events: List[Event],
      historyRepository: HistoryRepository[F, C, P, A, K],
      preStateHash: Blake2b256Hash,
      mergeableChs: NumberChannelsDiff
  ): F[EventLogIndex] =
    for {
      preStateReader <- historyRepository.getHistoryReader(preStateHash)
      produceExistsInPreState = (p: Produce) =>
        preStateReader.getData(p.channelsHash).map(_.exists(_.source == p))
      produceTouchesPreStateJoin = (p: Produce) =>
        preStateReader.getJoins(p.channelsHash).map(_.exists(_.size > 1))
      eventLogIndex <- EventLogIndex.apply(
                        events.map(EventConverter.toRspaceEvent),
                        produceExistsInPreState,
                        produceTouchesPreStateJoin,
                        mergeableChs
                      )
    } yield eventLogIndex

  def apply[F[_]: Concurrent, C, P, A, K](
      blockHash: BlockHash,
      usrProcessedDeploys: List[ProcessedDeploy],
      sysProcessedDeploys: List[ProcessedSystemDeploy],
      preStateHash: Blake2b256Hash,
      postStateHash: Blake2b256Hash,
      historyRepository: HistoryRepository[F, C, P, A, K],
      mergeableChanData: Seq[NumberChannelsDiff]
  ): F[BlockIndex] = {
    // Connect mergeable channels data with processed deploys by index
    val usrCount    = usrProcessedDeploys.size
    val sysCount    = sysProcessedDeploys.size
    val deployCount = usrCount + sysCount
    val mrgCount    = mergeableChanData.size

    // Number of deploys must match the size of mergeable channels maps
    assert(deployCount == mrgCount, {
      s"Cache of mergeable channels ($mrgCount) doesn't match deploys count ($deployCount)."
    })

    // Connect deploy with corresponding mergeable channels map
    val (usrDeploys, sysDeploys) = mergeableChanData.toVector
      .splitAt(usrCount)
      .bimap(usrProcessedDeploys.toVector.zip(_), sysProcessedDeploys.toVector.zip(_))

    for {
      usrDeployIndices <- usrDeploys
                           .filterNot(_._1.isFailed)
                           .traverse {
                             case (d, mergeChs) =>
                               DeployIndex(
                                 d.deploy.sig,
                                 d.cost.cost,
                                 d.deployLog,
                                 createEventLogIndex(_, historyRepository, preStateHash, mergeChs)
                               )
                           }
      sysDeploysData = sysDeploys
        .collect {
          case (Succeeded(log, SlashSystemDeployData(_, _)), mergeChs) =>
            (blockHash.concat(SYS_SLASH_DEPLOY_ID), SYS_SLASH_DEPLOY_COST, log, mergeChs)
          case (Succeeded(log, CloseBlockSystemDeployData), mergeChs) =>
            (
              blockHash.concat(SYS_CLOSE_BLOCK_DEPLOY_ID),
              SYS_CLOSE_BLOCK_DEPLOY_COST,
              log,
              mergeChs
            )
          case (Succeeded(log, Empty), mergeChs) =>
            (blockHash.concat(SYS_EMPTY_DEPLOY_ID), SYS_EMPTY_DEPLOY_COST, log, mergeChs)
        }
      sysDeployIndices <- sysDeploysData.traverse {
                           case (sig, cost, log, mergeChs) =>
                             DeployIndex(
                               sig,
                               cost,
                               log,
                               createEventLogIndex(
                                 _,
                                 historyRepository,
                                 preStateHash,
                                 mergeChs
                               )
                             )
                         }

      deployIndices = usrDeployIndices ++ sysDeployIndices

      /** Here deploys from a single block are examined. Atm deploys in block are executed sequentially,
        * so all conflicts are resolved according to order of sequential execution.
        * Therefore there won't be any conflicts between event logs. But there can be dependencies. */
      deployChains = computeRelatedSets[DeployIndex](
        deployIndices.toSet,
        (l, r) => MergingLogic.depends(l.eventLogIndex, r.eventLogIndex)
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
