package coop.rchain.casper

import java.nio.file.{Files, Path}

import cats.data.EitherT
import cats.effect.concurrent.{MVar, MVar2, Ref}
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.{Monad, Parallel}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.casper.ReportingCasper.RhoReportingRspace
import coop.rchain.casper.protocol.{BlockMessage, ProcessedDeploy, ProcessedSystemDeploy}
import coop.rchain.casper.util.{EventConverter, ProtoUtil}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.rholang.ReplayFailure
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash._
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.RholangMetricsSource
import coop.rchain.rholang.interpreter.RhoRuntime.{bootstrapRegistry, createRhoEnv}
import coop.rchain.rholang.interpreter.SystemProcesses.{BlockData, Definition, InvalidBlocks}
import coop.rchain.rholang.interpreter.accounting.{_cost, Cost, CostAccounting}
import coop.rchain.rholang.interpreter.storage._
import coop.rchain.rholang.interpreter.{Reduce, ReplayRhoRuntimeImpl}
import coop.rchain.rspace.ReportingRspace.ReportingEvent
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.{Blake2b256Hash, RSpace, ReportingRspace, Match => RSpaceMatch}
import coop.rchain.shared.Log
import monix.execution.atomic.AtomicAny
import coop.rchain.models.Validator.Validator

import scala.concurrent.ExecutionContext
import coop.rchain.metrics.MetricsSemaphore
import coop.rchain.store.KeyValueStoreManager

import scala.collection.concurrent.TrieMap

sealed trait ReportError
final case class ReportBlockNotFound(hash: BlockHash)    extends ReportError
final case class ReportReplayError(error: ReplayFailure) extends ReportError

trait ReportingCasper[F[_]] {
  def trace(
      hash: BlockHash
  ): F[Either[ReportError, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]]
}

object ReportingCasper {
  def noop[F[_]: Sync]: ReportingCasper[F] = new ReportingCasper[F] {

    override def trace(
        hash: BlockHash
    ): F[Either[ReportError, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]] =
      Sync[F].delay(Right(List.empty[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]))
  }

  type RhoReportingRspace[F[_]] =
    ReportingRspace[F, Par, BindPattern, ListParWithRandom, TaggedContinuation]

  def rhoReporter[F[_]: ContextShift: Concurrent: Log: Metrics: Span: Parallel: BlockStore: BlockDagStorage](
      store: ReportMemStore[F],
      keyValueStoreManager: KeyValueStoreManager[F],
      dataDir: Path,
      mapSize: Long
  )(implicit scheduler: ExecutionContext): ReportingCasper[F] =
    new ReportingCasper[F] {
      implicit val source = Metrics.Source(CasperMetricsSource, "report-replay")
      implicit val kvm    = keyValueStoreManager

      val blockLockMap = TrieMap[BlockHash, (MetricsSemaphore[F], Boolean)]()

      private def replayGetReport(
          block: BlockMessage
      ): F[Either[ReportError, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]] =
        for {
          reportingRspace  <- ReportingRuntime.setupReportingRSpace(dataDir, mapSize)
          reportingRuntime <- ReportingRuntime.createReportingRuntime(reportingRspace)
          dag              <- BlockDagStorage[F].getRepresentation
          genesis          <- BlockStore[F].getApprovedBlock
          isGenesis        = genesis.exists(a => block.blockHash == a.candidate.block.blockHash)
          invalidBlocksSet <- dag.invalidBlocks
          invalidBlocks = invalidBlocksSet
            .map(block => (block.blockHash, block.sender))
            .toMap
          preStateHash = ProtoUtil.preStateHash(block)
          _            <- reportingRuntime.setBlockData(BlockData.fromBlock(block))
          _            <- reportingRuntime.setInvalidBlocks(invalidBlocks)
          res          <- replayDeploys(reportingRuntime, preStateHash, block.body.deploys, !isGenesis)
          result <- res match {
                     case Left(replayError) =>
                       Log[F].info(
                         s"Relay ${PrettyPrinter.buildStringNoLimit(block.blockHash)} error ${replayError} from reporting"
                       ) >> Concurrent[F].delay(
                         ReportReplayError(replayError)
                           .asLeft[List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]
                       )
                     case Right(r) =>
                       for {
                         _ <- Log[F].info(
                               s"Cache ${PrettyPrinter.buildStringNoLimit(block.blockHash)}reporting data into mem."
                             )
                         _ <- r.traverse(data => store.put(data._1.deploy.sig, data._2))
                       } yield r.asRight[ReportError]
                   }
        } yield result

      private def traceBlock(
          hash: BlockHash
      ): F[Either[ReportError, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]] =
        for {
          maybeBlock <- BlockStore[F].get(hash)
          _          <- Log[F].info(s"trace block ${maybeBlock}")
          result <- maybeBlock match {
                     case None =>
                       Concurrent[F].delay(
                         ReportBlockNotFound(hash)
                           .asLeft[List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]
                       )
                     case Some(block) =>
                       for {
                         cached <- block.body.deploys.traverse(
                                    pd =>
                                      for {
                                        data <- store.get(pd.deploy.sig)
                                        re   = data.map((pd, _))
                                      } yield re
                                  )
                         maybeCached = cached.sequence
                         outcome <- maybeCached match {
                                     case None =>
                                       for {
                                         _ <- Log[F].info(
                                               s"No ${PrettyPrinter.buildStringNoLimit(block.blockHash)} reporting data in cached, going to replay"
                                             )
                                         result <- replayGetReport(block)
                                       } yield result
                                     case Some(cached) =>
                                       for {
                                         _ <- Log[F].info(
                                               s"Find ${PrettyPrinter.buildStringNoLimit(block.blockHash)} reporting data in cached"
                                             )
                                       } yield cached.asRight[ReportError]
                                   }
                       } yield outcome
                   }
        } yield result

      override def trace(
          hash: BlockHash
      ): F[Either[ReportError, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]] =
        for {
          semaphore    <- MetricsSemaphore.single
          lockWithDone = blockLockMap.getOrElseUpdate(hash, (semaphore, false))
          result <- if (lockWithDone._2) {
                     traceBlock(hash)
                   } else {
                     lockWithDone._1.withPermit[Either[ReportError, List[
                       (ProcessedDeploy, Seq[Seq[ReportingEvent]])
                     ]]](for {
                       re <- traceBlock(hash)
                       _  = blockLockMap.update(hash, (lockWithDone._1, true))
                     } yield re)
                   }
        } yield result

      private def replayDeploys(
          runtime: ReportingRuntime[F],
          startHash: StateHash,
          terms: Seq[ProcessedDeploy],
          withCostAccounting: Boolean
      ): F[Either[ReplayFailure, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]] =
        (for {
          _ <- EitherT.right(runtime.reset(Blake2b256Hash.fromByteString(startHash)))
          res <- EitherT.right(terms.toList.traverse { term =>
                  for {
                    rd <- runtime
                           .replayDeployE(withCostAccounting)(term)
                           .map(_.semiflatMap(_ => runtime.getReport))
                    res <- rd.value
                    r = res match {
                      case Left(_)  => Seq.empty[Seq[ReportingEvent]]
                      case Right(s) => s
                    }
                  } yield (term, r)
                })
          _ <- EitherT.right[ReplayFailure](runtime.createCheckpoint)
        } yield res).value
    }

}

class ReportingRuntime[F[_]: Sync](
    override val reducer: Reduce[F],
    override val space: RhoReportingRspace[F],
    override val cost: _cost[F],
    override val blockDataRef: Ref[F, BlockData],
    override val invalidBlocksParam: InvalidBlocks[F]
) extends ReplayRhoRuntimeImpl[F](reducer, space, cost, blockDataRef, invalidBlocksParam) {
  def getReport: F[Seq[Seq[ReportingEvent]]] = space.getReport
}

object ReportingRuntime {
  implicit val RuntimeMetricsSource: Source =
    Metrics.Source(RholangMetricsSource, "reportingRuntime")

  def setupReportingRSpace[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span](
      dataDir: Path,
      mapSize: Long
  )(
      implicit scheduler: ExecutionContext,
      kvm: KeyValueStoreManager[F]
  ): F[RhoReportingRspace[F]] = {

    import coop.rchain.rholang.interpreter.storage._
    implicit val m: RSpaceMatch[F, BindPattern, ListParWithRandom] = matchListPar[F]

    def checkCreateDataDir: F[Unit] =
      for {
        notexists <- Sync[F].delay(Files.notExists(dataDir))
        _         <- Sync[F].delay(Files.createDirectories(dataDir)).whenA(notexists)
      } yield ()

    for {
      _ <- checkCreateDataDir
      history <- RSpace.setUp[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                  dataDir,
                  mapSize,
                  Branch.MASTER
                )
      (historyRepository, replayStore) = history
      reportingRspace = new ReportingRspace[
        F,
        Par,
        BindPattern,
        ListParWithRandom,
        TaggedContinuation
      ](
        historyRepository,
        AtomicAny(replayStore),
        Branch.REPLAY
      )
    } yield reportingRspace
  }

  def createReportingRuntime[F[_]: Concurrent: Log: Metrics: Span: Parallel](
      reporting: RhoReportingRspace[F],
      extraSystemProcesses: Seq[Definition[F]] = Seq.empty
  ): F[ReportingRuntime[F]] =
    for {
      cost <- CostAccounting.emptyCost[F]
      rhoEnv <- {
        implicit val c = cost
        createRhoEnv(reporting, extraSystemProcesses)
      }
      (reducer, blockRef, invalidBlocks) = rhoEnv
      runtime                            = new ReportingRuntime[F](reducer, reporting, cost, blockRef, invalidBlocks)
      _                                  <- bootstrapRegistry(runtime)
    } yield runtime
}
