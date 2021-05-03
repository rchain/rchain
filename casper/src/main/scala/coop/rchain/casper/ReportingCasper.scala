package coop.rchain.casper

import java.nio.file.{Files, Path}
import cats.data.EitherT
import cats.effect.concurrent.{MVar, MVar2, Ref}
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.{Monad, Parallel}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.casper.ReportingCasper.RhoReportingRspace
import coop.rchain.casper.protocol.{
  BlockMessage,
  ProcessedDeploy,
  ProcessedSystemDeploy,
  SystemDeployData
}
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
import coop.rchain.rspace.{Blake2b256Hash, RSpace, ReportingRspace, Match => RSpaceMatch}
import coop.rchain.shared.Log
import monix.execution.atomic.AtomicAny
import coop.rchain.models.Validator.Validator

import scala.concurrent.ExecutionContext
import coop.rchain.metrics.MetricsSemaphore
import coop.rchain.rspace.RSpace.RSpaceStore

import scala.collection.concurrent.TrieMap

sealed trait ReportError
final case class ReportReplayError(error: ReplayFailure) extends ReportError
final case class DeployReportResult(
    processedDeploy: ProcessedDeploy,
    events: Seq[Seq[ReportingEvent]]
)
final case class SystemDeployReportResult(
    processedSystemDeploy: SystemDeployData,
    events: Seq[Seq[ReportingEvent]]
)
final case class ReplayResult(
    deployReportResult: List[DeployReportResult],
    systemDeployReportResult: List[SystemDeployReportResult],
    postStateHash: ByteString
)

trait ReportingCasper[F[_]] {
  def trace(
      block: BlockMessage
  ): F[Either[ReportError, ReplayResult]]
}

object ReportingCasper {
  def noop[F[_]: Sync]: ReportingCasper[F] = new ReportingCasper[F] {

    override def trace(
        block: BlockMessage
    ): F[Either[ReportError, ReplayResult]] =
      Sync[F].delay(Right(ReplayResult(List.empty, List.empty, ByteString.copyFromUtf8("empty"))))
  }

  type RhoReportingRspace[F[_]] =
    ReportingRspace[F, Par, BindPattern, ListParWithRandom, TaggedContinuation]

  def rhoReporter[F[_]: ContextShift: Concurrent: Log: Metrics: Span: Parallel: BlockStore: BlockDagStorage](
      rspaceStore: RSpaceStore[F]
  )(implicit scheduler: ExecutionContext): ReportingCasper[F] =
    new ReportingCasper[F] {
      override def trace(
          block: BlockMessage
      ): F[Either[ReportError, ReplayResult]] =
        for {
          reportingRspace  <- ReportingRuntime.setupReportingRSpace(rspaceStore)
          reportingRuntime <- ReportingRuntime.createReportingRuntime(reportingRspace)
          dag              <- BlockDagStorage[F].getRepresentation
          // TODO approvedBlock is not equal to genesisBlock
          genesis          <- BlockStore[F].getApprovedBlock
          isGenesis        = genesis.exists(a => block.blockHash == a.candidate.block.blockHash)
          invalidBlocksSet <- dag.invalidBlocks
          invalidBlocks = invalidBlocksSet
            .map(block => (block.blockHash, block.sender))
            .toMap
          preStateHash = ProtoUtil.preStateHash(block)
          blockdata    = BlockData.fromBlock(block)
          _            <- reportingRuntime.setBlockData(blockdata)
          _            <- reportingRuntime.setInvalidBlocks(invalidBlocks)
          res <- replayDeploys(
                  reportingRuntime,
                  preStateHash,
                  block.body.deploys,
                  block.body.systemDeploys,
                  !isGenesis,
                  blockdata
                )
          result <- res match {
                     case Left(replayError) =>
                       Log[F].info(
                         s"Relay ${PrettyPrinter.buildStringNoLimit(block.blockHash)} error ${replayError} from reporting"
                       ) >> Concurrent[F].delay(
                         ReportReplayError(replayError)
                           .asLeft[ReplayResult]
                       )
                     case Right(r) =>
                       for {
                         _ <- Log[F].info(
                               s"Cache ${PrettyPrinter.buildStringNoLimit(block.blockHash)} reporting data into mem."
                             )
                       } yield r.asRight[ReportError]
                   }
        } yield result

      private def replayDeploys(
          runtime: ReportingRuntime[F],
          startHash: StateHash,
          terms: Seq[ProcessedDeploy],
          systemDeploys: Seq[ProcessedSystemDeploy],
          withCostAccounting: Boolean,
          blockData: BlockData
      ): F[Either[ReplayFailure, ReplayResult]] =
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
                  } yield DeployReportResult(term, r)
                })
          sysRes <- EitherT.right(
                     systemDeploys.toList.traverse { term =>
                       for {
                         rd <- runtime
                                .replaySystemDeployE(blockData)(term)
                                .map(_.semiflatMap(_ => runtime.getReport))
                         res <- rd.value
                         r = res match {
                           case Left(_)  => Seq.empty[Seq[ReportingEvent]]
                           case Right(s) => s
                         }
                       } yield SystemDeployReportResult(term.systemDeploy, r)
                     }
                   )
          checkPoint <- EitherT.right[ReplayFailure](runtime.createCheckpoint)
          result <- EitherT.right[ReplayFailure](
                     ReplayResult(res, sysRes, checkPoint.root.toByteString).pure[F]
                   )
        } yield result).value
    }

}

class ReportingRuntime[F[_]: Sync: Span](
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
      store: RSpaceStore[F]
  )(
      implicit scheduler: ExecutionContext
  ): F[RhoReportingRspace[F]] = {

    import coop.rchain.rholang.interpreter.storage._
    implicit val m: RSpaceMatch[F, BindPattern, ListParWithRandom] = matchListPar[F]

    for {
      history                          <- RSpace.setUp[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](store)
      (historyRepository, replayStore) = history
      reportingRspace = new ReportingRspace[
        F,
        Par,
        BindPattern,
        ListParWithRandom,
        TaggedContinuation
      ](
        historyRepository,
        AtomicAny(replayStore)
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
