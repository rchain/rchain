package coop.rchain.casper

import cats.data.EitherT
import cats.{Monad, Parallel}
import cats.effect.concurrent.{MVar, Ref}
import cats.implicits._
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.mtl.FunctorTell
import cats.temp.par
import cats.temp.par.{Par => CatsPar}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.casper.ReportingCasper.RhoReportingRspace
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.{
  InternalError,
  InternalProcessedDeploy,
  ReplayFailure,
  RuntimeManager
}
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.models.BlockHash._
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.rholang.RholangMetricsSource
import coop.rchain.rholang.interpreter.{
  ErrorLog,
  EvaluateResult,
  HasCost,
  Reduce,
  Runtime,
  PrettyPrinter => RhoPrinter
}
import coop.rchain.rholang.interpreter.Runtime.{
  setupMapsAndRefs,
  setupReducer,
  BlockData,
  RhoHistoryRepository,
  SystemProcess
}
import coop.rchain.rholang.interpreter.accounting.{_cost, CostAccounting}
import coop.rchain.rspace.{
  Blake2b256Hash,
  HotStore,
  RSpace,
  ReplayException,
  ReportingRspace,
  Match => RSpaceMatch
}
import coop.rchain.rspace.history.Branch
import monix.execution.atomic.AtomicAny
import coop.rchain.rholang.interpreter.storage._
import coop.rchain.rspace.ReportingRspace.{
  ReportingComm,
  ReportingConsume,
  ReportingEvent,
  ReportingProduce
}
import coop.rchain.shared.Log
import ReportingCasperData._
import cats.syntax.all.none
import coop.rchain.rholang.interpreter.errors.InterpreterError

import scala.concurrent.ExecutionContext

trait ReportingCasper[F[_]] {
  def trace(hash: BlockHash): F[Report]
}

class ReportingEventTransformer[C, P, A, K](
    serializeC: C => String,
    serializeP: P => String,
    serializeA: A => String,
    serializeK: K => String
) {
  type RhoReportingComm    = ReportingComm[C, P, A, K]
  type RhoReportingProduce = ReportingProduce[C, A]
  type RhoReportingConsume = ReportingConsume[C, P, K]

  def serializeConsume(
      rc: RhoReportingConsume
  ): RhoConsume = {
    val k   = serializeK(rc.continuation)
    val chs = rc.channels.map(serializeC).mkString("[", ";", "]")
    val ps  = rc.patterns.map(serializeP).mkString("[", ";", "]")
    RhoConsume(channels = chs, patterns = ps, continuation = k)
  }

  def serializeProduce(rp: RhoReportingProduce): RhoProduce = {
    val d  = serializeA(rp.data)
    val ch = serializeC(rp.channel)
    RhoProduce(channel = ch, data = d)
  }

  def transformEvent(re: ReportingEvent): RhoEvent = re match {
    case comm: RhoReportingComm =>
      RhoComm(serializeConsume(comm.consume), comm.produces.map(serializeProduce).toList)
    case cons: RhoReportingConsume => serializeConsume(cons)
    case prod: RhoReportingProduce => serializeProduce(prod)

  }
}

object ReportingCasperData {
  sealed trait RhoEvent
  final case class RhoComm(consume: RhoConsume, produces: List[RhoProduce]) extends RhoEvent
  final case class RhoProduce(channel: String, data: String)                extends RhoEvent
  final case class RhoConsume(channels: String, patterns: String, continuation: String)
      extends RhoEvent

  final case class DeployTrace(deployHash: String, source: String, events: List[RhoEvent])

  sealed trait Report

  final case class BlockTracesReport(
      hash: String,
      seqNum: Int,
      creator: String,
      traces: List[DeployTrace],
      parents: List[String]
  ) extends Report
  final case class BlockNotFound(hash: String)    extends Report
  final case class BlockReplayError(hash: String) extends Report
  final case class BlockReplayFailed(hash: String, msg: String, deployId: Option[String])
      extends Report

  def transformDeploy[C, P, A, K](transformer: ReportingEventTransformer[C, P, A, K])(
      ipd: InternalProcessedDeploy,
      events: Seq[ReportingEvent]
  ): DeployTrace =
    DeployTrace(
      ipd.deploy.sig.base16String,
      ipd.deploy.term,
      events.map(transformer.transformEvent).toList
    )

  def createResponse[C, P, A, K](
      hash: BlockHash,
      state: Option[
        (Either[ReplayFailure, List[(InternalProcessedDeploy, Seq[ReportingEvent])]], BlockMessage)
      ],
      transformer: ReportingEventTransformer[C, P, A, K]
  ): Report =
    state match {
      case None => BlockNotFound(hash.base16String)
      case Some((Left(internalError @ InternalError(deployData, _)), _)) =>
        BlockReplayFailed(
          hash.base16String,
          internalError.toString,
          deployData.sig.base16String.some
        )
      case Some((Left(failed), _)) =>
        BlockReplayFailed(hash.base16String, failed.toString, None)
      case Some((Right(deploys), bm)) =>
        val t = transformDeploy(transformer)(_, _)
        BlockTracesReport(
          hash.base16String,
          bm.seqNum,
          bm.sender.base16String,
          deploys.map(Function.tupled(t)),
          bm.header.parentsHashList.map(_.base16String)
        )
      case _ => BlockReplayError(hash.base16String)
    }

  def setupTransformer(prettyPrinter: RhoPrinter) =
    new ReportingEventTransformer[Par, BindPattern, ListParWithRandom, TaggedContinuation](
      serializeC = channel => prettyPrinter.buildString(channel),
      serializeP = p => p.patterns.map(prettyPrinter.buildString).mkString("[", ";", "]"),
      serializeA = data => data.pars.map(prettyPrinter.buildString).mkString("[", ";", "]"),
      serializeK = k =>
        k.taggedCont match {
          case ParBody(value)      => prettyPrinter.buildString(value.body)
          case ScalaBodyRef(value) => s"ScalaBodyRef($value)"
          case Empty               => "empty"
        }
    )
}

object ReportingCasper {
  def noop[F[_]: Sync]: ReportingCasper[F] = new ReportingCasper[F] {
    override def trace(hash: BlockHash): F[Report] =
      Sync[F].delay(BlockNotFound(hash.toStringUtf8))
  }

  type RhoReportingRspace[F[_]] =
    ReportingRspace[F, Par, BindPattern, ListParWithRandom, TaggedContinuation]

  def rhoReporter[F[_]: ContextShift: Monad: Concurrent: Log: Metrics: Span: CatsPar: BlockStore: BlockDagStorage](
      historyRepository: RhoHistoryRepository[F]
  )(implicit scheduler: ExecutionContext) =
    new ReportingCasper[F] {
      val codecK                                                     = serializeTaggedContinuation.toCodec
      implicit val m: RSpaceMatch[F, BindPattern, ListParWithRandom] = matchListPar[F]
      val prettyPrinter                                              = RhoPrinter()
      val transformer                                                = setupTransformer(prettyPrinter)

      override def trace(hash: BlockHash): F[Report] =
        for {
          replayStore <- HotStore.empty(historyRepository)(codecK, Concurrent[F])
          reporting = new ReportingRspace[
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
          runtime <- ReportingRuntime.createWithEmptyCost[F](reporting, Seq.empty)
          manager <- fromRuntime(runtime)
          dag     <- BlockDagStorage[F].getRepresentation
          bmO     <- BlockStore[F].get(hash)
          result <- bmO.traverse(
                     bm =>
                       replayBlock(bm, dag, manager)
                         .map(s => (s, bm))
                   )
          response = createResponse(hash, result, transformer)
        } yield response
    }

  private def replayBlock[F[_]: Sync: BlockStore](
      block: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: ReportingRuntimeManagerImpl[F]
  ): F[Either[ReplayFailure, List[(InternalProcessedDeploy, Seq[ReportingEvent])]]] = {
    val hash        = ProtoUtil.preStateHash(block)
    val deploys     = block.body.deploys.map(InternalProcessedDeploy.fromProcessedDeploy)
    val timestamp   = block.header.timestamp
    val blockNumber = block.body.state.blockNumber
    runtimeManager.replayComputeState(hash)(deploys, BlockData(timestamp, blockNumber))
  }

  def fromRuntime[F[_]: Concurrent: Sync: Metrics: Span: Log](
      runtime: ReportingRuntime[F]
  ): F[ReportingRuntimeManagerImpl[F]] =
    for {
      _                <- runtime.reportingSpace.clear()
      _                <- Runtime.bootstrapRegistry(runtime, runtime.replayReducer :: Nil)
      replayCheckpoint <- runtime.reportingSpace.createCheckpoint()
      replayHash       = ByteString.copyFrom(replayCheckpoint.root.bytes.toArray)
      runtime          <- MVar[F].of(runtime)
    } yield new ReportingRuntimeManagerImpl(replayHash, runtime)

  class ReportingRuntimeManagerImpl[F[_]: Concurrent: Metrics: Span: Log](
      val emptyStateHash: StateHash,
      runtimeContainer: MVar[F, ReportingRuntime[F]]
  ) {

    def replayComputeState(startHash: StateHash)(
        terms: Seq[InternalProcessedDeploy],
        blockData: BlockData
    ): F[Either[ReplayFailure, List[(InternalProcessedDeploy, Seq[ReportingEvent])]]] =
      Sync[F].bracket {
        runtimeContainer.take
      } { runtime =>
        replayDeploys(runtime, startHash, terms, replayDeploy(runtime))
      }(runtimeContainer.put)

    private def replayDeploys(
        runtime: ReportingRuntime[F],
        startHash: StateHash,
        terms: Seq[InternalProcessedDeploy],
        replayDeploy: InternalProcessedDeploy => F[Either[ReplayFailure, Seq[ReportingEvent]]]
    ): F[Either[ReplayFailure, List[(InternalProcessedDeploy, Seq[ReportingEvent])]]] =
      (for {
        _ <- EitherT.right(runtime.reportingSpace.reset(Blake2b256Hash.fromByteString(startHash)))
        res <- EitherT.right(terms.toList.traverse { term =>
                for {
                  rd <- replayDeploy(term)
                  r = rd match {
                    case Left(_)  => Seq.empty[ReportingEvent]
                    case Right(s) => s
                  }
                } yield (term, r)
              })
        _ <- EitherT.right[ReplayFailure](runtime.reportingSpace.createCheckpoint())
      } yield res).value

    private def replayDeploy(runtime: ReportingRuntime[F])(
        processedDeploy: InternalProcessedDeploy
    ): F[Either[ReplayFailure, Seq[ReportingEvent]]] = {
      import processedDeploy._
      for {
        _              <- runtime.reportingSpace.rig(processedDeploy.deployLog)
        softCheckpoint <- runtime.reportingSpace.createSoftCheckpoint()
        replayEvaluateResultEither <- computeEffect(runtime, runtime.replayReducer)(
                                       processedDeploy.deploy
                                     ).attempt
        failureEither <- replayEvaluateResultEither match {
                          case Right(EvaluateResult(replayCost, replayUserErrors)) =>
                            if (isFailed != replayUserErrors.nonEmpty)
                              runtime.reportingSpace
                                .revertToSoftCheckpoint(softCheckpoint)
                                .as(
                                  ReplayFailure
                                    .replayStatusMismatch(isFailed, replayUserErrors.nonEmpty)
                                    .asLeft[Seq[ReportingEvent]]
                                )
                            else if (replayUserErrors.nonEmpty)
                              runtime.reportingSpace
                                .revertToSoftCheckpoint(softCheckpoint) >> runtime.reportingSpace.getReport
                                .map(_.asRight[ReplayFailure])
                            else if (cost.cost != replayCost.value)
                              runtime.reportingSpace
                                .revertToSoftCheckpoint(softCheckpoint)
                                .as(
                                  ReplayFailure
                                    .replayCostMismatch(deploy, cost.cost, replayCost.value)
                                    .asLeft[Seq[ReportingEvent]]
                                )
                            else
                              runtime.reportingSpace.checkReplayData().attempt >>= {
                                case Right(_) =>
                                  runtime.reportingSpace.getReport.map(_.asRight[ReplayFailure])
                                case Left(throwable) =>
                                  runtime.reportingSpace.revertToSoftCheckpoint(softCheckpoint) >> {
                                    throwable match {
                                      case replayException: ReplayException =>
                                        ReplayFailure
                                          .unusedCOMMEvent(deploy, replayException)
                                          .asLeft[Seq[ReportingEvent]]
                                          .pure[F]
                                      case _ =>
                                        ReplayFailure
                                          .internalError(deploy, throwable)
                                          .asLeft[Seq[ReportingEvent]]
                                          .pure[F]
                                    }
                                  }
                              }
                          case Left(throwable) =>
                            runtime.reportingSpace
                              .revertToSoftCheckpoint(softCheckpoint)
                              .as(
                                ReplayFailure
                                  .internalError(deploy, throwable)
                                  .asLeft[Seq[ReportingEvent]]
                              )
                        }
      } yield failureEither
    }

    private def computeEffect(runtime: ReportingRuntime[F], reducer: Reduce[F])(
        deploy: DeployData
    ): F[EvaluateResult] =
      RuntimeManager.evaluate(reducer, runtime.cost, runtime.errorLog)(deploy)
  }
}

class ReportingRuntime[F[_]: Sync](
    val replayReducer: Reduce[F],
    val reportingSpace: RhoReportingRspace[F],
    val errorLog: ErrorLog[F],
    val cost: _cost[F],
    val blockData: Ref[F, BlockData],
    val invalidBlocks: Runtime.InvalidBlocks[F]
) extends HasCost[F] {
  def readAndClearErrorVector(): F[Vector[InterpreterError]] = errorLog.readAndClearErrorVector()
  def close(): F[Unit] =
    for {
      _ <- reportingSpace.close()
    } yield ()
}

object ReportingRuntime {
  implicit val RuntimeMetricsSource: Source = Metrics.Source(RholangMetricsSource, "runtime")

  def createWithEmptyCost[F[_]: Concurrent: Log: Metrics: Span: par.Par](
      reporting: RhoReportingRspace[F],
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  ): F[ReportingRuntime[F]] = {
    implicit val P = par.Par[F].parallel
    for {
      cost <- CostAccounting.emptyCost[F]
      runtime <- {
        implicit val c = cost
        create(reporting, extraSystemProcesses)
      }
    } yield runtime
  }

  def create[F[_]: Concurrent: Log: Metrics: Span, M[_]](
      reporting: RhoReportingRspace[F],
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(
      implicit P: Parallel[F, M],
      cost: _cost[F]
  ): F[ReportingRuntime[F]] = {
    val errorLog                                      = new ErrorLog[F]()
    implicit val ft: FunctorTell[F, InterpreterError] = errorLog
    for {
      mapsAndRefs                                     <- setupMapsAndRefs(extraSystemProcesses)
      (blockDataRef, invalidBlocks, urnMap, procDefs) = mapsAndRefs
      reducer = setupReducer(
        ChargingRSpace.chargingRSpace[F](reporting),
        blockDataRef,
        invalidBlocks,
        extraSystemProcesses,
        urnMap
      )
      res <- Runtime.introduceSystemProcesses(reporting :: Nil, procDefs)
    } yield {
      assert(res.forall(_.isEmpty))
      new ReportingRuntime[F](reducer, reporting, errorLog, cost, blockDataRef, invalidBlocks)
    }
  }
}
