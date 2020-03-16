package coop.rchain.casper

import cats.data.EitherT
import cats.effect.concurrent.{MVar, Ref}
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.{Monad, Parallel}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.casper.ReportingCasper.RhoReportingRspace
import coop.rchain.casper.ReportingCasperData._
import coop.rchain.casper.protocol.{BlockMessage, ProcessedDeploy, ProcessedSystemDeploy}
import coop.rchain.casper.util.{EventConverter, ProtoUtil}
import coop.rchain.casper.util.rholang.RuntimeManager.{evaluate, StateHash}
import coop.rchain.casper.util.rholang.SystemDeployPlatformFailure.{
  ConsumeFailed,
  UnexpectedResult,
  UnexpectedSystemErrors
}
import coop.rchain.casper.util.rholang.SystemDeployUserError.SystemDeployError
import coop.rchain.casper.util.rholang.costacc.{PreChargeDeploy, RefundDeploy}
import coop.rchain.casper.util.rholang.{
  InternalError,
  ReplayFailure,
  SystemDeploy,
  SystemDeployPlatformFailure,
  SystemDeployUserError,
  SystemDeployUtil,
  UnusedCOMMEvent
}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash._
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models.{
  BindPattern,
  EVar,
  Expr,
  ListParWithRandom,
  Par,
  ParMap,
  SortedParMap,
  TaggedContinuation,
  Var
}
import coop.rchain.rholang.RholangMetricsSource
import coop.rchain.rholang.interpreter.Runtime.{
  setupMapsAndRefs,
  setupReducer,
  BlockData,
  RhoHistoryRepository,
  SystemProcess
}
import coop.rchain.rholang.interpreter.accounting.{_cost, Cost, CostAccounting}
import coop.rchain.rholang.interpreter.storage._
import coop.rchain.rholang.interpreter.{
  EvaluateResult,
  HasCost,
  Interpreter,
  Reduce,
  RhoType,
  Runtime,
  PrettyPrinter => RhoPrinter
}
import coop.rchain.rspace.ReportingRspace.{
  ReportingComm,
  ReportingConsume,
  ReportingEvent,
  ReportingProduce
}
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.{
  Blake2b256Hash,
  HotStore,
  ReplayException,
  ReportingRspace,
  Match => RSpaceMatch
}
import coop.rchain.shared.Log
import monix.execution.atomic.AtomicAny
import coop.rchain.models.Expr.ExprInstance.EVarBody
import coop.rchain.models.Validator.Validator
import coop.rchain.models.Var.VarInstance.FreeVar

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
      ipd: ProcessedDeploy,
      events: Seq[ReportingEvent]
  ): DeployTrace =
    DeployTrace(
      ipd.deploy.sig.base16String,
      ipd.deploy.data.term,
      events.map(transformer.transformEvent).toList
    )

  def createResponse[C, P, A, K](
      hash: BlockHash,
      state: Option[
        (Either[ReplayFailure, List[(ProcessedDeploy, Seq[ReportingEvent])]], BlockMessage)
      ],
      transformer: ReportingEventTransformer[C, P, A, K]
  ): Report =
    state match {
      case None => BlockNotFound(hash.base16String)
      case Some((Left(internalError @ InternalError(_)), _)) =>
        BlockReplayFailed(
          hash.base16String,
          internalError.toString,
          None
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

  def rhoReporter[F[_]: ContextShift: Monad: Concurrent: Log: Metrics: Span: Parallel: BlockStore: BlockDagStorage](
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
          runtime          <- ReportingRuntime.createWithEmptyCost[F](reporting, Seq.empty)
          manager          <- fromRuntime(runtime)
          dag              <- BlockDagStorage[F].getRepresentation
          bmO              <- BlockStore[F].get(hash)
          invalidBlocksSet <- dag.invalidBlocks
          invalidBlocks    = invalidBlocksSet.map(block => (block.blockHash, block.sender)).toMap
          result <- bmO.traverse(
                     bm =>
                       replayBlock(bm, dag, manager, invalidBlocks)
                         .map(s => (s, bm))
                   )
          response = createResponse(hash, result, transformer)
        } yield response

    }

  private def replayBlock[F[_]: Sync: BlockStore](
      block: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: ReportingRuntimeManagerImpl[F],
      invalidBlocks: Map[BlockHash, Validator]
  ): F[Either[ReplayFailure, List[(ProcessedDeploy, Seq[ReportingEvent])]]] = {
    val hash          = ProtoUtil.preStateHash(block)
    val deploys       = block.body.deploys
    val systemDeploys = block.body.systemDeploys
    runtimeManager.replayComputeState(hash)(
      deploys,
      systemDeploys,
      BlockData.fromBlock(block),
      invalidBlocks
    )
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
    import coop.rchain.models.rholang.{implicits => toPar}
    private val systemDeployConsumeAllPattern =
      BindPattern(List(toPar(Expr(EVarBody(EVar(Var(FreeVar(0))))))), freeCount = 1)
    private val emptyContinuation = TaggedContinuation()
    private def setInvalidBlocks(
        invalidBlocks: Map[BlockHash, Validator],
        runtime: ReportingRuntime[F]
    ) = {
      val invalidBlocksPar: Par =
        Par(
          exprs = Seq(
            Expr(
              Expr.ExprInstance.EMapBody(
                ParMap(SortedParMap(invalidBlocks.map {
                  case (validator, blockHash) =>
                    (
                      RhoType.ByteArray(validator.toByteArray),
                      RhoType.ByteArray(blockHash.toByteArray)
                    )
                }))
              )
            )
          )
        )
      runtime.invalidBlocks.setParams(invalidBlocksPar)
    }

    def replayComputeState(startHash: StateHash)(
        terms: Seq[ProcessedDeploy],
        sysDeploys: Seq[ProcessedSystemDeploy],
        blockData: BlockData,
        invalidBlocks: Map[BlockHash, Validator]
    ): F[Either[ReplayFailure, List[(ProcessedDeploy, Seq[ReportingEvent])]]] =
      Sync[F].bracket {
        runtimeContainer.take
      } { runtime =>
        for {
          _ <- runtime.blockData.set(blockData)
          _ <- setInvalidBlocks(invalidBlocks, runtime)
          result <- replayDeploys(
                     runtime,
                     startHash,
                     terms,
                     sysDeploys,
                     replayDeploy(runtime, true)
//                     replaySystemDeploy(runtime)
                   )
        } yield result
      }(runtimeContainer.put)

    private def replayDeploys(
        runtime: ReportingRuntime[F],
        startHash: StateHash,
        terms: Seq[ProcessedDeploy],
        systemDeploys: Seq[ProcessedSystemDeploy],
        replayDeploy: ProcessedDeploy => F[Either[ReplayFailure, Seq[ReportingEvent]]]
//        replaySystemDeploy: ProcessedSystemDeploy => F[Either[ReplayFailure, Seq[ReportingEvent]]]
    ): F[Either[ReplayFailure, List[(ProcessedDeploy, Seq[ReportingEvent])]]] =
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

    private def replayDeploy(runtime: ReportingRuntime[F], withCostAccounting: Boolean)(
        processedDeploy: ProcessedDeploy
    ): F[Either[ReplayFailure, Seq[ReportingEvent]]] = {
      import processedDeploy._
      val deployEvaluator = EitherT
        .liftF {
          for {
            fallback <- runtime.reportingSpace.createSoftCheckpoint()
            result   <- evaluate(runtime.replayReducer, runtime.cost)(deploy)
            /* Since the state of `replaySpace` is reset on each invocation of `replayComputeState`,
              and `ReplayFailure`s mean that block processing is cancelled upstream, we only need to
              reset state if the replay effects of valid deploys need to be discarded. */
            _ <- runtime.reportingSpace.revertToSoftCheckpoint(fallback).whenA(result.failed)
          } yield result
        }
        .ensureOr(
          /* Regardless of success or failure, verify that deploy status' match. */
          result => ReplayFailure.replayStatusMismatch(isFailed, result.failed)
        )(result => isFailed == result.failed)
        .ensureOr(
          result =>
            /* Verify evaluation costs match. */
            ReplayFailure.replayCostMismatch(cost.cost, result.cost.value)
        )(result => cost.cost == result.cost.value)

      def evaluatorT: EitherT[F, ReplayFailure, Boolean] =
        if (withCostAccounting) {
          val expectedFailure = processedDeploy.systemDeployError.map(SystemDeployError)
          replaySystemDeployInternal(runtime)(
            new PreChargeDeploy(
              deploy.data.totalPhloCharge,
              deploy.pk,
              SystemDeployUtil.generatePreChargeDeployRandomSeed(processedDeploy.deploy)
            ),
            expectedFailure
          ).flatMap(
            _ =>
              if (expectedFailure.isEmpty)
                deployEvaluator
                  .semiflatMap(
                    evalResult =>
                      runtime.reportingSpace
                        .createSoftCheckpoint()
                        .whenA(evalResult.succeeded)
                        .map(_ => evalResult.succeeded)
                  )
                  .flatTap(
                    succeeded =>
                      replaySystemDeployInternal(runtime)(
                        new RefundDeploy(
                          refundAmount,
                          SystemDeployUtil.generateRefundDeployRandomSeed(processedDeploy.deploy)
                        ),
                        None
                      ).map(_ => succeeded)
                  )
              else EitherT.rightT(true)
          )
        } else deployEvaluator.map(_.succeeded)

      Span[F].withMarks("replay-deploy") {
        for {
          _ <- runtime.reportingSpace.rig(
                processedDeploy.deployLog.map(EventConverter.toRspaceEvent)
              )
          _ <- Span[F].mark("before-replay-deploy-compute-effect")
          failureOption <- evaluatorT.flatMap { succeeded =>
                            /* This deployment represents either correct program `Some(result)`,
                or we have a failed pre-charge (`None`) but we agree on that it failed.
                In both cases we want to check reply data and see if everything is in order */
                            runtime.reportingSpace
                              .checkReplayData()
                              .attemptT
                              .leftMap {
                                case replayException: ReplayException =>
                                  ReplayFailure.unusedCOMMEvent(replayException)
                                case throwable => ReplayFailure.internalError(throwable)
                              }
                              .leftFlatMap {
                                case UnusedCOMMEvent(_) if !succeeded =>
                                  // TODO: temp fix for replay error mismatch
                                  // https://rchain.atlassian.net/browse/RCHAIN-3505
                                  EitherT.rightT[F, ReplayFailure](())
                                case ex: ReplayFailure => EitherT.leftT[F, Unit](ex)
                              }
                              .semiflatMap(
                                _ => runtime.reportingSpace.getReport
                              )
                          }.value

        } yield failureOption
      }
    }
    private def consumeResult[S <: SystemDeploy](
        runtime: ReportingRuntime[F]
    )(systemDeploy: S, replay: Boolean): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]] = {
      import coop.rchain.rspace.util._
      runtime.reportingSpace
        .consume(
          Seq(systemDeploy.returnChannel),
          Seq(systemDeployConsumeAllPattern),
          emptyContinuation,
          persist = false
        )
        .map(unpackOption)
    }
    private def replaySystemDeployInternal[S <: SystemDeploy](
        runtime: ReportingRuntime[F]
    )(
        systemDeploy: S,
        expectedFailure: Option[SystemDeployUserError]
    ): EitherT[F, ReplayFailure, Either[SystemDeployUserError, systemDeploy.Result]] =
      EitherT
        .liftF(evaluateSystemSource(runtime)(systemDeploy, replay = true))
        .ensureOr(evaluateResult => UnexpectedSystemErrors(evaluateResult.errors))(_.succeeded)
        .semiflatMap(_ => consumeResult(runtime)(systemDeploy, replay = true))
        .subflatMap {
          case Some((_, Seq(ListParWithRandom(Seq(par), _)))) =>
            systemDeploy.extractResult(par)
          case Some((_, unexpectedResults)) =>
            UnexpectedResult(unexpectedResults.flatMap(_.pars)).asLeft
          case None => ConsumeFailed.asLeft
        }
        .leftSemiflatMap {
          case platformFailure: SystemDeployPlatformFailure =>
            platformFailure.raiseError[F, SystemDeployUserError]
          case failure: SystemDeployUserError => failure.pure[F]
        }
        .transform {
          case Left(error) =>
            expectedFailure.fold(
              ReplayFailure
                .replayStatusMismatch(initialFailed = false, replayFailed = true)
                .asLeft[Either[SystemDeployUserError, systemDeploy.Result]]
            )(
              expected =>
                Either.cond(
                  error == expected,
                  Left(error),
                  ReplayFailure.systemDeployErrorMismatch(expected.errorMessage, error.errorMessage)
                )
            )
          case Right(result) =>
            Either.cond(
              expectedFailure.isEmpty,
              Right(result),
              ReplayFailure.replayStatusMismatch(initialFailed = true, replayFailed = false)
            )
        }
        .flatTap(_ => EitherT.right(runtime.reportingSpace.createSoftCheckpoint())) //We need a clear demarcation of system deploys

    private def evaluateSystemSource[S <: SystemDeploy](
        runtime: ReportingRuntime[F]
    )(systemDeploy: S, replay: Boolean): F[EvaluateResult] = {
      implicit val c: _cost[F]         = runtime.cost
      implicit val r: Blake2b512Random = systemDeploy.rand
      Interpreter[F].injAttempt(
        runtime.replayReducer,
        systemDeploy.source,
        Cost.UNSAFE_MAX,
        systemDeploy.env
      )
    }

  }
}

/**
  * The ReportingRuntime is similar with the Runtime(coop/rchain/rholang/interpreter/Runtime.scala) but doesn't
  * include val reducer and val space because the ReportingRuntime only care about the replay result.
  */
class ReportingRuntime[F[_]: Sync](
    val replayReducer: Reduce[F],
    val reportingSpace: RhoReportingRspace[F],
    val cost: _cost[F],
    val blockData: Ref[F, BlockData],
    val invalidBlocks: Runtime.InvalidBlocks[F]
) extends HasCost[F] {
  def close(): F[Unit] =
    for {
      _ <- reportingSpace.close()
    } yield ()
}

object ReportingRuntime {
  implicit val RuntimeMetricsSource: Source =
    Metrics.Source(RholangMetricsSource, "reportingRuntime")

  def createWithEmptyCost[F[_]: Concurrent: Log: Metrics: Span: Parallel](
      reporting: RhoReportingRspace[F],
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  ): F[ReportingRuntime[F]] = {
    implicit val P = Parallel[F]
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
      implicit P: Parallel[F],
      cost: _cost[F]
  ): F[ReportingRuntime[F]] =
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
      new ReportingRuntime[F](reducer, reporting, cost, blockDataRef, invalidBlocks)
    }
}
