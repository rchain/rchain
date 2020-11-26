package coop.rchain.casper

import cats.data.EitherT
import cats.effect.concurrent.{MVar, MVar2, Ref}
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.{Monad, Parallel}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.casper.ReportingCasper.RhoReportingRspace
import coop.rchain.casper.protocol.{BlockMessage, ProcessedDeploy}
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
  Runtime
}
import coop.rchain.rspace.ReportingRspace.ReportingEvent
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
import coop.rchain.metrics.MetricsSemaphore

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
      historyRepository: RhoHistoryRepository[F],
      store: ReportMemStore[F]
  )(implicit scheduler: ExecutionContext): ReportingCasper[F] =
    new ReportingCasper[F] {
      val codecK                                                     = serializeTaggedContinuation.toSizeHeadCodec
      implicit val m: RSpaceMatch[F, BindPattern, ListParWithRandom] = matchListPar[F]
      implicit val source                                            = Metrics.Source(CasperMetricsSource, "report-replay")

      val blockLockMap = TrieMap[BlockHash, (MetricsSemaphore[F], Boolean)]()

      private def replayGetReport(
          block: BlockMessage
      ): F[Either[ReportError, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]] =
        for {
          replayStore <- HotStore.empty(historyRepository)(
                          codecK,
                          Concurrent[F]
                        )
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
          runtime <- ReportingRuntime
                      .createWithEmptyCost[F](reporting, Seq.empty)
          manager          <- fromRuntime(runtime)
          dag              <- BlockDagStorage[F].getRepresentation
          genesis          <- BlockStore[F].getApprovedBlock
          isGenesis        = genesis.exists(a => block.blockHash == a.candidate.block.blockHash)
          invalidBlocksSet <- dag.invalidBlocks
          invalidBlocks = invalidBlocksSet
            .map(block => (block.blockHash, block.sender))
            .toMap
          res <- replayBlock(block, dag, manager, invalidBlocks, isGenesis)
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

    }

  private def replayBlock[F[_]: Sync: BlockStore](
      block: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: ReportingRuntimeManagerImpl[F],
      invalidBlocks: Map[BlockHash, Validator],
      isGenesis: Boolean
  ): F[Either[ReplayFailure, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]] = {
    val hash    = ProtoUtil.preStateHash(block)
    val deploys = block.body.deploys
    runtimeManager.replayComputeState(hash)(
      deploys,
      BlockData.fromBlock(block),
      invalidBlocks,
      isGenesis
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

  /**
    * The most of the codes of this ReportingRuntimeManagerImpl are copied from coop.rchain.casper.util.rholang.RuntimeManagerImpl
    * to keep the replay logic the same(the Precharge, userDeploy, refund). Based on the ticket of
    * https://rchain.atlassian.net/browse/RCHAIN-4025, the coop.rchain.casper.util.rholang.RuntimeManagerImpl should
    * be refactored in the future and the runtimeManager here should be refactored along with it and make
    * the reportingCasper work with the the refactored runtimeManager. The duplicate codes should be refactored anyway.
    * The codes here are very ugly now.
    */
  class ReportingRuntimeManagerImpl[F[_]: Concurrent: Metrics: Span: Log](
      val emptyStateHash: StateHash,
      runtimeContainer: MVar2[F, ReportingRuntime[F]]
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
        blockData: BlockData,
        invalidBlocks: Map[BlockHash, Validator],
        isGenesis: Boolean
    ): F[Either[ReplayFailure, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]] =
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
                     replayDeploy(runtime, !isGenesis)
                   )
        } yield result
      }(runtimeContainer.put)

    private def replayDeploys(
        runtime: ReportingRuntime[F],
        startHash: StateHash,
        terms: Seq[ProcessedDeploy],
        replayDeploy: ProcessedDeploy => F[Either[ReplayFailure, Seq[Seq[ReportingEvent]]]]
    ): F[Either[ReplayFailure, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]] =
      (for {
        _ <- EitherT.right(runtime.reportingSpace.reset(Blake2b256Hash.fromByteString(startHash)))
        res <- EitherT.right(terms.toList.traverse { term =>
                for {
                  rd <- replayDeploy(term)
                  r = rd match {
                    case Left(_)  => Seq.empty[Seq[ReportingEvent]]
                    case Right(s) => s
                  }
                } yield (term, r)
              })
        _ <- EitherT.right[ReplayFailure](runtime.reportingSpace.createCheckpoint())
      } yield res).value

    private def replayDeploy(runtime: ReportingRuntime[F], withCostAccounting: Boolean)(
        processedDeploy: ProcessedDeploy
    ): F[Either[ReplayFailure, Seq[Seq[ReportingEvent]]]] = {
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
