package coop.rchain.casper.util.rholang

import cats.Applicative
import cats.data.{EitherT, WriterT}
import cats.effect.concurrent.MVar
import cats.effect.{Sync, _}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.CasperMetricsSource
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager.{evaluate, StateHash}
import coop.rchain.casper.util.rholang.SystemDeployPlatformFailure._
import coop.rchain.casper.util.rholang.SystemDeployUserError._
import coop.rchain.casper.util.rholang.costacc.{PreChargeDeploy, RefundDeploy}
import coop.rchain.casper.util.{ConstructDeploy, EventConverter}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.casper.util.{ConstructDeploy, EventConverter, ProtoUtil}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.{EVarBody, GString}
import coop.rchain.models.Validator.Validator
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.BugFoundError
import coop.rchain.rholang.interpreter.{
  ErrorLog,
  EvaluateResult,
  Interpreter,
  Reduce,
  RhoType,
  Runtime
}
import coop.rchain.rspace.{Blake2b256Hash, ReplayException}
import coop.rchain.shared.Log

trait RuntimeManager[F[_]] {
  def playSystemDeploy[S <: SystemDeploy](startHash: StateHash)(
      systemDeploy: S
  ): F[SystemDeployPlayResult[systemDeploy.Result]]
  def replaySystemDeploy[S <: SystemDeploy](startHash: StateHash)(
      systemDeploy: S,
      processedSystemDeploy: ProcessedSystemDeploy
  ): F[Either[ReplayFailure, SystemDeployReplayResult[systemDeploy.Result]]]
  def captureResults(
      startHash: StateHash,
      deploy: Signed[DeployData],
      name: String = "__SCALA__"
  ): F[Seq[Par]]
  def replayComputeState(startHash: StateHash)(
      terms: Seq[ProcessedDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator],
      isGenesis: Boolean
  ): F[Either[ReplayFailure, StateHash]]
  def computeState(hash: StateHash)(
      terms: Seq[Signed[DeployData]],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator]
  ): F[(StateHash, Seq[ProcessedDeploy])]
  def computeGenesis(
      terms: Seq[Signed[DeployData]],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy])]
  def computeBonds(startHash: StateHash): F[Seq[Bond]]
  def getData(hash: StateHash)(channel: Par): F[Seq[Par]]
  def getContinuation(hash: StateHash)(
      channels: Seq[Par]
  ): F[Seq[(Seq[BindPattern], Par)]]
  def emptyStateHash: StateHash
  def withRuntimeLock[A](f: Runtime[F] => F[A]): F[A]
}

class RuntimeManagerImpl[F[_]: Concurrent: Metrics: Span: Log](
    val emptyStateHash: StateHash,
    runtimeContainer: MVar[F, Runtime[F]]
) extends RuntimeManager[F] {
  import coop.rchain.models.rholang.{implicits => toPar}

  private[this] val RuntimeManagerMetricsSource =
    Metrics.Source(CasperMetricsSource, "runtime-manager")
  private[this] val replayComputeStateLabel =
    Metrics.Source(RuntimeManagerMetricsSource, "replay-compute-state")
  private[this] val computeStateLabel = Metrics.Source(RuntimeManagerMetricsSource, "compute-state")
  private[this] val computeGenesisLabel =
    Metrics.Source(RuntimeManagerMetricsSource, "compute-genesis")

  private val systemDeployConsumeAllPattern =
    BindPattern(List(toPar(Expr(EVarBody(EVar(Var(FreeVar(0))))))), freeCount = 1)
  private val emptyContinuation = TaggedContinuation()

  private def evaluateSystemSource[S <: SystemDeploy](
      runtime: Runtime[F]
  )(systemDeploy: S, replay: Boolean): F[EvaluateResult] = {
    implicit val c: _cost[F]         = runtime.cost
    implicit val r: Blake2b512Random = systemDeploy.rand
    Interpreter[F].injAttempt(
      if (replay) runtime.replayReducer else runtime.reducer,
      runtime.errorLog,
      systemDeploy.source,
      Cost.UNSAFE_MAX,
      systemDeploy.env
    )
  }

  private def consumeResult[S <: SystemDeploy](
      runtime: Runtime[F]
  )(systemDeploy: S, replay: Boolean): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]] = {
    import coop.rchain.rspace.util._
    (if (replay) runtime.replaySpace else runtime.space)
      .consume(
        Seq(systemDeploy.returnChannel),
        Seq(systemDeployConsumeAllPattern),
        emptyContinuation,
        persist = false
      )
      .map(unpackOption)
  }

  def playSystemDeploy[S <: SystemDeploy](stateHash: StateHash)(
      systemDeploy: S
  ): F[SystemDeployPlayResult[systemDeploy.Result]] =
    withResetRuntimeLock(stateHash) { runtime =>
      playSystemDeployInternal(runtime)(systemDeploy) >>= {
        case (eventLog, Right(result)) =>
          runtime.space.createCheckpoint().map(_.root.toByteString) >>= { finalStateHash =>
            SystemDeployPlayResult.playSucceeded(finalStateHash, eventLog, result).pure
          }
        case (eventLog, Left(systemDeployError)) =>
          SystemDeployPlayResult.playFailed(eventLog, systemDeployError).pure
      }
    }
  private def playSystemDeployInternal[S <: SystemDeploy](runtime: Runtime[F])(
      systemDeploy: S
  ): F[(Vector[Event], Either[SystemDeployError, systemDeploy.Result])] =
    for {
      evaluateResult <- evaluateSystemSource(runtime)(systemDeploy, replay = false)
      maybeConsumedTuple <- if (evaluateResult.failed)
                             UnexpectedSystemErrors(evaluateResult.errors).raiseError
                           else consumeResult(runtime)(systemDeploy, replay = false)
      resultOrSystemDeployError <- maybeConsumedTuple match {
                                    case Some((_, Seq(ListParWithRandom(Seq(par), _)))) =>
                                      systemDeploy.extractResult(par) match {
                                        case Left(
                                            systemDeployPlatformFailure: SystemDeployPlatformFailure
                                            ) =>
                                          systemDeployPlatformFailure.raiseError
                                        // sorry about that but scala seems to be confused with everything else and promotes Left to SystemDeployFailure nonetheless
                                        case otherwise =>
                                          otherwise
                                            .asInstanceOf[
                                              Either[SystemDeployError, systemDeploy.Result]
                                            ]
                                            .pure
                                      }
                                    case Some((_, unexpectedResults)) =>
                                      UnexpectedResult(unexpectedResults.flatMap(_.pars)).raiseError
                                    case None => ConsumeFailed.raiseError
                                  }
      postDeploySoftCheckpoint <- runtime.space.createSoftCheckpoint()
      log                      = postDeploySoftCheckpoint.log
    } yield (log.map(EventConverter.toCasperEvent).toVector, resultOrSystemDeployError)

  def replaySystemDeploy[S <: SystemDeploy](stateHash: StateHash)(
      systemDeploy: S,
      processedSystemDeploy: ProcessedSystemDeploy
  ): F[Either[ReplayFailure, SystemDeployReplayResult[systemDeploy.Result]]] = withRuntimeLock {
    runtime =>
      for {
        _ <- runtime.replaySpace.rigAndReset(
              Blake2b256Hash.fromByteString(stateHash),
              processedSystemDeploy.eventList.map(EventConverter.toRspaceEvent)
            )
        expectedFailure = processedSystemDeploy
          .fold(_ => None, (_, errorMsg) => Some(SystemDeployError(errorMsg)))
        replayed <- replaySystemDeployInternal(runtime)(systemDeploy, expectedFailure)
                     .flatMap(
                       result =>
                         runtime.replaySpace
                           .checkReplayData()
                           .attemptT
                           .leftMap {
                             case replayException: ReplayException =>
                               ReplayFailure.unusedCOMMEvent(replayException)
                             case throwable => ReplayFailure.internalError(throwable)
                           }
                           .semiflatMap(
                             _ =>
                               result match {
                                 case Right(value) =>
                                   runtime.replaySpace
                                     .createCheckpoint()
                                     .map(
                                       checkpoint =>
                                         SystemDeployReplayResult
                                           .replaySucceeded(checkpoint.root.toByteString, value)
                                     )
                                 case Left(failure) =>
                                   SystemDeployReplayResult
                                     .replayFailed[systemDeploy.Result](failure)
                                     .pure
                               }
                           )
                     )
                     .value
      } yield replayed
  }

  private def replaySystemDeployInternal[S <: SystemDeploy](
      runtime: Runtime[F]
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
        case failure: SystemDeployUserError => failure.pure
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
      .flatTap(_ => EitherT.right(runtime.replaySpace.createSoftCheckpoint())) //We need a clear demarcation of system deploys

  def computeState(startHash: StateHash)(
      terms: Seq[Signed[DeployData]],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator]
  ): F[(StateHash, Seq[ProcessedDeploy])] =
    withRuntimeLock { runtime =>
      Span[F].trace(computeStateLabel) {
        for {
          _ <- runtime.blockData.set(blockData)
          _ <- setInvalidBlocks(invalidBlocks, runtime)
          _ <- Span[F].mark("before-process-deploys")
          result <- processDeploys(
                     runtime,
                     startHash,
                     terms,
                     processDeployWithCostAccounting(runtime)
                   )
        } yield result
      }
    }

  def computeGenesis(
      terms: Seq[Signed[DeployData]],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy])] = {
    val startHash = emptyStateHash
    withRuntimeLock { runtime =>
      Span[F].trace(computeGenesisLabel) {
        for {
          _          <- runtime.blockData.set(BlockData(blockTime, 0, PublicKey(Array[Byte]())))
          _          <- Span[F].mark("before-process-deploys")
          evalResult <- processDeploys(runtime, startHash, terms, processDeploy(runtime))
        } yield (startHash, evalResult._1, evalResult._2)
      }
    }
  }

  def replayComputeState(startHash: StateHash)(
      terms: Seq[ProcessedDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator],
      isGenesis: Boolean //FIXME have a better way of knowing this. Pass the replayDeploy function maybe?
  ): F[Either[ReplayFailure, StateHash]] =
    withRuntimeLock { runtime =>
      Span[F].trace(replayComputeStateLabel) {
        for {
          _ <- runtime.blockData.set(blockData)
          _ <- setInvalidBlocks(invalidBlocks, runtime)
          _ <- Span[F].mark("before-replay-deploys")
          result <- replayDeploys(
                     runtime,
                     startHash,
                     terms,
                     replayDeploy(runtime, withCostAccounting = !isGenesis)
                   )
        } yield result
      }
    }

  private def processDeploys(
      runtime: Runtime[F],
      startHash: StateHash,
      terms: Seq[Signed[DeployData]],
      processDeploy: Signed[DeployData] => F[ProcessedDeploy]
  ): F[(StateHash, Seq[ProcessedDeploy])] = {
    import cats.instances.list._

    for {
      _               <- runtime.space.reset(Blake2b256Hash.fromByteString(startHash))
      res             <- terms.toList.traverse(processDeploy)
      _               <- Span[F].mark("before-process-deploys-create-checkpoint")
      finalCheckpoint <- runtime.space.createCheckpoint()
      finalStateHash  = finalCheckpoint.root
    } yield (finalStateHash.toByteString, res)
  }

  private def processDeployWithCostAccounting(
      runtime: Runtime[F]
  )(deploy: Signed[DeployData])(implicit Log: Log[F]) = {
    import cats.instances.vector._
    val rand = Tools.rng(deploy.sig.toByteArray)
    EitherT(
      WriterT(
        Log.info(
          s"PreCharging ${Base16.encode(deploy.pk.bytes)} for ${deploy.data.totalPhloCharge}"
        ) >>
          /* execute pre-charge */
          playSystemDeployInternal(runtime)(
            new PreChargeDeploy(
              deploy.data.totalPhloCharge,
              deploy.pk,
              rand
            )
          )
      )
    ).semiflatMap( // execute user deploy
        _ => WriterT(processDeploy(runtime)(deploy).map(pd => (pd.deployLog.toVector, pd)))
      )
      .flatTap(
        pd =>
          /*execute Refund as a side effect (keeping the logs and the possible error but discarding result)*/
          EitherT(
            WriterT(
              Log.info(
                s"Refunding ${Base16.encode(deploy.pk.bytes)} with ${pd.refundAmount}"
              ) >>
                playSystemDeployInternal(runtime)(
                  new RefundDeploy(pd.refundAmount, rand.splitByte(64))
                )
            )
          ).leftSemiflatMap(
            error =>
              /*If pre-charge succeeds and refund fails, it's a platform error and we should signal it with raiseError*/ WriterT
                .liftF(
                  Log.warn(s"Refund failure '${error.errorMsg}'") >> GasRefundFailure(
                    error.errorMsg
                  ).raiseError
                )
          )
      )
      .onError {
        case SystemDeployError(errorMsg) =>
          EitherT.right(WriterT.liftF(Log.warn(s"Deploy failure '$errorMsg'")))
      }
      .valueOr {
        /* we can end up here if any of the PreCharge threw an user error
        we still keep the logs (from the underlying WriterT) which we'll fill in the next step.
        We're assigning it 0 cost - replay should reach the same state
         */
        case SystemDeployError(errorMsg) =>
          ProcessedDeploy
            .empty(deploy)
            .copy(systemDeployError = Some(errorMsg))
      }
      .run // run the computation and produce the logs
      .map { case (accLog, pd) => pd.copy(deployLog = accLog.toList) }
  }

  private def processDeploy(runtime: Runtime[F])(
      deploy: Signed[DeployData]
  ): F[ProcessedDeploy] = Span[F].withMarks("process-deploy") {
    for {
      fallback                     <- runtime.space.createSoftCheckpoint()
      evaluateResult               <- evaluate(runtime.reducer, runtime.cost, runtime.errorLog)(deploy)
      EvaluateResult(cost, errors) = evaluateResult
      _                            <- Span[F].mark("before-process-deploy-create-soft-checkpoint")
      checkpoint                   <- runtime.space.createSoftCheckpoint()
      deployResult = ProcessedDeploy(
        deploy,
        Cost.toProto(cost),
        checkpoint.log.map(EventConverter.toCasperEvent).toList,
        errors.nonEmpty
      )
      _ <- if (errors.nonEmpty) runtime.space.revertToSoftCheckpoint(fallback)
          else Applicative[F].unit
    } yield deployResult
  }

  private def replayDeploys(
      runtime: Runtime[F],
      startHash: StateHash,
      terms: Seq[ProcessedDeploy],
      replayDeploy: ProcessedDeploy => F[Option[ReplayFailure]]
  ): F[Either[ReplayFailure, StateHash]] =
    (for {
      _ <- EitherT.right(runtime.replaySpace.reset(Blake2b256Hash.fromByteString(startHash)))
      _ <- EitherT(
            terms.tailRecM { ts =>
              if (ts.isEmpty)
                ().asRight[ReplayFailure].asRight[Seq[ProcessedDeploy]].pure[F]
              else replayDeploy(ts.head).map(_.map(_.asLeft[Unit]).toRight(ts.tail))
            }
          )
      _   <- EitherT.right(Span[F].mark("before-replay-deploys-create-checkpoint"))
      res <- EitherT.right[ReplayFailure](runtime.replaySpace.createCheckpoint())
    } yield res.root.toByteString).value

  private def replayDeploy(runtime: Runtime[F], withCostAccounting: Boolean)(
      processedDeploy: ProcessedDeploy
  ): F[Option[ReplayFailure]] = {
    import processedDeploy._

    val deployEvaluator = EitherT
      .liftF {
        for {
          fallback <- runtime.replaySpace.createSoftCheckpoint()
          result   <- evaluate(runtime.replayReducer, runtime.cost, runtime.errorLog)(deploy)
          /* Since the state of `replaySpace` is reset on each invocation of `replayComputeState`,
            and `ReplayFailure`s mean that block processing is cancelled upstream, we only need to
            reset state if the replay effects of valid deploys need to be discarded. */
          _ <- runtime.replaySpace.revertToSoftCheckpoint(fallback).whenA(result.failed)
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

    def evaluatorT: EitherT[F, ReplayFailure, Unit] =
      if (withCostAccounting) {
        val rand            = Tools.rng(deploy.sig.toByteArray)
        val expectedFailure = processedDeploy.systemDeployError.map(SystemDeployError)
        replaySystemDeployInternal(runtime)(
          new PreChargeDeploy(deploy.data.totalPhloCharge, deploy.pk, rand),
          expectedFailure
        ).flatMap(
          _ =>
            if (expectedFailure.isEmpty)
              deployEvaluator
                .semiflatMap(
                  evalResult =>
                    runtime.replaySpace.createSoftCheckpoint().whenA(evalResult.succeeded)
                )
                .flatTap(
                  _ =>
                    replaySystemDeployInternal(runtime)(
                      new RefundDeploy(refundAmount, rand.splitByte(64)),
                      None
                    )
                )
            else EitherT.rightT(())
        )
      } else deployEvaluator.void

    Span[F].withMarks("replay-deploy") {
      for {
        _ <- runtime.replaySpace.rig(processedDeploy.deployLog.map(EventConverter.toRspaceEvent))
        _ <- Span[F].mark("before-replay-deploy-compute-effect")
        failureOption <- evaluatorT
                          .flatMap { _ =>
                            /* This deployment represents either correct program `Some(result)`,
                              or we have a failed pre-charge (`None`) but we agree on that it failed.
                              In both cases we want to check reply data and see if everything is in order */
                            runtime.replaySpace.checkReplayData().attemptT.leftMap {
                              case replayException: ReplayException =>
                                ReplayFailure.unusedCOMMEvent(replayException)
                              case throwable => ReplayFailure.internalError(throwable)
                            }
                          }
                          .swap
                          .value
                          .map(_.toOption)

      } yield failureOption
    }
  }

  def captureResults(
      start: StateHash,
      deploy: Signed[DeployData],
      name: String = "__SCALA__"
  ): F[Seq[Par]] =
    captureResults(start, deploy, Par().withExprs(Seq(Expr(GString(name)))))

  def captureResults(start: StateHash, deploy: Signed[DeployData], name: Par): F[Seq[Par]] =
    withResetRuntimeLock(start) { runtime =>
      evaluate(runtime.reducer, runtime.cost, runtime.errorLog)(deploy)
        .ensure(
          BugFoundError("Unexpected error while capturing results from rholang")
        )(
          _.errors.isEmpty
        ) >> getData(runtime)(name)
    }

  private def setInvalidBlocks(
      invalidBlocks: Map[BlockHash, Validator],
      runtime: Runtime[F]
  ): F[Unit] = {
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

  def computeBonds(hash: StateHash): F[Seq[Bond]] =
    captureResults(hash, ConstructDeploy.sourceDeployNow(bondsQuerySource()))
      .ensureOr(
        bondsPar =>
          new IllegalArgumentException(
            s"Incorrect number of results from query of current bonds: ${bondsPar.size}"
          )
      )(bondsPar => bondsPar.size == 1)
      .map { bondsPar =>
        toBondSeq(bondsPar.head)
      }

  private def bondsQuerySource(name: String = "__SCALA__"): String =
    s"""
       # new rl(`rho:registry:lookup`), poSCh in {
       #   rl!(`rho:rchain:pos`, *poSCh) |
       #   for(@(_, PoS) <- poSCh) {
       #     @PoS!("getBonds", "$name")
       #   }
       # }
       """.stripMargin('#')

  def withRuntimeLock[A](f: Runtime[F] => F[A]): F[A] =
    Sync[F].bracket {
      import coop.rchain.metrics.implicits._
      implicit val ms: Source = RuntimeManagerMetricsSource
      for {
        _       <- Metrics[F].incrementGauge("lock.queue")
        runtime <- Sync[F].defer(runtimeContainer.take).timer("lock.acquire")
        _       <- Metrics[F].decrementGauge("lock.queue")
      } yield runtime
    }(f)(runtimeContainer.put)

  private def withResetRuntimeLock[R](hash: StateHash)(block: Runtime[F] => F[R]): F[R] =
    withRuntimeLock(
      runtime => runtime.space.reset(Blake2b256Hash.fromByteString(hash)) >> block(runtime)
    )

  private def toBondSeq(bondsMap: Par): Seq[Bond] =
    bondsMap.exprs.head.getEMapBody.ps.map {
      case (validator: Par, bond: Par) =>
        assert(validator.exprs.length == 1, "Validator in bonds map wasn't a single string.")
        assert(bond.exprs.length == 1, "Stake in bonds map wasn't a single integer.")
        val validatorName = validator.exprs.head.getGByteArray
        val stakeAmount   = bond.exprs.head.getGInt
        Bond(validatorName, stakeAmount)
    }.toList

  def getData(hash: StateHash)(channel: Par): F[Seq[Par]] =
    withResetRuntimeLock(hash)(getData(_)(channel))

  private def getData(
      runtime: Runtime[F]
  )(channel: Par): F[Seq[Par]] =
    runtime.space.getData(channel).map(_.flatMap(_.a.pars))

  def getContinuation(
      hash: StateHash
  )(channels: Seq[Par]): F[Seq[(Seq[BindPattern], Par)]] =
    withResetRuntimeLock(hash)(
      _.space
        .getWaitingContinuations(channels)
        .map(
          _.filter(_.continuation.taggedCont.isParBody)
            .map(result => (result.patterns, result.continuation.taggedCont.parBody.get.body))
        )
    )

}

object RuntimeManager {

  type StateHash = ByteString

  def fromRuntime[F[_]: Concurrent: Sync: Metrics: Span: Log](
      runtime: Runtime[F]
  ): F[RuntimeManager[F]] =
    for {
      _                <- runtime.space.clear()
      _                <- runtime.replaySpace.clear()
      _                <- Runtime.bootstrapRegistry(runtime)
      checkpoint       <- runtime.space.createCheckpoint()
      replayCheckpoint <- runtime.replaySpace.createCheckpoint()
      hash             = ByteString.copyFrom(checkpoint.root.bytes.toArray)
      replayHash       = ByteString.copyFrom(replayCheckpoint.root.bytes.toArray)
      _                = assert(hash == replayHash)
      runtime          <- MVar[F].of(runtime)
    } yield new RuntimeManagerImpl(hash, runtime)

  def evaluate[F[_]: Sync](
      reducer: Reduce[F],
      costState: _cost[F],
      errorLog: ErrorLog[F]
  )(deploy: Signed[DeployData]): F[EvaluateResult] = {
    import coop.rchain.models.rholang.implicits._

    implicit val rand: Blake2b512Random =
      Tools.unforgeableNameRng(deploy.pk, deploy.data.timestamp)
    implicit val cost: _cost[F] = costState

    Interpreter[F].injAttempt(
      reducer,
      errorLog,
      deploy.data.term,
      Cost(deploy.data.phloLimit),
      NormalizerEnv(deploy).toEnv
    )
  }
}
