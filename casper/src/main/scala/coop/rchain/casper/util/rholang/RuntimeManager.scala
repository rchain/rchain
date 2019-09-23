package coop.rchain.casper.util.rholang

import cats._
import cats.data.EitherT
import cats.effect.concurrent.MVar
import cats.effect.{Sync, _}
import cats.implicits._

import com.google.protobuf.ByteString
import coop.rchain.casper.CasperMetricsSource
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.Metrics.Source
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.Validator.Validator
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.BugFoundError
import coop.rchain.rholang.interpreter.{
  ErrorLog,
  EvaluateResult,
  Interpreter,
  NormalizerEnv,
  Reduce,
  RhoType,
  Runtime
}
import coop.rchain.rspace.{trace, Blake2b256Hash, ReplayException}
import coop.rchain.shared.Log

trait RuntimeManager[F[_]] {
  import RuntimeManager.ReplayFailure

  def captureResults(
      startHash: StateHash,
      deploy: DeployData,
      name: String = "__SCALA__"
  ): F[Seq[Par]]
  def replayComputeState(startHash: StateHash)(
      terms: Seq[InternalProcessedDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator],
      isGenesis: Boolean
  ): F[Either[ReplayFailure, StateHash]]
  def computeState(hash: StateHash)(
      terms: Seq[DeployData],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator]
  ): F[(StateHash, Seq[InternalProcessedDeploy])]
  def computeGenesis(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[InternalProcessedDeploy])]
  def computeBonds(startHash: StateHash): F[Seq[Bond]]
  def getData(hash: StateHash)(channel: Par): F[Seq[Par]]
  def getContinuation(hash: StateHash)(
      channels: Seq[Par]
  ): F[Seq[(Seq[BindPattern], Par)]]
  def emptyStateHash: StateHash
  def withRuntimeLock[A](f: Runtime[F] => F[A]): F[A]
}

class RuntimeManagerImpl[F[_]: Concurrent: Metrics: Span: Log] private[rholang] (
    val emptyStateHash: StateHash,
    runtimeContainer: MVar[F, Runtime[F]]
) extends RuntimeManager[F] {
  import RuntimeManager.ReplayFailure

  private[this] val RuntimeManagerMetricsSource =
    Metrics.Source(CasperMetricsSource, "runtime-manager")
  private[this] val replayComputeStateLabel =
    Metrics.Source(RuntimeManagerMetricsSource, "replay-compute-state")
  private[this] val computeStateLabel = Metrics.Source(RuntimeManagerMetricsSource, "compute-state")
  private[this] val computeGenesisLabel =
    Metrics.Source(RuntimeManagerMetricsSource, "compute-genesis")

  def captureResults(
      start: StateHash,
      deploy: DeployData,
      name: String = "__SCALA__"
  ): F[Seq[Par]] =
    captureResults(start, deploy, Par().withExprs(Seq(Expr(GString(name)))))

  def captureResults(start: StateHash, deploy: DeployData, name: Par): F[Seq[Par]] =
    withResetRuntimeLock(start) { runtime =>
      computeEffect(runtime, runtime.reducer)(deploy)
        .ensure(
          BugFoundError("Unexpected error while capturing results from rholang")
        )(
          _.errors.isEmpty
        ) >> getData(runtime)(name)
    }

  private def computeEffect(runtime: Runtime[F], reducer: Reduce[F])(
      deploy: DeployData
  ): F[EvaluateResult] =
    for {
      _      <- runtime.deployParametersRef.set(ProtoUtil.getRholangDeployParams(deploy))
      result <- doInj(deploy, reducer, runtime.errorLog)(runtime.cost)
    } yield result

  def replayComputeState(startHash: StateHash)(
      terms: Seq[InternalProcessedDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator],
      isGenesis: Boolean //FIXME have a better way of knowing this. Pass the replayDeploy function maybe?
  ): F[Either[ReplayFailure, StateHash]] =
    withRuntimeLock { runtime =>
      Span[F].trace(replayComputeStateLabel) {
        for {
          _      <- runtime.blockData.set(blockData)
          _      <- setInvalidBlocks(invalidBlocks, runtime)
          _      <- Span[F].mark("before-replay-deploys")
          result <- replayDeploys(runtime, startHash, terms, replayDeploy(runtime))
        } yield result
      }
    }

  def computeState(startHash: StateHash)(
      terms: Seq[DeployData],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator]
  ): F[(StateHash, Seq[InternalProcessedDeploy])] =
    withRuntimeLock { runtime =>
      Span[F].trace(computeStateLabel) {
        for {
          _      <- runtime.blockData.set(blockData)
          _      <- setInvalidBlocks(invalidBlocks, runtime)
          _      <- Span[F].mark("before-process-deploys")
          result <- processDeploys(runtime, startHash, terms, processDeploy(runtime))
        } yield result
      }
    }

  def computeGenesis(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[InternalProcessedDeploy])] = {
    val startHash = emptyStateHash
    withRuntimeLock { runtime =>
      Span[F].trace(computeGenesisLabel) {
        for {
          _          <- runtime.blockData.set(BlockData(blockTime, 0))
          _          <- Span[F].mark("before-process-deploys")
          evalResult <- processDeploys(runtime, startHash, terms, processDeploy(runtime))
        } yield (startHash, evalResult._1, evalResult._2)
      }
    }
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

  private def processDeploys(
      runtime: Runtime[F],
      startHash: StateHash,
      terms: Seq[DeployData],
      processDeploy: DeployData => F[InternalProcessedDeploy]
  ): F[(StateHash, Seq[InternalProcessedDeploy])] =
    for {
      _ <- runtime.space.reset(Blake2b256Hash.fromByteString(startHash))
      res <- terms.toList
              .foldM(Seq.empty[InternalProcessedDeploy]) {
                case (results, deploy) => processDeploy(deploy).map(results :+ _)
              }
      _               <- Span[F].mark("before-process-deploys-create-checkpoint")
      finalCheckpoint <- runtime.space.createCheckpoint()
      finalStateHash  = finalCheckpoint.root
    } yield (finalStateHash.toByteString, res)

  private def processDeploy(runtime: Runtime[F])(
      deploy: DeployData
  ): F[InternalProcessedDeploy] = Span[F].withMarks("process-deploy") {
    for {
      fallback                     <- runtime.space.createSoftCheckpoint()
      evaluateResult               <- computeEffect(runtime, runtime.reducer)(deploy)
      EvaluateResult(cost, errors) = evaluateResult
      _                            <- Span[F].mark("before-process-deploy-create-soft-checkpoint")
      checkpoint                   <- runtime.space.createSoftCheckpoint()
      deployResult = InternalProcessedDeploy(
        deploy,
        Cost.toProto(cost),
        checkpoint.log,
        Seq.empty[trace.Event],
        DeployStatus.fromErrors(errors)
      )
      _ <- if (errors.nonEmpty) runtime.space.revertToSoftCheckpoint(fallback)
          else Applicative[F].unit
    } yield deployResult
  }

  private def replayDeploys(
      runtime: Runtime[F],
      startHash: StateHash,
      terms: Seq[InternalProcessedDeploy],
      replayDeploy: InternalProcessedDeploy => F[Option[ReplayFailure]]
  ): F[Either[ReplayFailure, StateHash]] =
    (for {
      _ <- EitherT.right(runtime.replaySpace.reset(Blake2b256Hash.fromByteString(startHash)))
      _ <- EitherT(
            terms.tailRecM { ts =>
              if (ts.isEmpty)
                ().asRight[ReplayFailure].asRight[Seq[InternalProcessedDeploy]].pure[F]
              else replayDeploy(ts.head).map(_.map(_.asLeft[Unit]).toRight(ts.tail))
            }
          )
      _   <- EitherT.right(Span[F].mark("before-replay-deploys-create-checkpoint"))
      res <- EitherT.right[ReplayFailure](runtime.replaySpace.createCheckpoint())
    } yield res.root.toByteString).value

  private def replayDeploy(runtime: Runtime[F])(
      processedDeploy: InternalProcessedDeploy
  ): F[Option[ReplayFailure]] = Span[F].withMarks("replay-deploy") {
    import processedDeploy._
    for {
      _                    <- runtime.replaySpace.rig(processedDeploy.deployLog)
      softCheckpoint       <- runtime.replaySpace.createSoftCheckpoint()
      _                    <- Span[F].mark("before-replay-deploy-compute-effect")
      replayEvaluateResult <- computeEffect(runtime, runtime.replayReducer)(processedDeploy.deploy)
      //TODO: compare replay deploy cost to given deploy cost
      EvaluateResult(_, errors) = replayEvaluateResult
      _                         <- Span[F].mark("before-replay-deploy-status")
      cont <- DeployStatus.fromErrors(errors) match {
               case int: InternalErrors =>
                 (deploy.some, int: Failed).some.pure[F]
               case replayStatus =>
                 if (status.isFailed != replayStatus.isFailed)
                   (deploy.some, ReplayStatusMismatch(replayStatus, status): Failed).some
                     .pure[F]
                 else if (errors.nonEmpty)
                   runtime.replaySpace.revertToSoftCheckpoint(softCheckpoint) >> none[ReplayFailure]
                     .pure[F]
                 else {
                   runtime.replaySpace
                     .checkReplayData()
                     .attempt
                     .flatMap {
                       case Right(_) => none[ReplayFailure].pure[F]
                       case Left(ex: ReplayException) => {
                         Log[F].error(s"Failed during processing of deploy: ${processedDeploy}") >>
                           (none[DeployData], UnusedCommEvent(ex): Failed).some
                             .pure[F]
                       }
                       case Left(ex) =>
                         (none[DeployData], UserErrors(Vector(ex)): Failed).some
                           .pure[F]
                     }
                 }
             }
    } yield cont
  }

  private[this] def doInj(
      deploy: DeployData,
      reducer: Reduce[F],
      errorLog: ErrorLog[F]
  )(implicit C: _cost[F]): F[EvaluateResult] = {
    implicit val rand: Blake2b512Random = Blake2b512Random(
      ProtoUtil.stripDeployData(deploy).toProto.toByteArray
    )
    Interpreter[F].injAttempt(
      reducer,
      errorLog,
      deploy.term,
      Cost(deploy.phloLimit),
      NormalizerEnv(deploy.toProto)
    )
  }
}

object RuntimeManager {

  type StateHash     = ByteString
  type ReplayFailure = (Option[DeployData], Failed)

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
}
