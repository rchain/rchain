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
import coop.rchain.catscontrib.Catscontrib.ToMonadOps
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.Validator.Validator
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.{BlockData, RhoISpace}
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.BugFoundError
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rholang.interpreter.{
  ChargingReducer,
  ErrorLog,
  EvaluateResult,
  Interpreter,
  RhoType,
  Runtime,
  accounting,
  PrettyPrinter => RholangPrinter
}
import coop.rchain.rspace.{trace, Blake2b256Hash, Checkpoint, ReplayException}

trait RuntimeManager[F[_]] {

  type ReplayFailure = (Option[DeployData], Failed)

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
  ): F[Either[(Option[DeployData], Failed), StateHash]]
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
}

class RuntimeManagerImpl[F[_]: Concurrent: Metrics] private[rholang] (
    val emptyStateHash: StateHash,
    runtimeContainer: MVar[F, Runtime[F]]
) extends RuntimeManager[F] {

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

  private def computeEffect(runtime: Runtime[F], reducer: ChargingReducer[F])(
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
      for {
        span <- Metrics[F].span(replayComputeStateLabel)
        _    <- runtime.blockData.setParams(blockData)
        _    <- setInvalidBlocks(invalidBlocks, runtime)
        _    <- span.mark("before-replay-deploys")
        replayFunction = if (isGenesis) replayDeploy(runtime, span) _
        else replayDeployWithPayment(runtime, span) _
        result <- replayDeploys(startHash, terms, replayFunction)
        _      <- span.close()
      } yield result
    }

  def computeState(startHash: StateHash)(
      terms: Seq[DeployData],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator]
  ): F[(StateHash, Seq[InternalProcessedDeploy])] =
    withRuntimeLock { runtime =>
      for {
        span   <- Metrics[F].span(computeStateLabel)
        _      <- runtime.blockData.setParams(blockData)
        _      <- setInvalidBlocks(invalidBlocks, runtime)
        _      <- span.mark("before-process-deploys")
        result <- processDeploys(startHash, terms, processDeployWithPayment(runtime, span))
        _      <- span.close()
      } yield result
    }

  def computeGenesis(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[InternalProcessedDeploy])] = {
    val startHash = emptyStateHash
    withRuntimeLock { runtime =>
      for {
        span       <- Metrics[F].span(computeGenesisLabel)
        _          <- runtime.blockData.setParams(BlockData(blockTime, 0))
        _          <- span.mark("before-process-deploys")
        evalResult <- processDeploys(startHash, terms, processDeploy(runtime, span))
        _          <- span.close()
      } yield (startHash, evalResult._1, evalResult._2)
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

  private def computeDeployPayment(
      runtime: Runtime[F],
      reducer: ChargingReducer[F],
      space: RhoISpace[F]
  )(deploy: DeployData): F[Either[String, Checkpoint]] =
    for {
      _ <- computeEffect(runtime, reducer)(
            ConstructDeploy
              .sourceDeploy(
                deployPaymentSource(deploy.phloLimit * deploy.phloPrice),
                timestamp = deploy.timestamp,
                accounting.MAX_VALUE
              )
              .withDeployer(deploy.deployer)
          ).ensureOr(r => BugFoundError("Deploy payment failed unexpectedly" + r.errors))(
            _.errors.isEmpty
          )
      consumeResult <- getResult(runtime, space)()
      result <- consumeResult match {
                 case Seq(RhoType.Tuple2(RhoType.Boolean(true), Par.defaultInstance)) =>
                   space.createCheckpoint().map(_.asRight[String])
                 case Seq(RhoType.Tuple2(RhoType.Boolean(false), RhoType.String(error))) =>
                   error
                     .asLeft[Checkpoint]
                     .pure[F]
                 case Seq() =>
                   space.createCheckpoint() >>
                     BugFoundError("Expected response message was not received")
                       .raiseError[F, Either[String, Checkpoint]]
                 case other =>
                   val contentAsStr = other.map(RholangPrinter().buildString(_)).mkString(",")
                   BugFoundError(
                     s"Deploy payment returned unexpected result: [$contentAsStr ]"
                   ).raiseError[F, Either[String, Checkpoint]]
               }
    } yield result

  private def deployPaymentSource(amount: Long, name: String = "__SCALA__"): String =
    s"""
       # new rl(`rho:registry:lookup`), poSCh in {
       #   rl!(`rho:rchain:pos`, *poSCh) |
       #   for(@(_, PoS) <- poSCh) {
       #     @PoS!("pay", $amount, "$name")
       #   }
       # }
       """.stripMargin('#')

  private def withRuntimeLock[A](f: Runtime[F] => F[A]): F[A] =
    Sync[F].bracket(runtimeContainer.take)(f)(runtimeContainer.put)

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
        val stakeAmount   = bond.exprs.head.getETupleBody.ps.head.exprs.head.getGInt
        Bond(validatorName, stakeAmount)
    }.toList

  def getData(hash: StateHash)(channel: Par): F[Seq[Par]] =
    withResetRuntimeLock(hash)(getData(_)(channel))

  private def getData(
      runtime: Runtime[F]
  )(channel: Par): F[Seq[Par]] =
    runtime.space.getData(channel).map(_.flatMap(_.a.pars))

  private def getResult(runtime: Runtime[F], space: RhoISpace[F])(
      name: String = "__SCALA__"
  ): F[Seq[Par]] = {

    val channel                 = Par().withExprs(Seq(Expr(GString(name))))
    val pattern                 = BindPattern(Seq(EVar(FreeVar(0))), freeCount = 1)
    val cont                    = TaggedContinuation().withParBody(ParWithRandom(Par()))
    implicit val cost: _cost[F] = runtime.cost

    space.consume(Seq(channel), Seq(pattern), cont, persist = false)(matchListPar).map {
      case Some((_, dataList)) => dataList.flatMap(_.value.pars)
      case None                => Seq.empty[Par]
    }
  }

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
      startHash: StateHash,
      terms: Seq[DeployData],
      processDeploy: (Blake2b256Hash, DeployData) => F[(Blake2b256Hash, InternalProcessedDeploy)]
  ): F[(StateHash, Seq[InternalProcessedDeploy])] = {

    val startHashBlake = Blake2b256Hash.fromByteString(startHash)
    terms.toList
      .foldM((startHashBlake, Seq.empty[InternalProcessedDeploy])) {
        case ((hash, results), deploy) => {
          processDeploy(hash, deploy).map(_.bimap(identity, results :+ _))
        }
      }
      .map(_.bimap(_.toByteString, x => x))
  }

  private def processDeploy(runtime: Runtime[F], span: Span[F])(
      startHash: Blake2b256Hash,
      deploy: DeployData
  ): F[(Blake2b256Hash, InternalProcessedDeploy)] =
    for {
      _                            <- span.mark("before-process-deploy-reset")
      _                            <- runtime.space.reset(startHash)
      _                            <- span.mark("before-process-deploy-compute-effect")
      evaluateResult               <- computeEffect(runtime, runtime.reducer)(deploy)
      EvaluateResult(cost, errors) = evaluateResult
      _                            <- span.mark("before-process-deploy-create-checkpoint")
      newCheckpoint                <- runtime.space.createCheckpoint()
      deployResult = InternalProcessedDeploy(
        deploy,
        Cost.toProto(cost),
        newCheckpoint.log,
        Seq.empty[trace.Event],
        DeployStatus.fromErrors(errors)
      )
      newHash = if (errors.isEmpty) newCheckpoint.root else startHash
      _       <- span.mark("process-deploy-finished")
    } yield (newHash, deployResult)

  private def processDeployWithPayment(runtime: Runtime[F], span: Span[F])(
      startHash: Blake2b256Hash,
      deploy: DeployData
  ): F[(Blake2b256Hash, InternalProcessedDeploy)] =
    for {
      _         <- runtime.space.reset(startHash)
      payResult <- computeDeployPayment(runtime, runtime.reducer, runtime.space)(deploy)
      result <- payResult.fold(
                 error => (startHash, InternalProcessedDeploy.failedPayment(deploy, error)).pure[F],
                 postPaymentCheckpoint =>
                   processDeploy(runtime, span)(postPaymentCheckpoint.root, deploy)
                     .map(_.bimap(x => x, _.copy(paymentLog = postPaymentCheckpoint.log)))
               )
      //FIXME add refund of remaining phlo
    } yield result

  private def replayDeploys(
      startHash: StateHash,
      terms: Seq[InternalProcessedDeploy],
      replayDeploy: (
          Blake2b256Hash,
          InternalProcessedDeploy
      ) => F[Either[ReplayFailure, Blake2b256Hash]]
  ): F[Either[ReplayFailure, StateHash]] = {

    val startHashBlake = Blake2b256Hash.fromByteString(startHash)
    val result = terms.toList.foldM(startHashBlake.asRight[ReplayFailure]) {
      case (hash, deploy) =>
        hash.flatTraverse(replayDeploy(_, deploy))
    }
    result.nested.map(_.toByteString).value
  }

  private def replayDeployWithPayment(runtime: Runtime[F], span: Span[F])(
      startHash: Blake2b256Hash,
      processedDeploy: InternalProcessedDeploy
  ): F[Either[ReplayFailure, Blake2b256Hash]] =
    for {
      deploy <- processedDeploy.deploy.pure[F]
      _      <- runtime.replaySpace.resetAndRig(startHash, processedDeploy.paymentLog)
      payResult <- computeDeployPayment(runtime, runtime.replayReducer, runtime.replaySpace)(deploy)
      result <- payResult.fold(
                 error =>
                   ((deploy.some, UnknownFailure: Failed /* FIXME */ ))
                     .asLeft[Blake2b256Hash]
                     .pure[F],
                 postPaymentCheckpoint =>
                   replayDeploy(runtime, span)(postPaymentCheckpoint.root, processedDeploy)
               )
      //FIXME add refund of remaining phlo
    } yield result

  /*

- do printlns for the test, get a grip what's the context
- yea.h.hhh...
  - add _.reducer parameter to computeDeployPayment

   */

  private def replayDeploy(runtime: Runtime[F], span: Span[F])(
      startHash: Blake2b256Hash,
      processedDeploy: InternalProcessedDeploy
  ): F[Either[ReplayFailure, Blake2b256Hash]] = {
    import processedDeploy._
    for {
      _                    <- span.mark("before-replay-deploy-reset-rig")
      _                    <- runtime.replaySpace.resetAndRig(startHash, processedDeploy.deployLog)
      _                    <- span.mark("before-replay-deploy-compute-effect")
      replayEvaluateResult <- computeEffect(runtime, runtime.replayReducer)(processedDeploy.deploy)
      //TODO: compare replay deploy cost to given deploy cost
      EvaluateResult(_, errors) = replayEvaluateResult
      _                         <- span.mark("before-replay-deploy-status")
      cont <- DeployStatus.fromErrors(errors) match {
               case int: InternalErrors =>
                 (deploy.some, int: Failed).asLeft[Blake2b256Hash].pure[F]
               case replayStatus =>
                 if (status.isFailed != replayStatus.isFailed)
                   (deploy.some, ReplayStatusMismatch(replayStatus, status): Failed)
                     .asLeft[Blake2b256Hash]
                     .pure[F]
                 else if (errors.nonEmpty)
                   startHash.asRight[ReplayFailure].pure[F]
                 else
                   for {
                     _             <- span.mark("before-replay-deploy-create-checkpoint")
                     checkpointErr <- runtime.replaySpace.createCheckpoint().attempt
                     result <- checkpointErr match {
                                case Right(newCheckpoint) =>
                                  newCheckpoint.root
                                    .asRight[ReplayFailure]
                                    .pure[F]
                                case Left(ex: ReplayException) =>
                                  (none[DeployData], UnusedCommEvent(ex): Failed)
                                    .asLeft[Blake2b256Hash]
                                    .pure[F]
                                case Left(ex) =>
                                  (none[DeployData], UserErrors(Vector(ex)): Failed)
                                    .asLeft[Blake2b256Hash]
                                    .pure[F]
                              }
                   } yield result
             }
      _ <- span.mark("replay-deploy-finished")
    } yield cont
  }

  private[this] def doInj(
      deploy: DeployData,
      reducer: ChargingReducer[F],
      errorLog: ErrorLog[F]
  )(implicit C: _cost[F]): F[EvaluateResult] = {
    implicit val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy))
    )
    Interpreter[F].injAttempt(
      reducer,
      errorLog,
      deploy.term,
      Cost(deploy.phloLimit),
      PublicKey(deploy.deployer.toByteArray).some
    )
  }
}

object RuntimeManager {

  type StateHash = ByteString

  def fromRuntime[F[_]: Concurrent: Sync: Metrics](
      active: Runtime[F]
  ): F[RuntimeManager[F]] =
    for {
      _                <- active.space.clear()
      _                <- active.replaySpace.clear()
      _                <- Runtime.injectEmptyRegistryRoot(active.space, active.replaySpace)
      checkpoint       <- active.space.createCheckpoint()
      replayCheckpoint <- active.replaySpace.createCheckpoint()
      hash             = ByteString.copyFrom(checkpoint.root.bytes.toArray)
      replayHash       = ByteString.copyFrom(replayCheckpoint.root.bytes.toArray)
      _                = assert(hash == replayHash)
      runtime          <- MVar[F].of(active)
    } yield new RuntimeManagerImpl(hash, runtime)

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      runtimeManager: RuntimeManager[F]
  ): RuntimeManager[T[F, ?]] =
    new RuntimeManager[T[F, ?]] {

      override def captureResults(
          start: StateHash,
          deploy: DeployData,
          name: String
      ): T[F, Seq[Par]] = runtimeManager.captureResults(start, deploy, name).liftM[T]

      override def replayComputeState(hash: StateHash)(
          terms: Seq[InternalProcessedDeploy],
          blockData: BlockData,
          invalidBlocks: Map[BlockHash, Validator],
          isGenesis: Boolean
      ): T[F, Either[ReplayFailure, StateHash]] =
        runtimeManager.replayComputeState(hash)(terms, blockData, invalidBlocks, isGenesis).liftM[T]

      override def computeState(hash: StateHash)(
          terms: Seq[DeployData],
          blockData: BlockData,
          invalidBlocks: Map[BlockHash, Validator]
      ): T[F, (StateHash, Seq[InternalProcessedDeploy])] =
        runtimeManager.computeState(hash)(terms, blockData, invalidBlocks).liftM[T]

      def computeGenesis(
          terms: Seq[DeployData],
          blockTime: Long
      ): T[F, (StateHash, StateHash, Seq[InternalProcessedDeploy])] =
        runtimeManager.computeGenesis(terms, blockTime).liftM[T]

      override def computeBonds(hash: StateHash): T[F, Seq[Bond]] =
        runtimeManager.computeBonds(hash).liftM[T]

      override def getData(hash: StateHash)(channel: Par): T[F, Seq[Par]] =
        runtimeManager.getData(hash)(channel).liftM[T]

      override def getContinuation(
          hash: StateHash
      )(channels: Seq[Par]): T[F, Seq[(Seq[BindPattern], Par)]] =
        runtimeManager.getContinuation(hash)(channels).liftM[T]

      override val emptyStateHash: StateHash = runtimeManager.emptyStateHash
    }

  def eitherTRuntimeManager[E, F[_]: Monad](
      rm: RuntimeManager[F]
  ): RuntimeManager[EitherT[F, E, ?]] =
    RuntimeManager.forTrans[F, EitherT[?[_], E, ?]](rm)
}
