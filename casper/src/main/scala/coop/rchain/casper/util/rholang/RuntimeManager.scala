package coop.rchain.casper.util.rholang

import cats._
import cats.data.EitherT
import cats.effect.concurrent.MVar
import cats.effect.{Sync, _}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.catscontrib.Catscontrib.ToMonadOps
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.BugFoundError
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rholang.interpreter.{
  ChargingReducer,
  ErrorLog,
  EvaluateResult,
  Interpreter,
  RhoType,
  Runtime,
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
      blockTime: Long
  ): F[Either[ReplayFailure, StateHash]]
  def computeState(startHash: StateHash)(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, Seq[InternalProcessedDeploy])]
  def computeGenesis(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[InternalProcessedDeploy])]
  def computeBonds(startHash: StateHash): F[Seq[Bond]]
  def computeDeployPayment(startHash: StateHash)(user: ByteString, amount: Long): F[StateHash]
  def getData(hash: StateHash)(channel: Par): F[Seq[Par]]
  def getContinuation(hash: StateHash)(
      channels: Seq[Par]
  ): F[Seq[(Seq[BindPattern], Par)]]
  def emptyStateHash: StateHash
}

class RuntimeManagerImpl[F[_]: Concurrent] private[rholang] (
    val emptyStateHash: StateHash,
    runtimeContainer: MVar[F, Runtime[F]]
) extends RuntimeManager[F] {

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
      blockTime: Long
  ): F[Either[ReplayFailure, StateHash]] =
    withRuntimeLock { runtime =>
      for {
        _      <- setBlockTime(blockTime, runtime)
        result <- replayDeploys(startHash, terms, replayDeploy(runtime))
      } yield result
    }

  def computeState(startHash: StateHash)(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, Seq[InternalProcessedDeploy])] =
    withRuntimeLock { runtime =>
      for {
        _      <- setBlockTime(blockTime, runtime)
        result <- processDeploys(startHash, terms, processDeploy(runtime))
      } yield result
    }

  def computeGenesis(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[InternalProcessedDeploy])] = {
    val startHash = emptyStateHash
    withRuntimeLock { runtime =>
      for {
        _          <- setBlockTime(blockTime, runtime)
        evalResult <- processDeploys(startHash, terms, processDeploy(runtime))
      } yield (startHash, evalResult._1, evalResult._2)
    }
  }

  private def setBlockTime(
      blockTime: Long,
      runtime: Runtime[F]
  ): F[Unit] = {
    val timestamp: Par = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(blockTime))))
    runtime.blockTime.setParams(timestamp)
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
       | new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
       |   rl!(`rho:rchain:systemInstancesRegistry`, *SystemInstancesCh) |
       |   for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
       |     @SystemInstancesRegistry!("lookup", "pos", *posCh) |
       |     for(pos <- posCh){ pos!("getBonds", "$name") }
       |   }
       | }
     """.stripMargin

  def computeDeployPayment(start: StateHash)(user: ByteString, amount: Long): F[StateHash] =
    withResetRuntimeLock(start)(
      runtime =>
        computeDeployPayment(runtime, runtime.reducer, runtime.space)(user, amount)
          .map(_.root.toByteString)
    )

  private def computeDeployPayment(
      runtime: Runtime[F],
      reducer: ChargingReducer[F],
      space: RhoISpace[F]
  )(user: ByteString, amount: Long): F[Checkpoint] =
    for {
      _ <- computeEffect(runtime, reducer)(
            ConstructDeploy.sourceDeployNow(deployPaymentSource(amount)).withDeployer(user)
          ).ensure(BugFoundError("Deploy payment failed unexpectedly"))(_.errors.isEmpty)
      consumeResult <- getResult(runtime, space)()
      result <- consumeResult match {
                 case Seq(RhoType.Tuple2(RhoType.Boolean(true), Par.defaultInstance)) =>
                   space.createCheckpoint()
                 case Seq(RhoType.Tuple2(RhoType.Boolean(false), RhoType.String(error))) =>
                   BugFoundError(s"Deploy payment failed unexpectedly: $error")
                     .raiseError[F, Checkpoint]
                 case Seq() =>
                   BugFoundError("Expected response message was not received")
                     .raiseError[F, Checkpoint]
                 case other =>
                   val contentAsStr = other.map(RholangPrinter().buildString(_)).mkString(",")
                   BugFoundError(
                     s"Deploy payment returned unexpected result: [$contentAsStr ]"
                   ).raiseError[F, Checkpoint]
               }
    } yield result

  private def deployPaymentSource(amount: Long, name: String = "__SCALA__"): String =
    s"""
       | new rl(`rho:registry:lookup`), poSCh in {
       |   rl!(`rho:rchain:pos`, *poSCh) |
       |   for(@(_, PoS) <- poSCh) {
       |     @PoS!("pay", $amount, "$name")
       |   }
       | }
       """.stripMargin

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

  private def processDeploy(runtime: Runtime[F])(
      startHash: Blake2b256Hash,
      deploy: DeployData
  ): F[(Blake2b256Hash, InternalProcessedDeploy)] =
    for {
      _                            <- runtime.space.reset(startHash)
      evaluateResult               <- computeEffect(runtime, runtime.reducer)(deploy)
      EvaluateResult(cost, errors) = evaluateResult
      newCheckpoint                <- runtime.space.createCheckpoint()
      deployResult = InternalProcessedDeploy(
        deploy,
        Cost.toProto(cost),
        newCheckpoint.log,
        Seq.empty[trace.Event],
        DeployStatus.fromErrors(errors)
      )
      newHash = if (errors.isEmpty) newCheckpoint.root else startHash
    } yield (newHash, deployResult)

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

  private def replayDeploy(runtime: Runtime[F])(
      hash: Blake2b256Hash,
      processedDeploy: InternalProcessedDeploy
  ): F[Either[ReplayFailure, Blake2b256Hash]] = {
    import processedDeploy._
    for {
      _                    <- runtime.replaySpace.resetAndRig(hash, processedDeploy.deployLog)
      replayEvaluateResult <- computeEffect(runtime, runtime.replayReducer)(processedDeploy.deploy)
      //TODO: compare replay deploy cost to given deploy cost
      EvaluateResult(cost, errors) = replayEvaluateResult
      cont <- DeployStatus.fromErrors(errors) match {
               case int: InternalErrors =>
                 (deploy.some, int: Failed).asLeft[Blake2b256Hash].pure[F]
               case replayStatus =>
                 if (status.isFailed != replayStatus.isFailed)
                   (deploy.some, ReplayStatusMismatch(replayStatus, status): Failed)
                     .asLeft[Blake2b256Hash]
                     .pure[F]
                 else if (errors.nonEmpty)
                   hash.asRight[ReplayFailure].pure[F]
                 else {
                   runtime.replaySpace
                     .createCheckpoint()
                     .attempt
                     .flatMap {
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
                 }
             }
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
      Cost(deploy.phloLimit)
    )
  }
}

object RuntimeManager {

  type StateHash = ByteString

  def fromRuntime[F[_]: Concurrent: Sync](
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
          blockTime: Long
      ): T[F, Either[ReplayFailure, StateHash]] =
        runtimeManager.replayComputeState(hash)(terms, blockTime).liftM[T]

      override def computeState(hash: StateHash)(
          terms: Seq[DeployData],
          blockTime: Long
      ): T[F, (StateHash, Seq[InternalProcessedDeploy])] =
        runtimeManager.computeState(hash)(terms, blockTime).liftM[T]

      def computeGenesis(
          terms: Seq[DeployData],
          blockTime: Long
      ): T[F, (StateHash, StateHash, Seq[InternalProcessedDeploy])] =
        runtimeManager.computeGenesis(terms, blockTime).liftM[T]

      override def computeBonds(hash: StateHash): T[F, Seq[Bond]] =
        runtimeManager.computeBonds(hash).liftM[T]

      override def computeDeployPayment(
          start: StateHash
      )(user: ByteString, amount: Long): T[F, StateHash] =
        runtimeManager.computeDeployPayment(start)(user, amount).liftM[T]

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
