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
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
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
  def captureResults(start: StateHash, deploy: DeployData, name: String = "__SCALA__"): F[Seq[Par]]
  def captureResults(start: StateHash, deploy: DeployData, name: Par): F[Seq[Par]]
  def replayComputeState(hash: StateHash)(
      terms: Seq[InternalProcessedDeploy],
      blockTime: Long
  ): F[Either[(Option[DeployData], Failed), StateHash]]
  def computeState(hash: StateHash)(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, Seq[InternalProcessedDeploy])]
  def computeGenesis(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[InternalProcessedDeploy])]
  def storageRepr(hash: StateHash): F[Option[String]]
  def computeBonds(hash: StateHash): F[Seq[Bond]]
  def computeBalance(start: StateHash)(user: ByteString): F[Long]
  def computeDeployPayment(start: StateHash)(user: ByteString, amount: Long): F[StateHash]
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
    withResetRuntime(start) { runtime =>
      //TODO: Is better error handling needed here?
      for {
        evaluateResult <- computeEffect(runtime)(deploy)
        result <- if (evaluateResult.errors.isEmpty) getData(runtime)(name)
                 else Seq.empty[Par].pure[F]
      } yield result
    }

  private def computeEffect(runtime: Runtime[F])(deploy: DeployData): F[EvaluateResult] =
    runtime.deployParametersRef.set(ProtoUtil.getRholangDeployParams(deploy)) >>
      doInj(deploy, runtime.reducer, runtime.errorLog)(runtime.cost)

  private def replayComputeEffect(
      runtime: Runtime[F]
  )(start: Blake2b256Hash, processedDeploy: InternalProcessedDeploy): F[EvaluateResult] = {
    import processedDeploy._
    runtime.replaySpace.rig(start, deployLog.toList) >>
      runtime.deployParametersRef.set(ProtoUtil.getRholangDeployParams(deploy)) >>
      doInj(deploy, runtime.replayReducer, runtime.errorLog)(runtime.cost)
  }

  /**
    * @note `replayEval` does not need to reset the evaluation store,
    *       merely the replay store. Hence, `replayComputeState` uses
    *       `withRuntime` rather than `withResetRuntime`.
    */
  def replayComputeState(hash: StateHash)(
      terms: Seq[InternalProcessedDeploy],
      blockTime: Long
  ): F[Either[(Option[DeployData], Failed), StateHash]] =
    withRuntime { runtime =>
      for {
        _      <- setBlockTime(blockTime, runtime)
        result <- replayEval(terms, runtime, hash)
      } yield result
    }

  def computeState(hash: StateHash)(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, Seq[InternalProcessedDeploy])] =
    withResetRuntime(hash) { runtime =>
      for {
        _      <- setBlockTime(blockTime, runtime)
        result <- newEval(terms, runtime, hash)
      } yield result
    }

  def computeGenesis(
      terms: Seq[DeployData],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[InternalProcessedDeploy])] = {
    val startHash = emptyStateHash
    withResetRuntime(startHash) { runtime =>
      for {
        _          <- setBlockTime(blockTime, runtime)
        evalResult <- newEval(terms, runtime, startHash)
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

  def storageRepr(hash: StateHash): F[Option[String]] =
    withResetRuntime(hash)(runtime => StoragePrinter.prettyPrint(runtime.space)).attempt
      .map {
        case Right(print) => Some(print)
        case Left(_)      => None
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
       |   rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
       |   for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
       |     @SystemInstancesRegistry!("lookup", "pos", *posCh) |
       |     for(pos <- posCh){ pos!("getBonds", "$name") }
       |   }
       | }
     """.stripMargin

  def computeBalance(start: StateHash)(user: ByteString): F[Long] =
    withResetRuntime(start)(computeBalance(_)(user))

  private def computeBalance(runtime: Runtime[F])(user: ByteString): F[Long] =
    computeEffect(runtime)(
      ConstructDeploy.sourceDeployNow(balanceQuerySource(user)).withDeployer(user)
    ) >> {
      getResult(runtime)() >>= {
        case Seq(RhoType.Number(balance)) => balance.pure[F]
        case Seq(RhoType.String(error)) =>
          BugFoundError(s"Balance query failed unexpectedly: $error").raiseError[F, Long]
        case other =>
          BugFoundError(s"Balance query returned unexpected result: $other").raiseError[F, Long]
      }
    }

  private def balanceQuerySource(user: ByteString, name: String = "__SCALA__"): String =
    s"""
       | new rl(`rho:registry:lookup`), revAddressOps(`rho:rev:address`), revVaultCh in {
       |   rl!(`rho:id:1o93uitkrjfubh43jt19owanuezhntag5wh74c6ur5feuotpi73q8z`, *revVaultCh)
       |   | for (@(_, RevVault) <- revVaultCh) {
       |     new vaultCh, revAddressCh in {
       |       revAddressOps!("fromPublicKey", "${Base16.encode(user.toByteArray)}".hexToBytes(), *revAddressCh)
       |       | for(@revAddress <- revAddressCh) {
       |         @RevVault!("findOrCreate", revAddress, *vaultCh)
       |         | for(@vaultEither <- vaultCh){
       |           match vaultEither {
       |             (true, vault) => {
       |               @vault!("balance", "$name")
       |             }
       |             (false, error) => {
       |               @"$name"!(error)
       |             }
       |           }
       |         }
       |       }
       |     }
       |   }
       | }
     """.stripMargin

  def computeDeployPayment(start: StateHash)(user: ByteString, amount: Long): F[StateHash] =
    withResetRuntime(start)(
      computeDeployPayment(_)(user, amount).map(cp => ByteString.copyFrom(cp.root.bytes.toArray))
    )

  private def computeDeployPayment(
      runtime: Runtime[F]
  )(user: ByteString, amount: Long): F[Checkpoint] =
    computeEffect(runtime)(
      ConstructDeploy.sourceDeployNow(deployPaymentSource(amount)).withDeployer(user)
    ) >>
      getResult(runtime)() >>= {
      case Seq(RhoType.Boolean(true)) =>
        runtime.space.createCheckpoint()
      case Seq(RhoType.String(error)) =>
        BugFoundError(s"Deploy payment failed unexpectedly: $error").raiseError[F, Checkpoint]
      case other =>
        BugFoundError(
          s"Deploy payment returned unexpected result: ${other.map(RholangPrinter().buildString(_))}"
        ).raiseError[F, Checkpoint]
    }

  private def deployPaymentSource(amount: Long, name: String = "__SCALA__"): String =
    s"""
       | new rl(`rho:registry:lookup`), poSCh in {
       |   rl!(`rho:id:cnec3pa8prp4out3yc8facon6grm3xbsotpd4ckjfx8ghuw77xadzt`, *poSCh) |
       |   for(@(_, PoS) <- poSCh) {
       |     @PoS!("pay", $amount, "$name")
       |   }
       | }
       """.stripMargin

  private def withRuntime[A](f: Runtime[F] => F[A]): F[A] =
    Sync[F].bracket(runtimeContainer.take)(f)(runtimeContainer.put)

  private def withResetRuntime[R](hash: StateHash)(block: Runtime[F] => F[R]) =
    withRuntime(
      runtime =>
        runtime.space.reset(Blake2b256Hash.fromByteArray(hash.toByteArray)) >> block(runtime)
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
    withResetRuntime(hash)(getData(_)(channel))

  private def getData(
      runtime: Runtime[F]
  )(channel: Par): F[Seq[Par]] =
    runtime.space.getData(channel).map(_.flatMap(_.a.pars))

  private def getResult(
      runtime: Runtime[F]
  )(name: String = "__SCALA__"): F[Seq[Par]] = {

    val channel                 = Par().withExprs(Seq(Expr(GString(name))))
    val pattern                 = BindPattern(Seq(EVar(FreeVar(0))), freeCount = 1)
    val cont                    = TaggedContinuation().withParBody(ParWithRandom(Par()))
    implicit val cost: _cost[F] = runtime.cost

    runtime.space.consume(Seq(channel), Seq(pattern), cont, persist = false)(matchListPar).map {
      case Some((_, dataList)) => dataList.flatMap(_.value.pars)
      case None                => Seq.empty[Par]
    }
  }

  def getContinuation(
      hash: StateHash
  )(channels: Seq[Par]): F[Seq[(Seq[BindPattern], Par)]] =
    withResetRuntime(hash)(
      _.space
        .getWaitingContinuations(channels)
        .map(
          _.filter(_.continuation.taggedCont.isParBody)
            .map(result => (result.patterns, result.continuation.taggedCont.parBody.get.body))
        )
    )

  private def newEval(
      terms: Seq[DeployData],
      runtime: Runtime[F],
      initHash: StateHash
  ): F[(StateHash, Seq[InternalProcessedDeploy])] = {

    type Acc = (Blake2b256Hash, Seq[InternalProcessedDeploy])

    def evalSingle(acc: Acc, deploy: DeployData): F[Acc] = {
      val (hash, deployResults) = acc
      for {
        _                            <- runtime.space.reset(hash)
        evaluateResult               <- computeEffect(runtime)(deploy)
        EvaluateResult(cost, errors) = evaluateResult
        newCheckpoint                <- runtime.space.createCheckpoint()
        deployResult = InternalProcessedDeploy(
          deploy,
          Cost.toProto(cost),
          newCheckpoint.log,
          Seq.empty[trace.Event],
          DeployStatus.fromErrors(errors)
        )
        newHash = if (errors.isEmpty) newCheckpoint.root else hash
      } yield (newHash, deployResults :+ deployResult)
    }

    val initHashBlake = Blake2b256Hash.fromByteArray(initHash.toByteArray)
    terms.toList
      .foldM[F, Acc]((initHashBlake, Seq.empty))(evalSingle)
      .map {
        case (hash, deployResults) =>
          val hashByteString = ByteString.copyFrom(hash.bytes.toArray)
          (hashByteString, deployResults)
      }
  }

  private def replayEval(
      terms: Seq[InternalProcessedDeploy],
      runtime: Runtime[F],
      initHash: StateHash
  ): F[Either[(Option[DeployData], Failed), StateHash]] = {

    def doReplayEval(
        terms: Seq[InternalProcessedDeploy],
        hash: Blake2b256Hash
    ): F[Either[(Option[DeployData], Failed), StateHash]] =
      Concurrent[F].defer {
        terms match {
          case processedDeploy +: rem =>
            import processedDeploy._
            for {
              replayEvaluateResult <- replayComputeEffect(runtime)(hash, processedDeploy)
              //TODO: compare replay deploy cost to given deploy cost
              EvaluateResult(cost, errors) = replayEvaluateResult
              cont <- DeployStatus.fromErrors(errors) match {
                       case int: InternalErrors => Left(Some(deploy) -> int).pure[F]
                       case replayStatus =>
                         if (status.isFailed != replayStatus.isFailed)
                           Left(Some(deploy) -> ReplayStatusMismatch(replayStatus, status)).pure[F]
                         else if (errors.nonEmpty) doReplayEval(rem, hash)
                         else {
                           runtime.replaySpace
                             .createCheckpoint()
                             .attempt
                             .flatMap {
                               case Right(newCheckpoint) =>
                                 doReplayEval(rem, newCheckpoint.root)
                               case Left(ex: ReplayException) =>
                                 Either
                                   .left[(Option[DeployData], Failed), StateHash](
                                     none[DeployData] -> UnusedCommEvent(ex)
                                   )
                                   .pure[F]
                               case Left(ex) =>
                                 Either
                                   .left[(Option[DeployData], Failed), StateHash](
                                     none[DeployData] -> UserErrors(Vector(ex))
                                   )
                                   .pure[F]
                             }
                         }
                     }
            } yield cont

          case _ =>
            Either
              .right[(Option[DeployData], Failed), StateHash](
                ByteString.copyFrom(hash.bytes.toArray)
              )
              .pure[F]
        }
      }

    doReplayEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray))
  }

  private[this] def doInj(
      deploy: DeployData,
      reducer: ChargingReducer[F],
      errorLog: ErrorLog[F]
  )(implicit C: _cost[F]) = {
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
      ): T[F, scala.Seq[Par]] = runtimeManager.captureResults(start, deploy, name).liftM[T]

      override def captureResults(
          start: StateHash,
          deploy: DeployData,
          name: Par
      ): T[F, scala.Seq[Par]] = runtimeManager.captureResults(start, deploy, name).liftM[T]

      override def replayComputeState(hash: StateHash)(
          terms: scala.Seq[InternalProcessedDeploy],
          blockTime: Long
      ): T[F, scala.Either[(Option[DeployData], Failed), StateHash]] =
        runtimeManager.replayComputeState(hash)(terms, blockTime).liftM[T]

      override def computeState(hash: StateHash)(
          terms: scala.Seq[DeployData],
          blockTime: Long
      ): T[F, (StateHash, scala.Seq[InternalProcessedDeploy])] =
        runtimeManager.computeState(hash)(terms, blockTime).liftM[T]

      def computeGenesis(
          terms: Seq[DeployData],
          blockTime: Long
      ): T[F, (StateHash, StateHash, Seq[InternalProcessedDeploy])] =
        runtimeManager.computeGenesis(terms, blockTime).liftM[T]

      override def storageRepr(hash: StateHash): T[F, Option[String]] =
        runtimeManager.storageRepr(hash).liftM[T]

      override def computeBonds(hash: StateHash): T[F, scala.Seq[Bond]] =
        runtimeManager.computeBonds(hash).liftM[T]

      override def computeBalance(start: StateHash)(user: ByteString): T[F, Long] =
        runtimeManager.computeBalance(start)(user).liftM[T]

      override def computeDeployPayment(
          start: StateHash
      )(user: ByteString, amount: Long): T[F, StateHash] =
        runtimeManager.computeDeployPayment(start)(user, amount).liftM[T]

      override def getData(hash: StateHash)(channel: Par): T[F, scala.Seq[Par]] =
        runtimeManager.getData(hash)(channel).liftM[T]

      override def getContinuation(
          hash: StateHash
      )(channels: Seq[Par]): T[F, scala.Seq[(scala.Seq[BindPattern], Par)]] =
        runtimeManager.getContinuation(hash)(channels).liftM[T]

      override val emptyStateHash: StateHash = runtimeManager.emptyStateHash
    }

  def eitherTRuntimeManager[E, F[_]: Monad](
      rm: RuntimeManager[F]
  ): RuntimeManager[EitherT[F, E, ?]] =
    RuntimeManager.forTrans[F, EitherT[?[_], E, ?]](rm)
}
