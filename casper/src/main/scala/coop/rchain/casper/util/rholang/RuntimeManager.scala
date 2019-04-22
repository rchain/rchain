package coop.rchain.casper.util.rholang

import cats._
import cats.data.EitherT
import cats.effect._
import cats.effect.concurrent.MVar
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Interpreter
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{
  accounting,
  ChargingReducer,
  ErrorLog,
  EvaluateResult,
  Runtime
}
import coop.rchain.rspace.internal.Datum
import coop.rchain.rspace.{Blake2b256Hash, ReplayException}

import scala.collection.immutable

trait RuntimeManager[F[_]] {
  def captureResults(start: StateHash, deploy: DeployData, name: String = "__SCALA__"): F[Seq[Par]]
  def captureResults(start: StateHash, deploy: DeployData, name: Par): F[Seq[Par]]
  def replayComputeState(
      hash: StateHash,
      terms: Seq[InternalProcessedDeploy],
      time: Option[Long] = None
  ): F[Either[(Option[DeployData], Failed), StateHash]]
  def computeState(
      hash: StateHash,
      terms: Seq[DeployData],
      time: Option[Long] = None
  ): F[(StateHash, Seq[InternalProcessedDeploy])]
  def storageRepr(hash: StateHash): F[Option[String]]
  def computeBonds(hash: StateHash): F[Seq[Bond]]
  def getData(hash: ByteString, channel: Par): F[Seq[Par]]
  def getContinuation(
      hash: ByteString,
      channels: immutable.Seq[Par]
  ): F[Seq[(Seq[BindPattern], Par)]]
  def emptyStateHash: ByteString
}

class RuntimeManagerImpl[F[_]: Concurrent : Sync] private[rholang] (
    val emptyStateHash: ByteString,
    runtimeContainer: MVar[F, Runtime[F]]
) extends RuntimeManager[F] {

  def captureResults(
      start: StateHash,
      deploy: DeployData,
      name: String = "__SCALA__"
  ): F[Seq[Par]] =
    captureResults(start, deploy, Par().withExprs(Seq(Expr(GString(name)))))

  def captureResults(start: StateHash, deploy: DeployData, name: Par): F[Seq[Par]] =
    Sync[F]
      .bracket(runtimeContainer.take) { runtime =>
        //TODO: Is better error handling needed here?
        for {
          evalR                     <- newEval(deploy :: Nil, runtime, start)
          (_, Seq(processedDeploy)) = evalR
          result <- if (processedDeploy.status.isFailed)
                      Sync[F].raiseError(new Exception("Evaluation error:" + processedDeploy.status))
                    else
                      runtime.space.getData(name).map(_.toSeq)
        } yield result.flatMap(_.a.pars)
      }(runtime => runtimeContainer.put(runtime))

  def replayComputeState(
      hash: StateHash,
      terms: Seq[InternalProcessedDeploy],
      time: Option[Long] = None
  ): F[Either[(Option[DeployData], Failed), StateHash]] =
    Sync[F].bracket(runtimeContainer.take) { runtime =>
      for {
        _      <- setTimestamp(time, runtime)
        result <- replayEval(terms, runtime, hash)
      } yield result
    }(runtime => runtimeContainer.put(runtime))

  def computeState(
      hash: StateHash,
      terms: Seq[DeployData],
      time: Option[Long] = None
  ): F[(StateHash, Seq[InternalProcessedDeploy])] =
    Sync[F].bracket(runtimeContainer.take) { runtime =>
      for {
        _      <- setTimestamp(time, runtime)
        result <- newEval(terms, runtime, hash)
      } yield result
    }(runtime => runtimeContainer.put(runtime))

  private def setTimestamp(
      time: Option[Long],
      runtime: Runtime[F]
  ): F[Unit] =
    time match {
      case Some(t) =>
        val timestamp: Par = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(t))))
        runtime.blockTime.setParams(timestamp)
      case None => ().pure[F]
    }

  def storageRepr(hash: StateHash): F[Option[String]] =
    Sync[F]
      .bracket(runtimeContainer.take) { runtime =>
        val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
        runtime.space.reset(blakeHash).map(_ => StoragePrinter.prettyPrint(runtime.space))
      }(runtime => runtimeContainer.put(runtime))
      .attempt
      .map {
        case Right(print) => Some(print)
        case Left(_)      => None
      }

  private def sourceDeploy(source: String, timestamp: Long, phlos: Long): DeployData =
    DeployData(
      deployer = ByteString.EMPTY,
      timestamp = timestamp,
      term = source,
      phloLimit = phlos
    )

  def computeBonds(hash: StateHash): F[Seq[Bond]] = {
    val bondsQuery =
      """new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
        |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
        |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
        |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
        |    for(pos <- posCh){ pos!("getBonds", "__SCALA__") }
        |  }
        |}""".stripMargin

    val bondsQueryTerm = sourceDeploy(bondsQuery, 0L, accounting.MAX_VALUE)
    captureResults(hash, bondsQueryTerm)
      .ensureOr(
        bondsPar =>
          new IllegalArgumentException(
            s"Incorrect number of results from query of current bonds: ${bondsPar.size}"
          )
      )(bondsPar => bondsPar.size == 1)
      .map { bondsPar =>
        toBondSeq(bondsPar.head)
      }
  }

  private def withResetRuntime[R](hash: StateHash, block: Runtime[F] => F[R]) =
    Sync[F].bracket(runtimeContainer.take) { runtime =>
      val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
      runtime.space.reset(blakeHash).flatMap(_ => block(runtime))
    }(runtime => runtimeContainer.put(runtime))

  private def toBondSeq(bondsMap: Par): Seq[Bond] =
    bondsMap.exprs.head.getEMapBody.ps.map {
      case (validator: Par, bond: Par) =>
        assert(validator.exprs.length == 1, "Validator in bonds map wasn't a single string.")
        assert(bond.exprs.length == 1, "Stake in bonds map wasn't a single integer.")
        val validatorName = validator.exprs.head.getGByteArray
        val stakeAmount   = bond.exprs.head.getETupleBody.ps.head.exprs.head.getGInt
        Bond(validatorName, stakeAmount)
    }.toList

  def getData(hash: ByteString, channel: Par): F[Seq[Par]] =
    withResetRuntime(hash, runtime => {
      runtime.space.getData(channel).map(_.flatMap(_.a.pars))
    })

  def getContinuation(
      hash: ByteString,
      channels: immutable.Seq[Par]
  ): F[Seq[(Seq[BindPattern], Par)]] =
    withResetRuntime(
      hash,
      runtime => {
        runtime.space
          .getWaitingContinuations(channels)
          .map(
            results =>
              for {
                result <- results.filter(_.continuation.taggedCont.isParBody)
              } yield (result.patterns, result.continuation.taggedCont.parBody.get.body)
          )
      }
    )

  private def newEval(
      terms: Seq[DeployData],
      runtime: Runtime[F],
      initHash: StateHash
  ): F[(StateHash, Seq[InternalProcessedDeploy])] = {

    def doEval(
        terms: Seq[DeployData],
        hash: Blake2b256Hash,
        acc: Seq[InternalProcessedDeploy]
    ): F[(StateHash, Seq[InternalProcessedDeploy])] =
      Concurrent[F].defer {
        terms match {
          case deploy +: rem =>
            for {
              _ <- runtime.space.reset(hash)
              (codeHash, phloPrice, userId, timestamp) = ProtoUtil.getRholangDeployParams(
                deploy
              )
              _                                      <- runtime.shortLeashParams.setParams(codeHash, phloPrice, userId, timestamp)
              injResult                              <- doInj(deploy, runtime.reducer, runtime.errorLog)(runtime.cost)
              EvaluateResult(evaluationCost, errors) = injResult
              newCheckpoint                          <- runtime.space.createCheckpoint()
              deployResult = InternalProcessedDeploy(
                deploy,
                Cost.toProto(evaluationCost),
                newCheckpoint.log,
                DeployStatus.fromErrors(errors)
              )
              cont <- if (errors.isEmpty)
                       doEval(rem, newCheckpoint.root, acc :+ deployResult)
                     else doEval(rem, hash, acc :+ deployResult)
            } yield cont

          case _ => (ByteString.copyFrom(hash.bytes.toArray), acc).pure[F]
        }
      }

    doEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray), Vector.empty)
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
          case InternalProcessedDeploy(deploy, _, log, status) +: rem =>
            val (codeHash, phloPrice, userId, timestamp) = ProtoUtil.getRholangDeployParams(
              deploy
            )
            for {
              _         <- runtime.shortLeashParams.setParams(codeHash, phloPrice, userId, timestamp)
              _         <- runtime.replaySpace.rig(hash, log.toList)
              injResult <- doInj(deploy, runtime.replayReducer, runtime.errorLog)(runtime.cost)
              //TODO: compare replay deploy cost to given deploy cost
              EvaluateResult(cost, errors) = injResult
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
    import coop.rchain.catscontrib.mtl.implicits._
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
          start: RuntimeManager.StateHash,
          deploy: DeployData,
          name: String
      ): T[F, scala.Seq[Par]] = runtimeManager.captureResults(start, deploy, name).liftM[T]

      override def captureResults(
          start: RuntimeManager.StateHash,
          deploy: DeployData,
          name: Par
      ): T[F, scala.Seq[Par]] = runtimeManager.captureResults(start, deploy, name).liftM[T]

      override def replayComputeState(
          hash: RuntimeManager.StateHash,
          terms: scala.Seq[InternalProcessedDeploy],
          time: Option[Long]
      ): T[F, scala.Either[(Option[DeployData], Failed), RuntimeManager.StateHash]] =
        runtimeManager.replayComputeState(hash, terms, time).liftM[T]

      override def computeState(
          hash: RuntimeManager.StateHash,
          terms: scala.Seq[DeployData],
          time: Option[Long]
      ): T[F, (RuntimeManager.StateHash, scala.Seq[InternalProcessedDeploy])] =
        runtimeManager.computeState(hash, terms, time).liftM[T]

      override def storageRepr(
          hash: RuntimeManager.StateHash
      ): T[F, Option[String]] = runtimeManager.storageRepr(hash).liftM[T]

      override def computeBonds(
          hash: RuntimeManager.StateHash
      ): T[F, scala.Seq[Bond]] = runtimeManager.computeBonds(hash).liftM[T]

      override def getData(
          hash: ByteString,
          channel: Par
      ): T[F, scala.Seq[Par]] = runtimeManager.getData(hash, channel).liftM[T]

      override def getContinuation(
          hash: ByteString,
          channels: scala.collection.immutable.Seq[Par]
      ): T[F, scala.Seq[(scala.Seq[BindPattern], Par)]] =
        runtimeManager.getContinuation(hash, channels).liftM[T]

      override val emptyStateHash: ByteString = runtimeManager.emptyStateHash
    }

  def eitherTRuntimeManager[E, F[_]: Monad](
      rm: RuntimeManager[F]
  ): RuntimeManager[EitherT[F, E, ?]] =
    RuntimeManager.forTrans[F, EitherT[?[_], E, ?]](rm)
}
