package coop.rchain.casper.util.rholang

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models._
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{accounting, ChargingReducer, ErrorLog, Runtime}
import coop.rchain.rspace.internal.Datum
import coop.rchain.rspace.{Blake2b256Hash, ReplayException}

import scala.collection.immutable
import scala.concurrent.SyncVar
import scala.util.{Failure, Success, Try}
import scala.language.higherKinds

//runtime is a SyncVar for thread-safety, as all checkpoints share the same "hot store"
class RuntimeManager[F[_]: Sync] private (
    val emptyStateHash: ByteString,
    runtimeContainer: SyncVar[Runtime[F]]
) {

  def captureResults(start: StateHash, deploy: Deploy, name: String = "__SCALA__"): F[Seq[Par]] =
    for {
      runtime    <- Sync[F].delay { runtimeContainer.take() }
      evalResult <- newEval(deploy :: Nil, runtime, start)

      (_, Seq(processedDeploy)) = evalResult

      //TODO: Is better error handling needed here?
      _ <- if (processedDeploy.status.isFailed)
            Sync[F].raiseError(new Exception("Processed deploy has failed"))
          else
            Sync[F].pure(())

      returnChannel = Par().copy(exprs = Seq(Expr(GString(name))))
      result        <- runtime.space.getData(returnChannel)

      _ <- Sync[F].delay { runtimeContainer.put(runtime) }
    } yield result.flatMap(_.a.pars)

  def replayComputeState(
      hash: StateHash,
      terms: Seq[InternalProcessedDeploy],
      time: Option[Long] = None
  ): F[Either[(Option[Deploy], Failed), StateHash]] =
    for {
      runtime <- Sync[F].delay { runtimeContainer.take() }
      _       <- setTimestamp(time, runtime)
      result  <- replayEval(terms, runtime, hash)
      _       <- Sync[F].delay { runtimeContainer.put(runtime) }
    } yield result

  def computeState(
      hash: StateHash,
      terms: Seq[Deploy],
      time: Option[Long] = None
  ): F[(StateHash, Seq[InternalProcessedDeploy])] =
    for {
      runtime <- Sync[F].delay { runtimeContainer.take() }
      _       <- setTimestamp(time, runtime)
      result  <- newEval(terms, runtime, hash)
      _       <- Sync[F].delay { runtimeContainer.put(runtime) }
    } yield result

  private def setTimestamp(time: Option[Long], runtime: Runtime[F]): F[Unit] =
    time match {
      case Some(t) =>
        val timestamp: Par = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(t))))
        runtime.blockTime.setParams(timestamp)
      case None => Sync[F].pure(())
    }

  def storageRepr(hash: StateHash): Option[String] =
    getResetRuntimeOpt(hash).map { resetRuntime =>
      val result = StoragePrinter.prettyPrint(resetRuntime.space.store)
      runtimeContainer.put(resetRuntime)
      result
    }

  def computeBonds(hash: StateHash): F[Seq[Bond]] = {
    val bondsQuery =
      """new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
        |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
        |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
        |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
        |    for(pos <- posCh){ pos!("getBonds", "__SCALA__") }
        |  }
        |}""".stripMargin

    val bondsQueryTerm =
      ProtoUtil.deployDataToDeploy(ProtoUtil.sourceDeploy(bondsQuery, 0L, accounting.MAX_VALUE))

    captureResults(hash, bondsQueryTerm)
      .map { bondsPar =>
        assert(
          bondsPar.size == 1,
          s"Incorrect number of results from query of current bonds: ${bondsPar.size}"
        )
        toBondSeq(bondsPar.head)
      }
  }

  private def getResetRuntime(hash: StateHash) = {
    val runtime   = runtimeContainer.take()
    val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
    Try(runtime.space.reset(blakeHash)) match {
      case Success(_) => runtime
      case Failure(ex) =>
        runtimeContainer.put(runtime)
        throw ex
    }
  }

  private def getResetRuntimeOpt(hash: StateHash) = {
    val runtime   = runtimeContainer.take()
    val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
    Try(runtime.space.reset(blakeHash)) match {
      case Success(_) => Some(runtime)
      case Failure(_) =>
        runtimeContainer.put(runtime)
        None
    }
  }

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
    for {
      resetRuntime <- Sync[F].delay { getResetRuntime(hash) }
      data         <- resetRuntime.space.getData(channel)
      _            <- Sync[F].delay { runtimeContainer.put(resetRuntime) }
    } yield data.flatMap(_.a.pars)

  def getContinuation(
      hash: ByteString,
      channels: immutable.Seq[Par]
  ): F[Seq[(Seq[BindPattern], Par)]] =
    for {
      resetRuntime  <- Sync[F].delay { getResetRuntime(hash) }
      continuations <- resetRuntime.space.getWaitingContinuations(channels)
      _             <- Sync[F].delay { runtimeContainer.put(resetRuntime) }
    } yield
      continuations
        .filter(_.continuation.taggedCont.isParBody)
        .map { result =>
          (result.patterns, result.continuation.taggedCont.parBody.get.body)
        }

  private def newEval(
      terms: Seq[Deploy],
      runtime: Runtime[F],
      initHash: StateHash
  ): F[(StateHash, Seq[InternalProcessedDeploy])] = {

    def doEval(
        terms: Seq[Deploy],
        hash: Blake2b256Hash,
        acc: Vector[InternalProcessedDeploy]
    ): F[(StateHash, Seq[InternalProcessedDeploy])] =
      terms match {
        case deploy +: rem =>
          for {
            _              <- runtime.space.reset(hash)
            availablePhlos = Cost(deploy.raw.map(_.phloLimit).get.value)
            _              <- runtime.reducer.setAvailablePhlos(availablePhlos)
            (codeHash, phloPrice, userId, timestamp) = ProtoUtil.getRholangDeployParams(
              deploy.raw.get
            )
            _                   <- runtime.shortLeashParams.setParams(codeHash, phloPrice, userId, timestamp)
            injResult           <- injAttempt(deploy, runtime.reducer, runtime.errorLog)
            (phlosLeft, errors) = injResult
            cost                = phlosLeft.copy(cost = availablePhlos.value - phlosLeft.cost)
            newCheckpoint       <- runtime.space.createCheckpoint()
            deployResult = InternalProcessedDeploy(
              deploy,
              cost,
              newCheckpoint.log,
              DeployStatus.fromErrors(errors)
            )
            cont <- if (errors.isEmpty)
                     doEval(rem, newCheckpoint.root, acc :+ deployResult)
                   else
                     doEval(rem, hash, acc :+ deployResult)
          } yield cont

        case _ => Sync[F].delay { (ByteString.copyFrom(hash.bytes.toArray), acc) }
      }

    doEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray), Vector.empty)
  }

  private def replayEval(
      terms: Seq[InternalProcessedDeploy],
      runtime: Runtime[F],
      initHash: StateHash
  ): F[Either[(Option[Deploy], Failed), StateHash]] = {

    def doReplayEval(
        terms: Seq[InternalProcessedDeploy],
        hash: Blake2b256Hash
    ): F[Either[(Option[Deploy], Failed), StateHash]] =
      terms match {
        case InternalProcessedDeploy(deploy, _, log, status) +: rem =>
          val availablePhlos = Cost(deploy.raw.map(_.phloLimit).get.value)
          for {
            _           <- runtime.replayReducer.setAvailablePhlos(availablePhlos)
            _           <- runtime.replaySpace.rig(hash, log.toList)
            injResult   <- injAttempt(deploy, runtime.replayReducer, runtime.errorLog)
            (_, errors) = injResult
            //TODO: compare replay deploy cost to given deploy cost
//              cost                = phlosLeft.copy(cost = availablePhlos.value - phlosLeft.cost)
            cont <- DeployStatus.fromErrors(errors) match {
                     case int: InternalErrors => Sync[F].delay { Left(Some(deploy) -> int) }
                     case replayStatus =>
                       if (status.isFailed != replayStatus.isFailed)
                         Sync[F].delay {
                           Left(Some(deploy) -> ReplayStatusMismatch(replayStatus, status))
                         } else if (errors.nonEmpty)
                         doReplayEval(rem, hash)
                       else {
                         runtime.replaySpace
                           .createCheckpoint()
                           .attempt
                           .flatMap {
                             case Right(newCheckpoint) =>
                               doReplayEval(rem, newCheckpoint.root)

                             case Left(ex: ReplayException) =>
                               val err: Either[(Option[Deploy], Failed), StateHash] =
                                 Left(none[Deploy] -> UnusedCommEvent(ex))
                               Sync[F].delay(err)
                           }
                       }
                   }
          } yield cont

        case _ => Sync[F].delay { Right(ByteString.copyFrom(hash.bytes.toArray)) }
      }

    doReplayEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray))
  }

  private def injAttempt(
      deploy: Deploy,
      reducer: ChargingReducer[F],
      errorLog: ErrorLog[F]
  ): F[(PCost, Vector[Throwable])] = {
    implicit val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy.raw.get))
    )
    for {
      result    <- reducer.inj(deploy.term.get).attempt
      oldErrors <- errorLog.readAndClearErrorVector()
      newErrors = result.swap.toSeq.toVector
      allErrors = oldErrors |+| newErrors

      phlos <- reducer.getAvailablePhlos().map(phlos => CostAccount.toProto(phlos) -> allErrors)
    } yield phlos
  }
}

object RuntimeManager {
  type StateHash = ByteString

  def fromRuntime[F[_]: Sync](active: Runtime[F]): F[RuntimeManager[F]] =
    for {
      _                <- active.space.clear()
      _                <- active.replaySpace.clear()
      _                <- Runtime.injectEmptyRegistryRoot(active.space, active.replaySpace)
      checkpoint       <- active.space.createCheckpoint()
      replayCheckpoint <- active.replaySpace.createCheckpoint()
      hash             = ByteString.copyFrom(checkpoint.root.bytes.toArray)
      replayHash       = ByteString.copyFrom(replayCheckpoint.root.bytes.toArray)
      _                = assert(hash == replayHash)
      runtime          = new SyncVar[Runtime[F]]()
      _                = runtime.put(active)
    } yield new RuntimeManager(hash, runtime)
}
