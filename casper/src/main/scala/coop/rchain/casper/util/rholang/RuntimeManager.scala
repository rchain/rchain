package coop.rchain.casper.util.rholang

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models._
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{accounting, ChargingReducer, ErrorLog, Runtime}
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.rspace.trace.Produce
import coop.rchain.rspace.{Blake2b256Hash, ReplayException}
import monix.eval.Task
import monix.execution.Scheduler

import coop.rchain.catscontrib.TaskContrib._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.concurrent.SyncVar
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

//runtime is a SyncVar for thread-safety, as all checkpoints share the same "hot store"
class RuntimeManager private (val emptyStateHash: ByteString, runtimeContainer: SyncVar[Runtime]) {

  def captureResults(start: StateHash, deploy: Deploy, name: String = "__SCALA__")(
      implicit scheduler: Scheduler
  ): Seq[Par] = captureResults(start, deploy, Par().withExprs(Seq(Expr(GString(name)))))

  def captureResults(start: StateHash, deploy: Deploy, name: Par)(
      implicit scheduler: Scheduler
  ): Seq[Par] = {
    val runtime                   = runtimeContainer.take()
    val (_, Seq(processedDeploy)) = newEval(deploy :: Nil, runtime, start).unsafeRunSync

    //TODO: Is better error handling needed here?
    val result: Seq[Datum[ListParWithRandom]] =
      if (processedDeploy.status.isFailed) Nil
      else runtime.space.getData(name).unsafeRunSync

    runtimeContainer.put(runtime)

    result.flatMap(_.a.pars)
  }

  def replayComputeState(
      hash: StateHash,
      terms: Seq[InternalProcessedDeploy],
      time: Option[Long] = None
  ): Task[Either[(Option[Deploy], Failed), StateHash]] =
    for {
      runtime <- Task.delay(runtimeContainer.take())
      _       <- setTimestamp(time, runtime)
      result  <- replayEval(terms, runtime, hash)
      _       <- Task.delay(runtimeContainer.put(runtime))
    } yield result

  def computeState(
      hash: StateHash,
      terms: Seq[Deploy],
      time: Option[Long] = None
  ): Task[(StateHash, Seq[InternalProcessedDeploy])] =
    for {
      runtime <- Task.delay(runtimeContainer.take())
      _       <- setTimestamp(time, runtime)
      result  <- newEval(terms, runtime, hash)
      _       <- Task.delay(runtimeContainer.put(runtime))
    } yield result

  private def setTimestamp(time: Option[Long], runtime: Runtime): Task[Unit] =
    time match {
      case Some(t) =>
        val timestamp: Par = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(t))))
        runtime.blockTime.setParams(timestamp)
      case None => Task.unit
    }

  def storageRepr(hash: StateHash)(
      implicit scheduler: Scheduler
  ): Option[String] =
    getResetRuntimeOpt(hash).map { resetRuntime =>
      val result = StoragePrinter.prettyPrint(resetRuntime.space.store)
      runtimeContainer.put(resetRuntime)
      result
    }

  def computeBonds(hash: StateHash)(implicit scheduler: Scheduler): Seq[Bond] = {
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
    val bondsPar = captureResults(hash, bondsQueryTerm)

    assert(
      bondsPar.size == 1,
      s"Incorrect number of results from query of current bonds: ${bondsPar.size}"
    )
    toBondSeq(bondsPar.head)
  }

  private def getResetRuntime(hash: StateHash)(
      implicit scheduler: Scheduler
  ) = {
    val runtime   = runtimeContainer.take()
    val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
    Try(runtime.space.reset(blakeHash).unsafeRunSync) match {
      case Success(_) => runtime
      case Failure(ex) =>
        runtimeContainer.put(runtime)
        throw ex
    }
  }

  private def getResetRuntimeOpt(hash: StateHash)(
      implicit scheduler: Scheduler
  ) = {
    val runtime   = runtimeContainer.take()
    val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
    Try(runtime.space.reset(blakeHash).unsafeRunSync) match {
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

  def getData(hash: ByteString, channel: Par)(
      implicit scheduler: Scheduler
  ): Seq[Par] = {
    val resetRuntime                          = getResetRuntime(hash)
    val result: Seq[Datum[ListParWithRandom]] = resetRuntime.space.getData(channel).unsafeRunSync
    runtimeContainer.put(resetRuntime)
    result.flatMap(_.a.pars)
  }

  def getContinuation(
      hash: ByteString,
      channels: immutable.Seq[Par]
  )(
      implicit scheduler: Scheduler
  ): Seq[(Seq[BindPattern], Par)] = {
    val resetRuntime = getResetRuntime(hash)
    val results: Seq[WaitingContinuation[BindPattern, TaggedContinuation]] =
      resetRuntime.space.getWaitingContinuations(channels).unsafeRunSync
    runtimeContainer.put(resetRuntime)
    for {
      result <- results.filter(_.continuation.taggedCont.isParBody)
    } yield (result.patterns, result.continuation.taggedCont.parBody.get.body)
  }

  private def newEval(
      terms: Seq[Deploy],
      runtime: Runtime,
      initHash: StateHash
  ): Task[(StateHash, Seq[InternalProcessedDeploy])] = {

    def doEval(
        terms: Seq[Deploy],
        hash: Blake2b256Hash,
        acc: Vector[InternalProcessedDeploy]
    ): Task[(StateHash, Vector[InternalProcessedDeploy])] =
      Task.defer {
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
                     else doEval(rem, hash, acc :+ deployResult)
            } yield cont

          case _ => Task.now { (ByteString.copyFrom(hash.bytes.toArray), acc) }
        }
      }

    doEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray), Vector.empty)
  }

  private def replayEval(
      terms: Seq[InternalProcessedDeploy],
      runtime: Runtime,
      initHash: StateHash
  ): Task[Either[(Option[Deploy], Failed), StateHash]] = {

    def doReplayEval(
        terms: Seq[InternalProcessedDeploy],
        hash: Blake2b256Hash
    ): Task[Either[(Option[Deploy], Failed), StateHash]] =
      Task.defer {
        terms match {
          case InternalProcessedDeploy(deploy, _, log, status) +: rem =>
            val availablePhlos = Cost(deploy.raw.map(_.phloLimit).get.value)
            for {
              _ <- runtime.replayReducer.setAvailablePhlos(availablePhlos)
              (codeHash, phloPrice, userId, timestamp) = ProtoUtil.getRholangDeployParams(
                deploy.raw.get
              )
              _         <- runtime.shortLeashParams.setParams(codeHash, phloPrice, userId, timestamp)
              _         <- runtime.replaySpace.rig(hash, log.toList)
              injResult <- injAttempt(deploy, runtime.replayReducer, runtime.errorLog)
              //TODO: compare replay deploy cost to given deploy cost
              (phlosLeft, errors) = injResult
              cost                = phlosLeft.copy(cost = availablePhlos.value - phlosLeft.cost)
              cont <- DeployStatus.fromErrors(errors) match {
                       case int: InternalErrors => Task.now(Left(Some(deploy) -> int))
                       case replayStatus =>
                         if (status.isFailed != replayStatus.isFailed)
                           Task.now(
                             Left(Some(deploy) -> ReplayStatusMismatch(replayStatus, status))
                           )
                         else if (errors.nonEmpty) doReplayEval(rem, hash)
                         else {
                           runtime.replaySpace.createCheckpoint().attempt.flatMap {
                             case Right(newCheckpoint) =>
                               doReplayEval(rem, newCheckpoint.root)
                             case Left(ex: ReplayException) =>
                               Task.now(Left(none[Deploy] -> UnusedCommEvent(ex)))
                           }
                         }
                     }
            } yield cont

          case _ => Task.now(Right(ByteString.copyFrom(hash.bytes.toArray)))
        }
      }

    doReplayEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray))
  }

  private def injAttempt(
      deploy: Deploy,
      reducer: ChargingReducer[Task],
      errorLog: ErrorLog
  ): Task[(PCost, Vector[Throwable])] = {
    implicit val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy.raw.get))
    )
    reducer
      .inj(deploy.term.get)
      .attempt
      .flatMap(result => {
        val oldErrors = errorLog.readAndClearErrorVector()
        val newErrors = result.swap.toSeq.toVector
        val allErrors = oldErrors |+| newErrors

        reducer.getAvailablePhlos().map(phlos => CostAccount.toProto(phlos) -> allErrors)
      })
  }
}

object RuntimeManager {
  type StateHash = ByteString

  def fromRuntime(active: Runtime)(implicit scheduler: Scheduler): RuntimeManager =
    (for {
      _                <- active.space.clear()
      _                <- active.replaySpace.clear()
      _                <- Runtime.injectEmptyRegistryRoot(active.space, active.replaySpace)
      checkpoint       <- active.space.createCheckpoint()
      replayCheckpoint <- active.replaySpace.createCheckpoint()
      hash             = ByteString.copyFrom(checkpoint.root.bytes.toArray)
      replayHash       = ByteString.copyFrom(replayCheckpoint.root.bytes.toArray)
      _                = assert(hash == replayHash)
      runtime          = new SyncVar[Runtime]()
      _                = runtime.put(active)
    } yield (new RuntimeManager(hash, runtime))).unsafeRunSync
}
