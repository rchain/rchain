package coop.rchain.casper.util.rholang

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.{EMapBody, GString}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Registry
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{accounting, ChargingReducer, ErrorLog, Runtime}
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.rspace.trace.Produce
import coop.rchain.rspace.{Blake2b256Hash, ReplayException}
import monix.eval.Task
import monix.execution.Scheduler

import scala.annotation.tailrec
import scala.collection.immutable
import scala.concurrent.SyncVar
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

//runtime is a SyncVar for thread-safety, as all checkpoints share the same "hot store"
class RuntimeManager private (val emptyStateHash: ByteString, runtimeContainer: SyncVar[Runtime]) {

  def captureResults(start: StateHash, term: Par, name: String = "__SCALA__")(
      implicit scheduler: Scheduler
  ): Seq[Par] = {
    val runtime                   = runtimeContainer.take()
    val deploy                    = ProtoUtil.termDeploy(term, System.currentTimeMillis(), accounting.MAX_VALUE)
    val (_, Seq(processedDeploy)) = newEval(deploy :: Nil, runtime, start)

    //TODO: Is better error handling needed here?
    val result: Seq[Datum[ListParWithRandom]] =
      if (processedDeploy.status.isFailed) Nil
      else {
        val returnChannel = Par().copy(exprs = Seq(Expr(GString(name))))
        runtime.space.getData(returnChannel)
      }

    runtimeContainer.put(runtime)

    result.flatMap(_.a.pars)
  }

  def replayComputeState(hash: StateHash, terms: Seq[InternalProcessedDeploy])(
      implicit scheduler: Scheduler
  ): Either[(Option[Deploy], Failed), StateHash] = {
    val runtime = runtimeContainer.take()
    val result  = replayEval(terms, runtime, hash)
    runtimeContainer.put(runtime)
    result
  }

  def computeState(hash: StateHash, terms: Seq[Deploy])(
      implicit scheduler: Scheduler
  ): (StateHash, Seq[InternalProcessedDeploy]) = {
    val runtime = runtimeContainer.take()
    val result  = newEval(terms, runtime, hash)
    runtimeContainer.put(runtime)
    result
  }

  def storageRepr(hash: StateHash): String = {
    val resetRuntime = getResetRuntime(hash)
    val result       = StoragePrinter.prettyPrint(resetRuntime.space.store)
    runtimeContainer.put(resetRuntime)
    result
  }

  def computeBonds(hash: StateHash)(implicit scheduler: Scheduler): Seq[Bond] =
    Try(unsafeComputeBonds(hash)) match {
      case Success(value) => value
      case Failure(ex) =>
        ex.printStackTrace()
        throw ex
    }

  private def unsafeComputeBonds(hash: StateHash)(implicit scheduler: Scheduler): Seq[Bond] = {
    // TODO: Switch to a read only name
    val bondsQuery =
      """for(@pos <- @"proofOfStake"){ @(pos, "getBonds")!("__SCALA__") }"""
    //TODO: construct directly instead of parsing rholang source
    val bondsQueryTerm = InterpreterUtil.mkTerm(bondsQuery).right.get
    val bondsPar       = captureResults(hash, bondsQueryTerm)
    assert(
      bondsPar.size == 1,
      s"Incorrect number of results from query of current bonds: ${bondsPar.size}"
    )
    toBondSeq(bondsPar.head)
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

  private def toBondSeq(bondsMap: Par): Seq[Bond] =
    bondsMap.exprs.head.getEMapBody.ps.map {
      case (validator: Par, bond: Par) =>
        assert(validator.exprs.length == 1, "Validator in bonds map wasn't a single string.")
        assert(bond.exprs.length == 1, "Stake in bonds map wasn't a single integer.")
        val validatorName = validator.exprs.head.getGByteArray
        val stakeAmount   = bond.exprs.head.getETupleBody.ps.head.exprs.head.getGInt
        Bond(validatorName, stakeAmount)
    }.toList

  def getData(hash: ByteString, channel: Par): Seq[Par] = {
    val resetRuntime                          = getResetRuntime(hash)
    val result: Seq[Datum[ListParWithRandom]] = resetRuntime.space.getData(channel)
    runtimeContainer.put(resetRuntime)
    result.flatMap(_.a.pars)
  }

  def getContinuation(
      hash: ByteString,
      channels: immutable.Seq[Par]
  ): Seq[(Seq[BindPattern], Par)] = {
    val resetRuntime = getResetRuntime(hash)
    val results: Seq[WaitingContinuation[BindPattern, TaggedContinuation]] =
      resetRuntime.space.getWaitingContinuations(channels)
    runtimeContainer.put(resetRuntime)
    for {
      result <- results.filter(_.continuation.taggedCont.isParBody)
    } yield (result.patterns, result.continuation.taggedCont.parBody.get.body)
  }

  private def newEval(terms: Seq[Deploy], runtime: Runtime, initHash: StateHash)(
      implicit scheduler: Scheduler
  ): (StateHash, Seq[InternalProcessedDeploy]) = {

    @tailrec
    def doEval(
        terms: Seq[Deploy],
        hash: Blake2b256Hash,
        acc: Vector[InternalProcessedDeploy]
    ): (StateHash, Vector[InternalProcessedDeploy]) =
      terms match {
        case deploy +: rem =>
          runtime.space.reset(hash)
          val availablePhlos = Cost(deploy.raw.flatMap(_.phloLimit).get.value)
          runtime.reducer.setAvailablePhlos(availablePhlos).runSyncUnsafe(1.second)
          val (phlosLeft, errors) = injAttempt(deploy, runtime.reducer, runtime.errorLog)
          val cost                = phlosLeft.copy(cost = availablePhlos.value - phlosLeft.cost)
          val newCheckpoint       = runtime.space.createCheckpoint()
          val deployResult = InternalProcessedDeploy(
            deploy,
            cost,
            newCheckpoint.log,
            DeployStatus.fromErrors(errors)
          )
          if (errors.isEmpty) doEval(rem, newCheckpoint.root, acc :+ deployResult)
          else doEval(rem, hash, acc :+ deployResult)

        case _ => (ByteString.copyFrom(hash.bytes.toArray), acc)
      }

    doEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray), Vector.empty)
  }

  private def replayEval(
      terms: Seq[InternalProcessedDeploy],
      runtime: Runtime,
      initHash: StateHash
  )(implicit scheduler: Scheduler): Either[(Option[Deploy], Failed), StateHash] = {

    def doReplayEval(
        terms: Seq[InternalProcessedDeploy],
        hash: Blake2b256Hash
    ): Either[(Option[Deploy], Failed), StateHash] =
      terms match {
        case InternalProcessedDeploy(deploy, _, log, status) +: rem =>
          val availablePhlos = Cost(deploy.raw.flatMap(_.phloLimit).get.value)
          runtime.replayReducer.setAvailablePhlos(availablePhlos).runSyncUnsafe(1.second)
          runtime.replaySpace.rig(hash, log.toList)
          //TODO: compare replay deploy cost to given deploy cost
          val (phlosLeft, errors) = injAttempt(deploy, runtime.replayReducer, runtime.errorLog)
          val cost                = phlosLeft.copy(cost = availablePhlos.value - phlosLeft.cost)
          DeployStatus.fromErrors(errors) match {
            case int: InternalErrors => Left(Some(deploy) -> int)
            case replayStatus =>
              if (status.isFailed != replayStatus.isFailed)
                Left(Some(deploy) -> ReplayStatusMismatch(replayStatus, status))
              else if (errors.nonEmpty) doReplayEval(rem, hash)
              else {
                Try(runtime.replaySpace.createCheckpoint()) match {
                  case Success(newCheckpoint) =>
                    doReplayEval(rem, newCheckpoint.root)
                  case Failure(ex: ReplayException) =>
                    Left(none[Deploy] -> UnusedCommEvent(ex))
                  case Failure(ex) =>
                    Left(none[Deploy] -> InternalErrors(Vector(ex)))
                }
              }
          }

        case _ => Right(ByteString.copyFrom(hash.bytes.toArray))
      }

    doReplayEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray))
  }

  private def injAttempt(deploy: Deploy, reducer: ChargingReducer[Task], errorLog: ErrorLog)(
      implicit scheduler: Scheduler
  ): (PCost, Vector[Throwable]) = {
    implicit val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy.raw.get))
    )
    Try(reducer.inj(deploy.term.get).unsafeRunSync) match {
      case Success(_) =>
        val errors = errorLog.readAndClearErrorVector()
        val cost   = CostAccount.toProto(reducer.getAvailablePhlos().unsafeRunSync)
        cost -> errors

      case Failure(ex) =>
        val otherErrors = errorLog.readAndClearErrorVector()
        val errors      = otherErrors :+ ex
        val cost        = CostAccount.toProto(reducer.getAvailablePhlos().unsafeRunSync)
        cost -> errors
    }
  }
}

object RuntimeManager {
  type StateHash = ByteString

  def fromRuntime(active: Runtime)(implicit scheduler: Scheduler): RuntimeManager = {
    active.space.clear()
    active.replaySpace.clear()

    //populate registry root with empty map
    val emptyMap     = Par().withExprs(List(Expr(EMapBody(ParMap(Nil)))))
    val registryRoot = Par().withIds(List(Registry.registryRoot))
    val produceTerm  = Par().withSends(List(Send(registryRoot, List(emptyMap))))
    implicit val rand = Blake2b512Random(
      Array[Byte](3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3)
    )
    val max       = Cost(Long.MaxValue)
    val zero      = Cost(0L)
    val emptyChPt = active.space.createCheckpoint()
    val registryInit: Task[Array[Byte]] = for {
      _          <- active.reducer.setAvailablePhlos(max)
      _          <- active.reducer.inj(produceTerm)
      checkpoint = active.space.createCheckpoint()
      hash       = checkpoint.root.bytes.toArray
      _          <- active.reducer.setAvailablePhlos(zero)
      _          <- active.replayReducer.setAvailablePhlos(max)
      _          = active.replaySpace.rig(emptyChPt.root, checkpoint.log)
      _          <- active.replayReducer.inj(produceTerm)
      replayHash = active.replaySpace.createCheckpoint().root.bytes.toArray
      _          = assert(hash.zip(replayHash).forall { case (a, b) => a == b })
    } yield hash

    val hash    = registryInit.unsafeRunSync
    val runtime = new SyncVar[Runtime]()
    runtime.put(active)

    new RuntimeManager(ByteString.copyFrom(hash), runtime)
  }
}
