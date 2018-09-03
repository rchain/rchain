package coop.rchain.casper.util.rholang

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.PrettyPrinter.buildString
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models._
import coop.rchain.rholang.interpreter.{ErrorLog, Reduce, Runtime}
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rspace.{trace, Blake2b256Hash, Checkpoint, ReplayException}
import monix.execution.Scheduler
import coop.rchain.rspace.internal.WaitingContinuation

import scala.concurrent.SyncVar
import scala.util.{Failure, Success, Try}
import RuntimeManager.StateHash
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rspace.internal.Datum
import coop.rchain.rspace.trace.Produce
import monix.eval.Task

import scala.collection.immutable

//runtime is a SyncVar for thread-safety, as all checkpoints share the same "hot store"
class RuntimeManager private (val emptyStateHash: ByteString, runtimeContainer: SyncVar[Runtime]) {

  def captureResults(start: StateHash, term: Par, name: String = "__SCALA__")(
      implicit scheduler: Scheduler): Seq[Par] = {
    val runtime                   = runtimeContainer.take()
    val deploy                    = ProtoUtil.termDeploy(term, System.currentTimeMillis())
    val (_, Seq(processedDeploy)) = newEval(deploy :: Nil, runtime, start)

    //TODO: Is better error handling needed here?
    val result: Seq[Datum[ListChannelWithRandom]] =
      if (processedDeploy.status.isFailed) Nil
      else {
        val returnChannel = Channel(Quote(Par().copy(exprs = Seq(Expr(GString(name))))))
        runtime.space.getData(returnChannel)
      }

    runtimeContainer.put(runtime)

    for {
      datum   <- result
      channel <- datum.a.channels
      par     <- channel.channelInstance.quote
    } yield par
  }

  def replayComputeState(hash: StateHash, terms: Seq[InternalProcessedDeploy])(
      implicit scheduler: Scheduler): Either[(Option[Deploy], Failed), StateHash] = {
    val runtime = runtimeContainer.take()
    val result  = replayEval(terms, runtime, hash)
    runtimeContainer.put(runtime)
    result
  }

  def computeState(hash: StateHash, terms: Seq[Deploy])(
      implicit scheduler: Scheduler): (StateHash, Seq[InternalProcessedDeploy]) = {
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

  def computeBonds(hash: StateHash): Seq[Bond] = {
    val resetRuntime = getResetRuntime(hash)
    // TODO: Switch to a read only name
    val bondsChannel     = Channel(Quote(Par().copy(exprs = Seq(Expr(GString("proofOfStake"))))))
    val bondsChannelData = resetRuntime.space.getData(bondsChannel)
    runtimeContainer.put(resetRuntime)
    toBondSeq(bondsChannelData)
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

  private def toBondSeq(data: Seq[Datum[ListChannelWithRandom]]): Seq[Bond] = {
    assert(data.length == 1, "Data length for bonds map was not 1.")
    val Datum(as: ListChannelWithRandom, _: Boolean, _: Produce) = data.head
    as.channels.head match {
      case Channel(Quote(p)) =>
        p.exprs.head.getEMapBody.ps.map {
          case (validator: Par, bond: Par) =>
            assert(validator.exprs.length == 1, "Validator in bonds map wasn't a single string.")
            assert(bond.exprs.length == 1, "Stake in bonds map wasn't a single integer.")
            val validatorName = validator.exprs.head.getGString
            val stakeAmount   = Math.toIntExact(bond.exprs.head.getGInt)
            Bond(ByteString.copyFrom(Base16.decode(validatorName)), stakeAmount)
        }.toList
      case Channel(_) => throw new Error("Matched a Channel that did not contain a Quote inside.")
    }
  }

  def getData(hash: ByteString, channel: Channel): Seq[Par] = {
    val resetRuntime                              = getResetRuntime(hash)
    val result: Seq[Datum[ListChannelWithRandom]] = resetRuntime.space.getData(channel)
    runtimeContainer.put(resetRuntime)
    for {
      datum   <- result
      channel <- datum.a.channels
      par     <- channel.channelInstance.quote
    } yield par
  }

  def getContinuation(hash: ByteString,
                      channels: immutable.Seq[Channel]): Seq[(Seq[BindPattern], Par)] = {
    val resetRuntime = getResetRuntime(hash)
    val results: Seq[WaitingContinuation[BindPattern, TaggedContinuation]] =
      resetRuntime.space.getWaitingContinuations(channels)
    runtimeContainer.put(resetRuntime)
    for {
      result <- results.filter(_.continuation.taggedCont.isParBody)
    } yield (result.patterns, result.continuation.taggedCont.parBody.get.body)
  }

  private def newEval(terms: Seq[Deploy], runtime: Runtime, initHash: StateHash)(
      implicit scheduler: Scheduler): (StateHash, Seq[InternalProcessedDeploy]) = {

    def doEval(terms: Seq[Deploy],
               hash: Blake2b256Hash,
               acc: Vector[InternalProcessedDeploy]): (StateHash, Vector[InternalProcessedDeploy]) =
      terms match {
        case deploy +: rem =>
          runtime.space.reset(hash)
          implicit val costAccountingAlg = CostAccountingAlg.unsafe[Task](CostAccount.zero)
          val (cost, errors)             = injAttempt(deploy, runtime.reducer, runtime.errorLog)
          val newCheckpoint              = runtime.space.createCheckpoint()
          val deployResult = InternalProcessedDeploy(deploy,
                                                     cost,
                                                     newCheckpoint.log,
                                                     DeployStatus.fromErrors(errors))
          if (errors.isEmpty) doEval(rem, newCheckpoint.root, acc :+ deployResult)
          else doEval(rem, hash, acc :+ deployResult)

        case _ => (ByteString.copyFrom(hash.bytes.toArray), acc)
      }

    doEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray), Vector.empty)
  }

  private def replayEval(terms: Seq[InternalProcessedDeploy],
                         runtime: Runtime,
                         initHash: StateHash)(
      implicit scheduler: Scheduler): Either[(Option[Deploy], Failed), StateHash] = {

    def doReplayEval(terms: Seq[InternalProcessedDeploy],
                     hash: Blake2b256Hash): Either[(Option[Deploy], Failed), StateHash] =
      terms match {
        case InternalProcessedDeploy(deploy, _, log, status) +: rem =>
          implicit val costAccountingAlg = CostAccountingAlg.unsafe[Task](CostAccount.zero)
          runtime.replaySpace.rig(hash, log.toList)
          //TODO: compare replay deploy cost to given deploy cost
          val (_, errors) = injAttempt(deploy, runtime.replayReducer, runtime.errorLog)
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
                  case Failure(_) =>
                    // TODO: Match _ for type of exception
                    Left(none[Deploy] -> UnusedCommEvent)
                }
              }
          }

        case _ => Right(ByteString.copyFrom(hash.bytes.toArray))
      }

    doReplayEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray))
  }

  private def injAttempt(deploy: Deploy, reducer: Reduce[Task], errorLog: ErrorLog)(
      implicit scheduler: Scheduler,
      costAlg: CostAccountingAlg[Task]): (PCost, Vector[Throwable]) = {
    implicit val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy.raw.get)))
    Try(reducer.inj(deploy.term.get).unsafeRunSync) match {
      case Success(_) =>
        val errors = errorLog.readAndClearErrorVector()
        val cost   = CostAccount.toProto(costAlg.getCost().unsafeRunSync)
        cost -> errors

      case Failure(ex) =>
        val otherErrors = errorLog.readAndClearErrorVector()
        val errors      = otherErrors :+ ex
        val cost        = CostAccount.toProto(costAlg.getCost().unsafeRunSync)
        cost -> errors
    }
  }
}

object RuntimeManager {
  type StateHash = ByteString

  def fromRuntime(active: Runtime): RuntimeManager = {
    active.space.clear()
    active.replaySpace.clear()
    val hash       = ByteString.copyFrom(active.space.createCheckpoint().root.bytes.toArray)
    val replayHash = ByteString.copyFrom(active.replaySpace.createCheckpoint().root.bytes.toArray)
    assert(hash == replayHash)
    val runtime = new SyncVar[Runtime]()
    runtime.put(active)

    new RuntimeManager(hash, runtime)
  }
}
