package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.protocol.{Bond, Deploy, DeployString}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models._
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.rholang.interpreter.{ErrorLog, Reduce, Runtime}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rspace.{trace, Blake2b256Hash, Checkpoint}
import coop.rchain.rspace.internal.Datum
import monix.execution.Scheduler

import scala.concurrent.SyncVar
import scala.util.{Failure, Success, Try}
import RuntimeManager.{DeployError, StateHash}
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.rspace.internal.Datum
import coop.rchain.rspace.trace.Produce
import monix.eval.Task

//runtime is a SyncVar for thread-safety, as all checkpoints share the same "hot store"
class RuntimeManager private (val emptyStateHash: ByteString, runtimeContainer: SyncVar[Runtime]) {

  def captureResults(start: StateHash, term: Par, name: String = "__SCALA__")(
      implicit scheduler: Scheduler): Seq[Par] = {
    val runtime = getResetRuntime(start)
    val deploy  = ProtoUtil.termDeploy(term)
    val error   = eval(deploy :: Nil, runtime.reducer, runtime.errorLog)

    //TODO: Is better error handling needed here?
    val result = error.fold {
      val returnChannel = Channel(Quote(Par().copy(exprs = Seq(Expr(GString(name))))))
      runtime.space.getData(returnChannel)
    }(_ => Nil)

    runtimeContainer.put(runtime)

    for {
      datum   <- result
      channel <- datum.a.channels
      par     <- channel.channelInstance.quote
    } yield par
  }

  def replayComputeState(log: trace.Log)(implicit scheduler: Scheduler)
    : (StateHash, Seq[Deploy]) => Either[DeployError, Checkpoint] = {
    (hash: StateHash, terms: Seq[Deploy]) =>
      {
        val runtime   = runtimeContainer.take()
        val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
        val riggedRuntime = Try(runtime.replaySpace.rig(blakeHash, log)) match {
          case Success(_) => runtime
          case Failure(ex) =>
            runtimeContainer.put(runtime)
            throw ex
        }
        val error = eval(terms, riggedRuntime.replayReducer, riggedRuntime.errorLog)
        val newCheckpoint = error.fold[Either[DeployError, Checkpoint]](
          Right(riggedRuntime.replaySpace.createCheckpoint()))(Left(_))
        runtimeContainer.put(riggedRuntime)
        newCheckpoint
      }
  }

  def computeState(hash: StateHash, terms: Seq[Deploy])(
      implicit scheduler: Scheduler): Either[DeployError, Checkpoint] = {
    val resetRuntime: Runtime = getResetRuntime(hash)
    val error                 = eval(terms, resetRuntime.reducer, resetRuntime.errorLog)
    val newCheckpoint = error.fold[Either[DeployError, Checkpoint]](
      Right(resetRuntime.space.createCheckpoint()))(Left(_))
    runtimeContainer.put(resetRuntime)
    newCheckpoint
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

  private def toBondSeq(data: Seq[Datum[ListChannelWithRandom]]): Seq[Bond] = {
    assert(data.length == 1)
    val Datum(as: ListChannelWithRandom, _: Boolean, _: Produce) = data.head
    as.channels.head match {
      case Channel(Quote(p)) =>
        p.exprs.head.getEMapBody.ps.map {
          case (validator: Par, bond: Par) =>
            assert(validator.exprs.length == 1)
            assert(bond.exprs.length == 1)
            val validatorName = validator.exprs.head.getGString
            val stakeAmount   = bond.exprs.head.getGInt
            Bond(ByteString.copyFrom(Base16.decode(validatorName)), stakeAmount)
        }.toList
      case Channel(_) => throw new Error("Should never happen")
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

  private def eval(terms: Seq[Deploy], reducer: Reduce[Task], errorLog: ErrorLog)(
      implicit scheduler: Scheduler): Option[DeployError] =
    terms match {
      case deploy +: rest =>
        implicit val rand: Blake2b512Random = Blake2b512Random(
          DeployString.toByteArray(deploy.raw.get))
        Try(reducer.inj(deploy.term.get).unsafeRunSync) match {
          case Success(_) =>
            val errors = errorLog.readAndClearErrorVector()
            if (errors.isEmpty)
              eval(rest, reducer, errorLog)
            else
              Some(deploy -> errors)
          case Failure(ex) =>
            val otherErrors = errorLog.readAndClearErrorVector()
            Some(deploy -> (ex +: otherErrors))
        }
      case Nil => None
    }
}

object RuntimeManager {
  type StateHash   = ByteString
  type DeployError = (Deploy, Vector[Throwable])

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
