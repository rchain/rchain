package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Deploy
import coop.rchain.casper.protocol.DeployString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.{Reduce, Runtime}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rspace.{trace, Blake2b256Hash, Checkpoint}
import monix.execution.Scheduler

import scala.concurrent.SyncVar
import scala.util.{Failure, Success, Try}
import RuntimeManager.StateHash
import monix.eval.Task

//runtime is a SyncVar for thread-safety, as all checkpoints share the same "hot store"
class RuntimeManager private (runtimeContainer: SyncVar[Runtime]) {

  def replayComputeState(log: trace.Log)(
      implicit scheduler: Scheduler): (StateHash, Seq[Deploy]) => Either[Throwable, Checkpoint] = {
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
        val error = eval(terms, riggedRuntime.replayReducer)
        val newCheckpoint = error.fold[Either[Throwable, Checkpoint]](
          Right(riggedRuntime.replaySpace.createCheckpoint()))(Left(_))
        runtimeContainer.put(riggedRuntime)
        newCheckpoint
      }
  }

  def computeState(hash: StateHash, terms: Seq[Deploy])(
      implicit scheduler: Scheduler): Either[Throwable, Checkpoint] = {
    val resetRuntime: Runtime = getResetRuntime(hash)
    val error                 = eval(terms, resetRuntime.reducer)
    val newCheckpoint = error.fold[Either[Throwable, Checkpoint]](
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

  private def eval(terms: Seq[Deploy], reducer: Reduce[Task])(
      implicit scheduler: Scheduler): Option[Throwable] =
    terms match {
      case deploy +: rest =>
        implicit val rand: Blake2b512Random = Blake2b512Random(
          DeployString.toByteArray(deploy.raw.get))
        Try(reducer.inj(deploy.term.get).unsafeRunSync) match {
          case Success(_)  => eval(rest, reducer)
          case Failure(ex) => Some(ex)
        }
      case Nil => None
    }
}

object RuntimeManager {
  type StateHash = ByteString

  def fromRuntime(runtime: SyncVar[Runtime]): (StateHash, RuntimeManager) = {
    val active = runtime.take()
    val hash   = ByteString.copyFrom(active.space.createCheckpoint().root.bytes.toArray)
    runtime.put(active)

    (hash, new RuntimeManager(runtime))
  }
}
