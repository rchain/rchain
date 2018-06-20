package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rspace.Blake2b256Hash

import monix.execution.Scheduler

import scala.concurrent.SyncVar
import scala.util.{Failure, Success, Try}
import RuntimeManager.StateHash

//runtime is a SyncVar for thread-safety, as all checkpoints share the same "hot store"
class RuntimeManager private (runtime: SyncVar[Runtime]) {

  def computeState(hash: StateHash, terms: List[Par])(
      implicit scheduler: Scheduler): Either[Throwable, StateHash] = {
    val active = getActive(hash)
    val error  = eval(terms, active)
    val newHash = error.fold[Either[Throwable, ByteString]](
      Right(ByteString.copyFrom(active.space.createCheckpoint().root.bytes.toArray)))(Left(_))
    runtime.put(active)
    newHash
  }

  def storageRepr(hash: StateHash): String = {
    val active = getActive(hash)
    val result = StoragePrinter.prettyPrint(active.space.store)
    runtime.put(active)
    result
  }

  private def getActive(hash: StateHash): Runtime = {
    val active    = runtime.take()
    val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
    Try(active.space.reset(blakeHash)) match {
      case Success(_) => active
      case Failure(ex) =>
        runtime.put(active)
        throw ex
    }
  }

  private def eval(terms: List[Par], active: Runtime)(
      implicit scheduler: Scheduler): Option[Throwable] =
    terms match {
      case term :: rest =>
        Try(active.reducer.inj(term).unsafeRunSync) match {
          case Success(_)  => eval(rest, active)
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
