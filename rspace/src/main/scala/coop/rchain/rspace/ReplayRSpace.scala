package coop.rchain.rspace

import cats.{Id, Monad}
import cats.effect.{ContextShift, Sync}
import cats.implicits._
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.history.{Branch, ITrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Produce, _}
import coop.rchain.shared.SyncVarOps._
import scodec.Codec

import scala.Function.const
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar
import scala.concurrent.ExecutionContext
import kamon._

trait IReplaySpace[F[_], C, P, E, A, R, K] extends ISpace[F, C, P, E, A, R, K] {

  private[rspace] val replayData: ReplayData = ReplayData.empty

  /** Rigs this ReplaySpace with the initial state and a log of permitted operations.
    * During replay, whenever a COMM event that is not available in the log occurs, an error is going to be raised.
    *
    * This method is not thread safe.
    *
    *  @param startRoot A [Blake2b256Hash] representing the intial state
    *  @param log A [Log] with permitted operations
    */
  def rig(startRoot: Blake2b256Hash, log: trace.Log)(implicit syncF: Sync[F]): F[Unit] =
    syncF
      .delay {
        // create a set of the "new" IOEvents
        val newStuff: Set[Event] = log.filter {
          case _: Produce => true
          case _: Consume => true
          case _          => false
        }.toSet
        // create and prepare the ReplayData table
        replayData.clear()
        log.foreach {
          case comm @ COMM(consume, produces) =>
            (consume +: produces).foreach { ioEvent =>
              if (newStuff(ioEvent)) {
                replayData.addBinding(ioEvent, comm)
              }
            }
          case _ =>
            ()
        }
      }
      .flatMap { _ =>
        // reset to the starting checkpoint
        reset(startRoot)
      }
}

object ReplayRSpace {

  def create[F[_], C, P, E, A, R, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext
  ): F[IReplaySpace[F, C, P, E, A, R, K]] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val mainStore = context match {
      case lmdbContext: LMDBContext[C, P, A, K] =>
        LMDBStore.create[C, P, A, K](lmdbContext, branch)

      case memContext: InMemoryContext[C, P, A, K] =>
        InMemoryStore.create(memContext.trieStore, branch)

      case mixedContext: MixedContext[C, P, A, K] =>
        InMemoryStore.create(mixedContext.trieStore, branch)
    }

    val replaySpace: IReplaySpace[F, C, P, E, A, R, K] =
      new spaces.FineGrainedReplayRSpace[F, C, P, E, A, R, K](mainStore, branch)

    /*
     * history.initialize returns true if the history trie contains no root (i.e. is empty).
     *
     * In this case, we create a checkpoint for the empty store so that we can reset
     * to the empty store state with the clear method.
     */
    if (history.initialize(mainStore.trieStore, branch)) {
      replaySpace.createCheckpoint().map(_ => replaySpace)
    } else {
      replaySpace.pure[F]
    }
  }

}
