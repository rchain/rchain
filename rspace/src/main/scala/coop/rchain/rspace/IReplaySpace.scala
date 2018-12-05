package coop.rchain.rspace

import cats.effect.Sync
import cats.implicits._
import coop.rchain.rspace.trace._
import coop.rchain.rspace.internal._

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
