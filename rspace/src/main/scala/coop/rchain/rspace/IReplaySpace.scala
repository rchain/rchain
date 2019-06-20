package coop.rchain.rspace

import cats.effect.Sync
import cats.implicits._
import coop.rchain.rspace.trace._
import coop.rchain.rspace.internal._
import coop.rchain.shared.Debug

trait IReplaySpace[F[_], C, P, A, R, K] extends ISpace[F, C, P, A, R, K] {

  private[rspace] val replayData: ReplayData = ReplayData.empty

  /** Rigs this ReplaySpace with the initial state and a log of permitted operations.
    * During replay, whenever a COMM event that is not available in the log occurs, an error is going to be raised.
    *
    * This method is not thread safe.
    *
    *  @param startRoot A [Blake2b256Hash] representing the intial state
    *  @param log A [Log] with permitted operations
    */
  def resetAndRig(startRoot: Blake2b256Hash, log: trace.Log)(implicit syncF: Sync[F]): F[Unit] =
    Debug.print(startRoot, log.size) >>
      syncF
        .delay {
          val (ioEvents, commEvents) = log.partition {
            case _: Produce => true
            case _: Consume => true
            case _: COMM    => false
          }

          // create a set of the "new" IOEvents
          val newStuff = ioEvents.toSet

          // create and prepare the ReplayData table
          replayData.clear()
          commEvents.foreach {
            case comm @ COMM(consume, produces) =>
              (consume +: produces).foreach { ioEvent =>
                if (newStuff(ioEvent)) {
                  replayData.addBinding(ioEvent, comm)
                }
              }
            case _ =>
              syncF.raiseError(
                new RuntimeException("BUG FOUND: only COMM events are expected here")
              )
          }
        }
        .flatMap { _ =>
          // reset to the starting checkpoint
          reset(startRoot)
        }
}
