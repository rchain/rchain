package coop.rchain.rspace

import cats.effect.Sync
import cats.implicits._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace._
import coop.rchain.shared.Log

trait IReplaySpace[F[_], C, P, A, K] extends ISpace[F, C, P, A, K] {

  protected def logF: Log[F]

  private[rspace] val replayData: ReplayData = ReplayData.empty

  /** Rigs this ReplaySpace with the initial state and a log of permitted operations.
    * During replay, whenever a COMM event that is not available in the log occurs, an error is going to be raised.
    *
    * This method is not thread safe.
    *
    *  @param startRoot A [Blake2b256Hash] representing the intial state
    *  @param log A [Log] with permitted operations
    */
  def rigAndReset(startRoot: Blake2b256Hash, log: trace.Log)(implicit syncF: Sync[F]): F[Unit] =
    rig(log) >> reset(startRoot)

  def rig(rawLog: trace.Log)(implicit syncF: Sync[F]): F[Unit] =
    syncF
      .delay {
        def logFreeAndComm = {
          val boundEvs = rawLog flatMap {
            case COMM(c, ps, _, _) => c +: ps
            case _                 => Set[Event]()
          }
          rawLog.filterNot(boundEvs.toSet)
        }
        val log = logFreeAndComm
//        val log = rawLog

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
          case comm @ COMM(consume, produces, _, _) =>
            (consume +: produces).foreach { ioEvent =>
              if (newStuff(ioEvent)) {
//                replayData.addBinding(ioEvent, comm)
                println(s"Event is free: $ioEvent")
              }

              replayData.addBinding(ioEvent, comm)
            }
          case _ =>
            syncF.raiseError(new RuntimeException("BUG FOUND: only COMM events are expected here"))
        }
      }

  def checkReplayData()(implicit syncF: Sync[F]): F[Unit] =
    syncF
      .delay(replayData.isEmpty)
      .ifM(
        ifTrue = syncF.unit,
        ifFalse = {
          val msg =
            s"Unused COMM event: replayData multimap has ${replayData.size} elements left"
          logF.error(msg) >> logF.error(replayData.toString) >> syncF.raiseError[Unit](
            new ReplayException(msg)
          )
        }
      )

}
