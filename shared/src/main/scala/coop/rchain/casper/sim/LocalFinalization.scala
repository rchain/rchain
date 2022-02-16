package coop.rchain.casper.sim

import cats.Id
import coop.rchain.casper.eagercasper.Casper.FinalityReconciliation
import coop.rchain.casper.eagercasper.{Casper, FinalityView, LevelView}
import coop.rchain.casper.sim.Simulation.{Msg, MsgView, Sender}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.shared.Stopwatch

/**
  * A high level logic for local finalization.
  */
object LocalFinalization {
  def run(msg: Msg)(
      witnessF: Msg => Map[Sender, Msg],
      justificationsF: Msg => Set[MsgView]
  ): (Vector[LevelView[Msg, Sender]], FinalityView[Msg, Sender]) = {

    // Minimal generative justification set - justifications that do not see other justifications
    val jss = justificationsF(msg)
    val minGenJs = {
      val jsLvl2 = jss.map(_.root).flatMap(justificationsF)
      jss diff jsLvl2
    }

    // Bonds map - bonded senders common for all MGJs (not ejected).
    val bondsMap = Simulation.bondsMap(minGenJs.map(_.root.bondsMap))

    // View of the message.
    val view     = jss.map(_.root).map(m => m.sender -> m).toMap
    val viewSeqs = view.mapValues(_.senderSeq)
    // Function to load only witnesses in the view
    val witsInViewF = witnessF(_: Msg).filter {
      case (s, m) => m.senderSeq <= viewSeqs.getOrElse(s, 0)
    }

    // STEP 1.
    // Reconcile finality views of parents
    val parentsFinals = minGenJs.map(_.finalized)

    // TODO adjustments should be processed - do real conflict resolution
    val (FinalityReconciliation(parentsReconciled, _), pvrt) = Stopwatch.profile {
      FinalityView.reconcile[Id, Msg, Sender](parentsFinals.toVector)(bondsMap)
    }

    // STEP 2.
    // Search for finalization advancement compared to parents view

    val (preFinal, fat) = Stopwatch.profile {
      val baseView = parentsReconciled.levelsViews.last
      val fringeAdvOpt =
        Casper.finalize[Id, Msg, Sender](baseView.fringe, msg.bondsMap)(
          witsInViewF,
          justificationsF(_).map(m => m.root.sender -> m.root).toMap,
          _.sender
        )
      fringeAdvOpt
        .map { // split by block num, as it might be the case that partition fringe is a diagonal
          _.groupBy { case (_, m) => m.height }.toList
            .sortBy {
              case (height, _) => height
            }
            .map(_._2)
        }
        .map { newLvl =>
          newLvl.foldLeft(parentsReconciled) {
            case (acc, lvl) =>
              val newLvlIdx    = acc.levelsViews.last.height + 1
              val newLevelView = acc.levelsViews :+ LevelView(newLvlIdx, Vector(lvl))
              acc.copy(levelsViews = newLevelView)
          }
        }
        .getOrElse(parentsReconciled)
    }

    // STEP 3.
    // Find fringes that can be finalized and finalization view to put in message.
    val ((toFinalize, mFinal), ncft) = Stopwatch.profile {
      FinalityView.finalize(preFinal, msg.bondsMap)
    }

//    println(s"Parents finality views reconciled in $pvrt ")
//    println(s"Finalization advancement computed in $fat ")
//    println(s"New complete fringes found in $ncft ")
//    println()
//    println(parentsReconciled.map(_._2.fringe.map { case (s, m) => (s.id, m.id) }))
//    println(preFinal.map(_._2.fringe.map { case (s, m)          => (s.id, m.id) }))
//    println(real.map(_._2.fringe.map { case (s, m)              => (s.id, m.id) }))
//    println(mFinal.map(_._2.fringe.map { case (s, m)            => (s.id, m.id) }))

    (toFinalize, mFinal)
  }
}
