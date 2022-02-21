package coop.rchain.pcasper.finalization.eager

import cats.syntax.all._
import coop.rchain.pcasper.finalization.{Final, Fringe}

// View on finality, starts from level going across all senders in the bonds map
final case class EagerFinal[M, S](
    completeLevelIdx: Long,
    override val complete: Fringe[M, S],
    override val provisional: Vector[LevelView[M, S]]
) extends Final[M, S] {
  override type Provisional = Vector[LevelView[M, S]]
}

object EagerFinal {

  /**
    * Result of reconciliation of several [[EagerFinal]].
    * @param newView  new finality View
    * @param reconciled Reconciliations one on different levels.
    *                   This is an input for conflict resolver.
    *                   List is ordered down top according to block height, which means rejections computed
    *                   for element N have to be respected on level N + 1.
    */
  final case class FinalityReconciliation[M, S](
      newView: EagerFinal[M, S],
      reconciled: Vector[LevelReconciliation[M, S]]
  )

  /**
    * Result of reconciliation of several [[LevelView]]
    * @param newView    new finality view
    * @param reconciled Partial fringes for which conflict resolution has to be run again.
    */
  final case class LevelReconciliation[M, S](
      newView: LevelView[M, S],
      reconciled: Vector[Fringe[M, S]]
  )

  /** Reconcile two finality views. */
  def reconcile[F[_], M, S](targetViews: Vector[EagerFinal[M, S]])(
      bondsMap: Map[S, Long]
  ): FinalityReconciliation[M, S] = {
    // TODO if we do eager reconciliation, it's not guaranteed that all parents have the same complete fringe,
    //  therefore the highest one should be chosen.
    val highestCompleteLevelIdx = targetViews.map(_.completeLevelIdx).max
    val views = targetViews.map {
      case v @ EagerFinal(_, _, levels) =>
        // align all views to start from the same level
        val diff = highestCompleteLevelIdx - v.completeLevelIdx
        EagerFinal(v.completeLevelIdx, v.complete, levels.drop(diff.toInt))
    }
    assert(
      views.map(_.completeLevelIdx).distinct.size == 1,
      s"Reconciliation is possible only for views having the same start level."
    )
    // align views bu adding dummy records on top
    val maxHeight = views.map(_.provisional.size).max
    val aligned = views.map { v =>
      val toFill  = maxHeight - v.provisional.size
      val padding = (1 to toFill).map(_ => LevelView(-1, Vector.empty[Fringe[M, S]]))
      v.copy(provisional = v.provisional ++ padding)
    }

    // reconcile top down
    val reconciled = (maxHeight - 1 to 0).foldLeft(Vector.empty[LevelReconciliation[M, S]]) {
      case (acc, idx) =>
        val toReconcile = aligned.flatMap(_.provisional.lift(idx))
        // stop once all views see the same level - then all levels below are guaranteed to be the same as well
        val done = toReconcile.map(_.fringe.keySet).distinct.size == 1
        if (done) acc
        else acc :+ LevelView.reconcile(toReconcile)(bondsMap)
    }
    val newView = EagerFinal(
      views.head.completeLevelIdx,
      views.head.complete,
      views.head.provisional.dropRight(reconciled.size) ++
        reconciled.map { case LevelReconciliation(newV, _) => newV }
    )
    FinalityReconciliation(newView, reconciled)
  }

  /** Return levels that are complete, along with new view, starting from latest complete level. */
  def finalize[M, S](
      view: EagerFinal[M, S],
      bondsMap: Map[S, Long]
  ): (Vector[LevelView[M, S]], EagerFinal[M, S]) = {
    // first level is already complete, so no reason to find it again, therefore drop(1)
    val viewsWithIdx = view.provisional.toIterator.zipWithIndex
    val (complete, remainder) = viewsWithIdx
      .partition {
        case (lvl, idx) =>
          val levelAbove   = view.provisional.lift(idx + 1).map(_.fringe.keySet).getOrElse(Set())
          val levelSenders = lvl.partitions.flatMap(_.keySet) ++ levelAbove
          (bondsMap.keySet -- levelSenders).isEmpty
      }
      .bimap(_.map(_._1).toVector, _.map(_._1).toVector)
    // new finality view should start from the latest complete level found
    val newFinalityView =
      if (complete.isEmpty && remainder.isEmpty) view // workaround for first blocks after genesis
      else {
        val newComplete   = complete.lastOption.map(_.fringe).getOrElse(view.complete)
        val newCompleteIx = view.completeLevelIdx + complete.size
        EagerFinal(newCompleteIx, newComplete, remainder)
      }
    (complete, newFinalityView)
  }
}
