package coop.rchain.casper.eagercasper
import cats.syntax.all._
import coop.rchain.casper.eagercasper.Casper.{FinalityReconciliation, Fringe, LevelReconciliation}

// View on finality, starts from level going across all senders in the bonds map
final case class FinalityView[M, S](levelsViews: Vector[LevelView[M, S]]) {
  require(levelsViews.nonEmpty, "Finality view has to have at least one level.")
  def completeLevelIdx: Long = levelsViews.head.height
}

object FinalityView {

  /** Reconcile two finality views. */
  def reconcile[F[_], M, S](targetViews: Vector[FinalityView[M, S]])(
      bondsMap: Map[S, Long]
  ): FinalityReconciliation[M, S] = {
    // TODO if we do eager reconciliation, it's not guaranteed that all parents have the same complete fringe,
    //  therefore the highest one should be chosen.
    val highestCompleteLevelIdx = targetViews.map(_.completeLevelIdx).max
    val views = targetViews.map {
      case v @ FinalityView(levels) =>
        // align all views to start from the same level
        val diff = highestCompleteLevelIdx - v.completeLevelIdx
        FinalityView(levels.drop(diff.toInt))
    }
    assert(
      views.map(_.completeLevelIdx).distinct.size == 1,
      s"Reconciliation is possible only for views having the same start level."
    )
    // align views bu adding dummy records on top
    val maxHeight = views.map(_.levelsViews.size).max
    val aligned = views.map { v =>
      val toFill  = maxHeight - v.levelsViews.size
      val padding = (1 to toFill).map(_ => LevelView(-1, Vector.empty[Fringe[M, S]]))
      v.copy(levelsViews = v.levelsViews ++ padding)
    }

    // reconcile top down
    val reconciled = (maxHeight - 1 to 0).foldLeft(Vector.empty[LevelReconciliation[M, S]]) {
      case (acc, idx) =>
        val toReconcile = aligned.flatMap(_.levelsViews.lift(idx))
        // stop once all views see the same level - then all levels below are guaranteed to be the same as well
        val done = toReconcile.map(_.fringe.keySet).distinct.size == 1
        if (done) acc
        else acc :+ LevelView.reconcile(toReconcile)(bondsMap)
    }
    val newView = FinalityView(
      views.head.levelsViews.dropRight(reconciled.size) ++
        reconciled.map { case LevelReconciliation(newV, _) => newV }
    )
    FinalityReconciliation(newView, reconciled)
  }

  /** Return levels that are complete, along with new view, starting from latest complete level. */
  def finalize[M, S](
      view: FinalityView[M, S],
      bondsMap: Map[S, Long]
  ): (Vector[LevelView[M, S]], FinalityView[M, S]) = {
    // first level is already complete, so no reason to find it again, therefore drop(1)
    val viewsWithIdx = view.levelsViews.toIterator.drop(1).zipWithIndex
    val (complete, remainder) = viewsWithIdx
      .partition {
        case (lvl, idx) =>
          val levelAbove   = view.levelsViews.lift(idx + 1).map(_.fringe.keySet).getOrElse(Set())
          val levelSenders = lvl.partitions.flatMap(_.keySet) ++ levelAbove
          (bondsMap.keySet -- levelSenders).isEmpty
      }
      .bimap(_.map(_._1).toVector, _.map(_._1).toVector)
    // new finality view should start from the latest complete level found
    val newFinalityView =
      if (complete.isEmpty && remainder.isEmpty) view // workaround for first blocks after genesis
      else {
        val newCompleteOpt   = complete.lastOption
        val newLevelViewsOpt = newCompleteOpt.map(_ +: remainder)
        val newLevelViews    = newLevelViewsOpt.getOrElse(view.levelsViews)
        FinalityView(newLevelViews)
      }
    (complete, newFinalityView)
  }
}
