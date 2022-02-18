package coop.rchain.casper.lazycasper
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.lazycasper.syntax.all._

object LazyCasper {

  final case class FinalityState[M, S](completeFringe: Fringe[M, S], mFinal: Fringe[M, S])

  /** Should be invoked before replay to advance finalization view of a sender which is a base for message creation. */
  def finalise[F[_]: Sync, M, S](curFinalityState: FinalityState[M, S])(
      bonds: Map[S, Long], // senders bonded in the state of the current fringe
      view: Map[S, M],     // view of the message replayed - used to limit traversal complexity
      dag: DagData[F, M, S],
      recordNewCompleteFringe: Fringe[M, S] => F[Unit]
  ): F[FinalityState[M, S]] = {
    import dag._

    val viewBoundary = view.mapValues(seqNum)
    val witnessesInViewF =
      witnessesF(_: M)
        .map(_.filter {
          case (s, m) =>
            val latestInView = viewBoundary.get(s)
            assert(
              latestInView.isDefined,
              s"Sender $s is not present in latest messages defining the view $view."
            )
            seqNum(m) <= latestInView.get
        }.toMap)
    val dagTruncated = new DagData[F, M, S] {
      override def witnessesF: M => F[Map[S, M]]      = witnessesInViewF
      override def justificationsF: M => F[Map[S, M]] = dag.justificationsF
      override def seqNum: M => Long                  = dag.seqNum
      override def sender: M => S                     = dag.sender
    }

    val finalizer                = LazyFinalizer(view, dagTruncated, bonds)
    val newProvisionalFringeOptF = finalizer.findNextFringe(curFinalityState.mFinal)
    // TODO here recordNewCompleteFringe should use provisional fringe to make merge of the new
    //  final state more efficient
    val newCompleteFringeOptF = finalizer
      .findNextCompleteFringe(curFinalityState.completeFringe)
      .flatTap(_.traverse(recordNewCompleteFringe))

    for {
      // try to advance provisional finalization
      npOpt     <- newProvisionalFringeOptF
      newMFinal = npOpt.getOrElse(curFinalityState.mFinal)
      // try to advance complete finalization
      ncOpt             <- newCompleteFringeOptF
      newCompleteFringe = ncOpt.getOrElse(curFinalityState.completeFringe)
    } yield FinalityState(newCompleteFringe, newMFinal)
  }
}
