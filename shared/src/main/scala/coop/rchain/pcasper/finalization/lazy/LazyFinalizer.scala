package coop.rchain.pcasper.finalization.`lazy`

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.pcasper.finalization._

final case class LazyFinalizer[F[_]: Sync, M, S, P](
    sndr: S,
    view: Map[S, M],
    bonds: Map[S, Long],
    dag: DagData[F, M, S]
) extends Finalizer[F, M, S, LazyFinal[M, S]] {
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
  val dagInView = new DagData[F, M, S] {
    override def witnessesF: M => F[Map[S, M]]      = witnessesInViewF
    override def justificationsF: M => F[Map[S, M]] = dag.justificationsF
    override def seqNum: M => Long                  = dag.seqNum
    override def sender: M => S                     = dag.sender
  }

  private def nextFringeOpt(
      base: Fringe[M, S],
      constraint: Set[S] => Boolean = _ => true
  ): F[Option[Fringe[M, S]]] = {

    val findAdvancement = LazyPartitioner(dagInView, bonds, constraint).findPartition(base)

    // advance fringe if there are justifications higher
    def jumpToJss(fringe: Map[S, M]): F[Map[S, M]] =
      for {
        // across justifications of fringe advancement
        advJss <- fringe.valuesIterator.toVector.traverse(justificationsF)
        // find in justification of advanced senders the higher messages outside advanced senders
        leftoversViews = advJss.map { jsMap =>
          jsMap.filterKeys { !fringe.keySet.contains(_) }
        }
        leftovers    = highestMessages(leftoversViews)(seqNum)
        leftoversJss <- leftovers.valuesIterator.toVector.traverse(justificationsF)
        // now given full fringe all across
        fullAcross = fringe ++ leftovers
        // check for each message in if there are justifications of other messages higher
        toCheck = advJss ++ leftoversJss
        r       = highestMessages(fullAcross +: toCheck)(seqNum)
      } yield r

    findAdvancement.flatMap(_.traverse(jumpToJss))
  }

  override def computeFinal(minGenJsViews: Iterable[LazyFinal[M, S]]): F[LazyFinal[M, S]] = {

    val baseComplete = highestMessages(minGenJsViews.map(_.complete))(seqNum)
    val baseProvisional = {
      val partitions = minGenJsViews.filter {
        case LazyFinal(_, partition) => partition.contains(sndr)
      }
      assert(
        partitions.size <= 1,
        s"Overlapping partitions are not allowed by LazyCasper, please check if input argument " +
          s"is a minimal generative justification set."
      )
      if (partitions.nonEmpty) partitions.head.provisional else baseComplete
    }

    val newProvisionalF: F[Fringe[M, S]] =
      if (baseComplete != baseProvisional)
        nextFringeOpt(baseProvisional).map(_.getOrElse(baseProvisional))
      else Map.empty[S, M].pure[F]

    val newCompleteF: F[Fringe[M, S]] =
      nextFringeOpt(baseComplete, isSupermajority(_, bonds)).map(_.getOrElse(baseComplete))

    for {
      pf <- newProvisionalF
      cf <- newCompleteF
    } yield LazyFinal(cf, pf)
  }

  override def genesisFinal(genesis: M): LazyFinal[M, S] = {
    val complete = bonds.map { case (s, _) => s -> genesis }
    LazyFinal(complete, complete)
  }
}
