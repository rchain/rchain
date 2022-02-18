package coop.rchain.casper.lazycasper

import cats.effect.Sync
import cats.syntax.all._

/** Finalizer waiting for the next fringe advancement to be supermajority. */
final case class LazyFinalizer[F[_]: Sync, M, S](
    view: Map[S, M], // view of the message replayed - used to limit traversal complexity
    dag: DagData[F, M, S],
    bondsMap: Map[S, Long]
) {
  import dag._

  def findNextFringe(
      curFringe: Fringe[M, S],
      constraint: Set[S] => Boolean = _ => true
  ): F[Option[Fringe[M, S]]] = {

    // targets for finalization - next self message
    val targetF = curFringe.toVector
      .traverse { case (s, m) => witnessesF(m).map(_.find { case (s1, _) => s1 == s }) }
      .map(_.flatten.toMap)

    val findAdvancement =
      LazyPartitioner(dag).find(curFringe)(bondsMap, constraint).flatMap { partitionOpt =>
        partitionOpt.traverse(p => targetF.map(_.filter { case (s, _) => p.contains(s) }))
      }

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
        toCheck = (advJss ++ leftoversJss)
        r       = highestMessages(fullAcross +: toCheck)(seqNum)
      } yield r

    findAdvancement.flatMap(_.traverse(jumpToJss))
  }
}
