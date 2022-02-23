package coop.rchain.pcasper.finalization.eager
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.pcasper.finalization.eager.EagerFinal.FinalityReconciliation
import coop.rchain.pcasper.finalization.{Finalizer, Fringe}

class EagerFinalizer[F[_]: Sync, M, S](
    sndr: S,
    bonds: Map[S, Long],
    witnessesF: M => F[Map[S, M]],
    justificationsF: M => F[Map[S, M]],
    sender: M => S
) extends Finalizer[F, M, S, EagerFinal[M, S]] {
  override def computeFinal(parentViews: Iterable[EagerFinal[M, S]]): F[EagerFinal[M, S]] =
//    val jsListF = justificationsF(_: M).map(_.valuesIterator.toList)
//
//    // TODO record reconciliation (how exactly not clear)
//    val FinalityReconciliation(newVew, _) = EagerFinal.reconcile(parentViews.toVector)(bonds)
//
//    // fringe of a partition where sender belongs
//    val baseFringe = {
//      val partitions = newVew.provisional.reverse.find(_.fringe.keySet contains sndr)
//      assert(
//        partitions.nonEmpty,
//        s"No level found in provisional view where sender belongs to."
//      )
//      partitions.get.fringe
//    }
//
//    EagerPartitioner(witnessesF, jsListF, sender)
//      .findPartition(baseFringe)
//      .flatMap(_.traverse { partitionFringe =>
//        if (partitionFringe.isEmpty) none[Fringe[M, S]].pure[F]
//        else {
//          val partitionBondsMap = bonds.filter {
//            case (s, _) => partitionFringe.keySet.contains(s)
//          }
//          SimpleProtocol
//            .isSafe(partitionFringe, partitionBondsMap, witnessesF, sender)
//            .map(_.guard[Option].as(partitionFringe))
//        }
//      })
//      .map(_.flatten)
    parentViews.head.pure[F]

  override def genesisFinal(genesis: M): EagerFinal[M, S] = {
    val complete = bonds.map { case (s, _) => s -> genesis }
    EagerFinal(0, complete, Vector())
  }
}
