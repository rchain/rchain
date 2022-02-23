package coop.rchain.pcasper.finalization.eager
import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.pcasper.finalization.{closestMessages, Fringe, Partitioner}
import coop.rchain.shared.syntax._
import EagerPartitioner.timelySet

final case class EagerPartitioner[F[_]: Sync, M, S](
    witnessesF: M => F[Map[S, M]],
    justificationsF: M => F[List[M]],
    sender: M => S
) extends Partitioner[F, M, S] {
  override def findPartition(base: Fringe[M, S]): F[Option[Fringe[M, S]]] = {
    val resultF = closestMessages(base)(witnessesF)
      .map(_.toList.map { case (_, m) => m })
      .flatMap(timelySet(_)(justificationsF))
      .flatMap { timely =>
        // fringe advancements in partitions size of less then 2 are not possible - in this case add witnesses
        if (timely.size == 1) {
          val single = timely.head
          witnessesF(single).map(_ + (sender(single) -> single))
        } else timely.map(m => sender(m) -> m).toMap.pure[F]
      }

    resultF.map(r => (r.size != 1).guard[Option].as(r))
  }
}

object EagerPartitioner {

  /**
    * Set of timely messages across target.
    * Timely message - message that do not have reference to other messages in the target set.
    */
  def timelySet[F[_]: Monad, M, S](
      target: List[M]
  )(justificationsF: M => F[List[M]]): F[List[M]] =
    target.filterA(m => justificationsF(m).map(_.exists(target.contains)).not)
}
