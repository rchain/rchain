package coop.rchain.sdk.dag.data

import fs2.{Chunk, Stream}

/**
  * Represents read-only view of the DAG starting from latest messages.
  */
trait DagView[F[_], M, S] {
  implicit val dd: DagData[M, S]
  implicit val ordM: Ordering[M]

  /**
    * Latest messages (tips) of the DAG for this view.
    */
  def latestMessages: F[Map[S, Set[M]]]

  /**
    * Traversal through DAG starting from latest messages.
    */
  def messages: Stream[F, Chunk[(M, List[M])]]
}

object DagView {
  def apply[F[_], M, S](implicit instance: DagView[F, M, S]): instance.type = instance
}
