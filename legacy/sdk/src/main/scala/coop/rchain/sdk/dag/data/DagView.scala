package coop.rchain.sdk.dag.data

import fs2.{Chunk, Stream}

/**
  * Represents read-only view of the DAG starting from top message which sees the view.
  */
trait DagView[F[_], M, MId, S, SId] {
  implicit val dd: DagData[M, MId, S, SId]
  implicit val ordM: Ordering[M]

  /**
    * Top message from which this DAG view is seen.
    */
  def seenBy: F[M]

  /**
    * Traversal through DAG starting from latest messages.
    */
  def messages: Stream[F, Chunk[(M, List[M])]]

  /* Basic loading operations */

  def loadMessage(mid: MId): F[M]

  def loadSender(sid: SId): F[S]
}

object DagView {
  def apply[F[_], M, MId, S, SId](implicit instance: DagView[F, M, MId, S, SId]): instance.type =
    instance
}
