package coop.rchain.sdk.dag.data

/**
  * High level module for managing DAG persistent state and producing read-only views.
  *
  * @tparam M represents a message (part of the block important for DAG structure)
  * @tparam S represents message creator (sender)
  */
trait DagManager[F[_], M, MId, S, SId] {

  /**
    * Returns DAG read-only representation starting with specified latest messages.
    *
    * NOTE: Getting view for invalid latest messages should throw error.
    */
  def getDagView(seenBy: MId): F[DagView[F, M, MId, S, SId]]

  /**
    * Latest messages (tips) seen in the whole DAG (not specific sender).
    */
  def latestMessages: F[Map[S, Set[M]]]

  /**
    * Thread safe function to insert new message to the DAG.
    */
  def insert(msg: M): F[Unit]

  /**
    * Thread safe function to update finalized nodes of the DAG seen by specific message.
    */
  def finalize(by: MId, finalized: Set[MId]): F[Unit]

  /* Basic loading operations */

  def loadMessage(mid: MId): F[M]

  def loadSender(sid: SId): F[M]
}

object DagManager {
  def apply[F[_], M, MId, S, SId](implicit instance: DagManager[F, M, MId, S, SId]): instance.type =
    instance
}
