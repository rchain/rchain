package coop.rchain.sdk.dag.data

/**
  * High level module for managing DAG persistent state and producing read-only views.
  *
  * @tparam M represents a message (part of the block important for DAG structure)
  * @tparam S represents message creator (sender)
  */
trait DagManager[F[_], M, S] {

  /**
    * Returns DAG read-only representation starting with specified latest messages.
    *
    * NOTE: Getting view for invalid latest messages should throw error.
    */
  def getDagView(latestMessages: Set[M]): F[DagView[F, M, S]]

  /**
    * Thread safe function to insert new node to the DAG.
    */
  def updateDag(m: M): F[Unit]
}

object DagManager {
  def apply[F[_], M, S](implicit instance: DagManager[F, M, S]): instance.type = instance
}
