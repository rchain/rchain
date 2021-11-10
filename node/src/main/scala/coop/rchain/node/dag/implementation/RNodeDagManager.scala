package coop.rchain.node.dag.implementation

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import coop.rchain.sdk.dag.data.{DagChecker, DagManager, DagView}
import fs2.Stream

object RNodeDagManager {
  def apply[F[_]: Concurrent, M, MId, S](
      st: Ref[F, Map[MId, M]],
      requestMsg: MId => F[Unit],
      msgInput: Stream[F, M],
      dagChecker: DagChecker[F, M, MId]
  ): F[RNodeDagManager[F, M, MId, S]] =
    Sync[F].delay(new RNodeDagManager(st, requestMsg, msgInput, dagChecker))
}

final case class RNodeDagManager[F[_]: Concurrent, M, MId, S] private (
    /**
      * DagManager internal in-memory state.
      */
    st: Ref[F, Map[MId, M]],
    /**
      * Request external module for required messages.
      */
    requestMsg: MId => F[Unit],
    /**
      * Receiving stream of messages.
      */
    messagesInput: Stream[F, M],
    /**
      * All necessary validation to maintain the DAG.
      *
      * This interface should bring the Casper logic into DAG manager.
      */
    dagChecker: DagChecker[F, M, MId]
) extends DagManager[F, M, S] {

  /**
    * Returns DAG read-only representation starting with specified latest messages.
    *
    * NOTE: Getting view for invalid latest messages should throw error.
    */
  override def getDagView(latestMessages: Set[M]): F[DagView[F, M, S]] = ???

  /**
    * Thread safe function to insert new node to the DAG.
    */
  override def updateDag(m: M): F[Unit] = ???
}
