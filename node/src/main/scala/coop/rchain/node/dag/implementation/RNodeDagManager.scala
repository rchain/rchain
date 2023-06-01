package coop.rchain.node.dag.implementation

import cats.effect.{Async, Sync}
import coop.rchain.sdk.dag.data.{DagManager, DagView}
import fs2.Stream
import cats.effect.Ref

object RNodeDagManager {
  def apply[F[_]: Async, M, MId, S, SId](
      st: Ref[F, Map[MId, M]],
      requestMsg: MId => F[Unit],
      msgInput: Stream[F, M]
  ): F[RNodeDagManager[F, M, MId, S, SId]] =
    Sync[F].delay(new RNodeDagManager(st, requestMsg, msgInput))
}

final case class RNodeDagManager[F[_]: Async, M, MId, S, SId] private (
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
    messagesInput: Stream[F, M]
) extends DagManager[F, M, MId, S, SId] {

  /**
    * Returns DAG read-only representation starting with specified latest messages.
    *
    * NOTE: Getting view for invalid latest messages should throw error.
    */
  override def getDagView(seenBy: MId): F[DagView[F, M, MId, S, SId]] = ???

  /**
    * Latest messages (tips) seen in the whole DAG (not specific sender).
    */
  override def latestMessages: F[Map[S, Set[M]]] = ???

  /**
    * Thread safe function to insert new message to the DAG with corresponding finalized messages.
    */
  override def insert(msg: M, finalized: Set[MId], provisionallyFinalized: Boolean): F[Unit] = ???

  override def loadMessage(mid: MId): F[M] = ???

  override def loadSender(sid: SId): F[S] = ???
}
