package coop.rchain.node.dag

import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.node.dag.implementation.{BlockStatus, NetworkBlockRequester, RNodeDagManager}
import coop.rchain.sdk.dag.data.{DagChecker, MessageCheckResult}

object RNodeStateSetup {
  def setupRNodeState[F[_]: Concurrent, M, MId, S](
      /**
        * INFO: DAG checker enables connection to existing code and new Casper logic.
        */
      dagChecker: DagChecker[F, M, MId]
  ) =
    for {
      /* State */

      // Block requester state
      blockReqSt <- Ref.of(Map[MId, BlockStatus[M, MId]]())

      // DAG Manager state
      dagMngrSt <- Ref.of(Map[MId, M]())

      requester <- NetworkBlockRequester[F, M, MId](blockReqSt)

      dagMngr <- RNodeDagManager[F, M, MId, S](
                  dagMngrSt,
                  requester.requestBlock,
                  requester.response,
                  dagChecker
                )
    } yield ()

  def createDagChecker[F[_]: Sync, M, MId]: F[DagChecker[F, M, MId]] = Sync[F].delay {
    new DagChecker[F, M, MId] {
      override def getDependencies(m: M): F[Set[MId]] = ???

      /**
        * Basic block validation (signature, fields, format, ...)
        */
      override def check(m: M): F[MessageCheckResult] = ???
    }
  }
}
