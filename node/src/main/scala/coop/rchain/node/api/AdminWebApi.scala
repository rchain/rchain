package coop.rchain.node.api

import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.{
  LastFinalizedHeightConstraintChecker,
  SafetyOracle,
  SynchronyConstraintChecker
}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.Log
import coop.rchain.state.StateManager

trait AdminWebApi[F[_]] {
  def propose: F[String]
}

object AdminWebApi {

  class AdminWebApiImpl[F[_]: Sync: Concurrent: EngineCell: Log: Span: SafetyOracle: BlockStore: Metrics: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker](
      blockApiLock: Semaphore[F],
      stateManager: StateManager[F]
  ) extends AdminWebApi[F] {
    import WebApiSyntax._

    def propose: F[String] =
      BlockAPI.createBlock[F](blockApiLock).flatMap(_.liftToBlockApiErr)
  }

  final class BlockApiException(message: String) extends Exception(message)

}
