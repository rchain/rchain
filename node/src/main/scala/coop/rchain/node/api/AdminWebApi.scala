package coop.rchain.node.api

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import cats.effect.concurrent.Semaphore
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.{
  LastFinalizedHeightConstraintChecker,
  SafetyOracle,
  SynchronyConstraintChecker
}
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.node.api.AdminWebApi._
import coop.rchain.shared.Log
import monix.eval.Task

trait AdminWebApi[F[_]] {
  def propose: F[String]
}

object AdminWebApi {

  class AdminWebApiImpl[F[_]: Sync: Concurrent: EngineCell: Log: Span: SafetyOracle: BlockStore: Metrics: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker](
      apiMaxBlocksLimit: Int,
      blockApiLock: Semaphore[F]
  ) extends AdminWebApi[F] {
    import AdminWebApiSyntax._

    def propose: F[String] =
      BlockAPI.createBlock[F](blockApiLock).flatMap(_.liftToBlockApiErr)
  }

  final class BlockApiException(message: String) extends Exception(message)

  import AdminWebApiSyntax._

  object AdminWebApiSyntax {
    implicit final class EitherStringExt[A](val x: Either[String, A]) extends AnyVal {
      def liftToBlockApiErr[F[_]: Sync]: F[A] =
        x.leftMap(new BlockApiException(_)).liftTo[F]
    }
  }

}
