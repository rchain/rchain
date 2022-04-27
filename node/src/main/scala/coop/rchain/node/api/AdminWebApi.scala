package coop.rchain.node.api

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.api.BlockAPI_v2

trait AdminWebApi[F[_]] {
  def propose: F[String]
  def proposeResult: F[String]
}

object AdminWebApi {
  class AdminWebApiImpl[F[_]: Sync](blockApi: BlockAPI_v2[F]) extends AdminWebApi[F] {
    import WebApiSyntax._

    def propose: F[String] =
      blockApi.createBlock(isAsync = false).flatMap(_.liftToBlockApiErr)

    def proposeResult: F[String] =
      blockApi.getProposeResult.flatMap(_.liftToBlockApiErr)
  }
}
