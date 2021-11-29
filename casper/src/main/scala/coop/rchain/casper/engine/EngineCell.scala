package coop.rchain.casper.engine

import cats._, cats.data._, cats.syntax.all._
import cats.effect._
import coop.rchain.shared.Cell

object EngineCell {
  type EngineCell[F[_]] = Cell[F, Engine[F]]
  def apply[F[_]](implicit ev: EngineCell[F]): EngineCell[F] = ev
  def init[F[_]: Concurrent]: F[EngineCell[F]] =
    Cell.mvarCell[F, Engine[F]](Engine.noop)
}
