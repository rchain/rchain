package coop.rchain.rholang.interpreter.accounting

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.mtl._
import cats.{FlatMap, Monad}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

trait CostAccounting[F[_]] {
  def charge(cost: Cost): F[Unit]
  def get(): F[Cost]
  def set(cost: Cost): F[Unit]
  def refund(refund: Cost): F[Unit]
}

object CostAccounting {
  def of[F[_]: Sync](init: Cost): F[CostAccounting[F]] =
    Ref[F].of(init).map(ref => new CostAccountingImpl[F](ref))

  def apply[F[_]](implicit ev: CostAccounting[F]): CostAccounting[F] = ev

  def unsafe[F[_]: Monad](init: Cost)(implicit F: Sync[F]): CostAccounting[F] = {
    val ref = Ref.unsafe[F, Cost](init)
    new CostAccountingImpl[F](ref)
  }

  private class CostAccountingImpl[F[_]](state: Ref[F, Cost])(implicit F: Sync[F])
      extends CostAccounting[F] {
    override def charge(cost: Cost): F[Unit] = chargeInternal(_ - cost)

    private def chargeInternal(f: Cost => Cost): F[Unit] =
      for {
        _ <- failOnOutOfPhlo
        _ <- state.update(f)
        _ <- failOnOutOfPhlo
      } yield ()

    override def get: F[Cost] = state.get
    override def set(cost: Cost): F[Unit] =
      state.set(cost)

    override def refund(refund: Cost): F[Unit] = state.update(_ + refund)
    private val failOnOutOfPhlo: F[Unit] =
      FlatMap[F].ifM(state.get.map(_.value < 0))(F.raiseError(OutOfPhlogistonsError), F.unit)
  }

  implicit def costAccountingMonadState[F[_]: Monad](
      costAccounting: CostAccounting[F]
  ): _cost[F] = new DefaultMonadState[F, Cost] {
    val monad: cats.Monad[F]  = implicitly[Monad[F]]
    def get: F[Cost]          = costAccounting.get
    def set(s: Cost): F[Unit] = costAccounting.set(s)
  }
}
