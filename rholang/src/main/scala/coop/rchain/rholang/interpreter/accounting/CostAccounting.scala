package coop.rchain.rholang.interpreter.accounting

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.{FlatMap, Monad}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

trait CostAccounting[F[_]] {
  def charge(cost: Cost): F[Unit]
  def get(): F[CostAccount]
  def set(cost: CostAccount): F[Unit]
  def refund(refund: Cost): F[Unit]
}

object CostAccounting {
  def of[F[_]: Sync](init: CostAccount): F[CostAccounting[F]] =
    Ref[F].of(init).map(ref => new CostAccountingImpl[F](ref))

  def apply[F[_]](implicit ev: CostAccounting[F]): CostAccounting[F] = ev

  def unsafe[F[_]: Monad](init: CostAccount)(implicit F: Sync[F]): CostAccounting[F] = {
    val ref = Ref.unsafe[F, CostAccount](init)
    new CostAccountingImpl[F](ref)
  }

  private class CostAccountingImpl[F[_]](state: Ref[F, CostAccount])(implicit F: Sync[F])
      extends CostAccounting[F] {
    override def charge(cost: Cost): F[Unit] = chargeInternal(_ - cost)

    private def chargeInternal(f: CostAccount => CostAccount): F[Unit] =
      for {
        _ <- failOnOutOfPhlo
        _ <- state.update(f)
        _ <- failOnOutOfPhlo
      } yield ()

    override def get: F[CostAccount] = state.get
    override def set(cost: CostAccount): F[Unit] =
      state.set(cost)

    override def refund(refund: Cost): F[Unit] = state.update(_ + refund)
    private val failOnOutOfPhlo: F[Unit] =
      FlatMap[F].ifM(state.get.map(_.cost.value < 0))(F.raiseError(OutOfPhlogistonsError), F.unit)
  }
}
