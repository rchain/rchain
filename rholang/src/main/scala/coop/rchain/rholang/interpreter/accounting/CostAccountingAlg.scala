package coop.rchain.rholang.interpreter.accounting

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.{FlatMap, Monad}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

trait CostAccountingAlg[F[_]] {
  def charge(cost: Cost): F[Unit]
  def charge(cost: CostAccount): F[Unit]
  def get(): F[CostAccount]
  def set(cost: CostAccount): F[Unit]
  def refund(cost: Cost): F[Unit]
}

object CostAccountingAlg {
  def of[F[_]: Sync](init: CostAccount): F[CostAccountingAlg[F]] =
    Ref[F].of(init).map(ref => new CostAccountingAlgImpl[F](ref))

  def apply[F[_]](implicit ev: CostAccountingAlg[F]): CostAccountingAlg[F] = ev

  def unsafe[F[_]: Monad](init: CostAccount)(implicit F: Sync[F]): CostAccountingAlg[F] = {
    val ref = Ref.unsafe[F, CostAccount](init)
    new CostAccountingAlgImpl[F](ref)
  }

  private class CostAccountingAlgImpl[F[_]](state: Ref[F, CostAccount])(implicit F: Sync[F])
      extends CostAccountingAlg[F] {
    override def charge(cost: Cost): F[Unit] = chargeInternal(_ - cost)

    override def charge(cost: CostAccount): F[Unit] = chargeInternal(_ - cost)

    private def chargeInternal(f: CostAccount => CostAccount): F[Unit] =
      for {
        _ <- failOnOutOfPhlo
        _ <- state.update(f)
        _ <- failOnOutOfPhlo
      } yield ()


    override def refund(cost: Cost): F[Unit] = state.update(_ + cost)
    override def get: F[CostAccount] = state.get

    override def set(cost: CostAccount): F[Unit] =
      state.set(cost)

    private val failOnOutOfPhlo: F[Unit] =
      FlatMap[F].ifM(state.get.map(_.cost.value < 0))(F.raiseError(OutOfPhlogistonsError), F.unit)
  }
}
