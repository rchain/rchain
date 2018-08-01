package coop.rchain.rholang.interpreter.accounting

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import coop.rchain.rholang.interpreter.accounting
import cats.implicits._

trait CostAccountingAlg[F[_]] {
  def charge(cost: Cost): F[Unit]
  def modify(f: CostAccount => CostAccount): F[Unit]
  def getCost(): F[CostAccount]
  def setCost(cost: CostAccount): F[Unit]
}

object CostAccountingAlg {
  def apply[F[_]: Sync](init: CostAccount): F[CostAccountingAlg[F]] = Ref[F].of(init).map { state =>
    new CostAccountingAlg[F] {
      override def charge(cost: Cost): F[Unit]                    = state.update(_.charge(cost))
      override def modify(f: CostAccount => CostAccount): F[Unit] = state.update(f)
      override def getCost: F[CostAccount]                        = state.get
      override def setCost(cost: CostAccount): F[Unit]            = state.set(cost)
    }
  }

  def of[F[_]](implicit ev: CostAccountingAlg[F]): CostAccountingAlg[F] = ev

  def unsafe[F[_]: Monad](initialState: CostAccount)(implicit F: Sync[F]): CostAccountingAlg[F] = {
    val state = Ref.unsafe[F, CostAccount](initialState)
    new CostAccountingAlg[F] {

      override def modify(f: CostAccount => CostAccount): F[Unit] =
        F.suspend(state.update(f))

      override def setCost(cost: CostAccount): F[Unit] =
        F.suspend(state.set(cost))

      override def charge(cost: accounting.Cost): F[Unit] =
        F.suspend(state.update(_ + cost))

      override def getCost(): F[CostAccount] = F.suspend(state.get)
    }
  }
}
