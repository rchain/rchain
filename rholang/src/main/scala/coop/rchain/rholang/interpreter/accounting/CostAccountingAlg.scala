package coop.rchain.rholang.interpreter.accounting

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import coop.rchain.rholang.interpreter.accounting

trait CostAccountingAlg[F[_]] {
  def charge(cost: Cost): F[Unit]
  def modify(f: CostAccount => CostAccount): F[Unit]
  def getTotal: F[CostAccount]
  def set(cost: CostAccount): F[Unit]
}

object CostAccountingAlg {
  def apply[F[_]](implicit ev: CostAccountingAlg[F]): CostAccountingAlg[F] = ev

  def unsafe[F[_]: Monad](initialState: CostAccount)(implicit F: Sync[F]): CostAccountingAlg[F] = {
    val state = Ref.unsafe[F, CostAccount](initialState)
    new CostAccountingAlg[F] {

      override def modify(f: CostAccount => CostAccount): F[Unit] =
        F.suspend(state.update(f))

      override def set(cost: CostAccount): F[Unit] =
        F.suspend(state.set(cost))

      override def charge(cost: accounting.Cost): F[Unit] =
        F.suspend(state.update(_ + cost))

      override def getTotal: F[CostAccount] = F.suspend(state.get)
    }
  }
}
