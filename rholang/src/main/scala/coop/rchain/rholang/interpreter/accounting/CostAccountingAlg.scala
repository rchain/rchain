package coop.rchain.rholang.interpreter.accounting

import cats.effect.Sync
import cats.mtl.MonadState
import coop.rchain.rholang.interpreter.accounting

trait CostAccountingAlg[F[_]] {
  def charge(cost: Cost): F[Unit]
  def modify(f: CostAccount => CostAccount): F[Unit]
  def getTotal: F[CostAccount]
  def set(cost: CostAccount): F[Unit]
}

object CostAccountingAlg {
  def monadState[F[_]](state: MonadState[F, CostAccount])(
      implicit F: Sync[F]): CostAccountingAlg[F] =
    new CostAccountingAlg[F] {

      override def modify(f: CostAccount => CostAccount): F[Unit] = F.suspend(state.modify(f))

      override def set(cost: CostAccount): F[Unit] = F.suspend(state.set(cost))

      override def charge(cost: accounting.Cost): F[Unit] = state.modify(_ + cost)

      override def getTotal: F[CostAccount] = F.suspend(state.get)
    }
}
