package coop.rchain.rholang.interpreter.accounting

import coop.rchain.rholang.interpreter.accounting.CostAccounting.CostStateRef

trait HasCost[F[_]] {
  def cost: CostStateRef[F]
}
