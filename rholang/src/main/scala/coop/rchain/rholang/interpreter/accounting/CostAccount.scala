package coop.rchain.rholang.interpreter.accounting

case class CostAccount(idx: Int, cost: Cost) {
  def charge(value: Cost): CostAccount = copy(idx = this.idx + 1, cost = this.cost + value)
  def +(cost: Cost): CostAccount       = charge(cost)
}

object CostAccount {
  def zero: CostAccount = CostAccount(0, Cost(BigInt(0)))
}
