package coop.rchain.rholang.interpreter.accounting

case class CostAccount(idx: Int, cost: BigInt) {
  def charge(value: BigInt): CostAccount = copy(idx = this.idx + 1, cost = this.cost + value)
  def +(value: BigInt): CostAccount      = charge(value)
}

object CostAccount {
  def zero: CostAccount = CostAccount(0, BigInt(0))
}
