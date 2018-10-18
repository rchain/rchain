package coop.rchain.rholang.interpreter.accounting

import cats.Monoid
import coop.rchain.models.PCost

case class CostAccount(idx: Int, cost: Cost) {
  def +(other: Cost): CostAccount =
    copy(idx + 1, cost + other)
  def +(other: CostAccount): CostAccount =
    copy(idx + other.idx, cost + other.cost)
  def -(other: Cost): CostAccount =
    copy(idx + 1, cost - other)
  def -(other: CostAccount): CostAccount =
    copy(idx + other.idx, cost - other.cost)
}

object CostAccount {
  def apply(value: Long): CostAccount  = CostAccount(0, Cost(value))
  def toProto(c: CostAccount): PCost   = PCost(c.idx, c.cost.value)
  def fromProto(c: PCost): CostAccount = CostAccount(c.iterations, Cost(c.cost))

  implicit val monoidCostAccount: Monoid[CostAccount] = new Monoid[CostAccount] {
    override def empty: CostAccount = CostAccount(0)

    override def combine(x: CostAccount, y: CostAccount): CostAccount = x + y
  }
}
