package coop.rchain.rspace.merger

import coop.rchain.rspace.trace.{Consume, Produce}

final case class EventLogIndex(
    producesLinear: Set[Produce],
    producesPersistent: Set[Produce],
    producesDestroyed: Set[Produce],
    producesPeeked: Set[Produce],
    consumesLinearAndPeeks: Set[Consume],
    consumesPersistent: Set[Consume],
    consumesDestroyed: Set[Consume]
) {
  def +(that: EventLogIndex) =
    EventLogIndex(
      this.producesLinear ++ that.producesLinear,
      this.producesPersistent ++ that.producesPersistent,
      this.producesDestroyed ++ that.producesDestroyed,
      this.producesPeeked ++ that.producesPeeked,
      this.consumesLinearAndPeeks ++ that.consumesLinearAndPeeks,
      this.consumesPersistent ++ that.consumesPersistent,
      this.consumesDestroyed ++ that.consumesDestroyed
    )
}
