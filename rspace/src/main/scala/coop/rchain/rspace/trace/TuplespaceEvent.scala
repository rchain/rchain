package coop.rchain.rspace.trace

import coop.rchain.rspace.hashing.Blake2b256Hash

final case class TuplespaceEvent(
    incoming: TuplespaceOperation,
    matched: Option[TuplespaceOperation]
)
final case class TuplespaceOperation(
    polarity: Polarity,
    cardinality: Cardinality,
    eventHash: Blake2b256Hash
)

trait Polarity
case object Send    extends Polarity
case object Receive extends Polarity

trait Cardinality
case object Linear    extends Cardinality
case object NonLinear extends Cardinality
case object Peek      extends Cardinality

object TuplespaceEvent {

  private[this] def toOperation(produce: Produce): TuplespaceOperation =
    TuplespaceOperation(Send, if (produce.persistent) NonLinear else Linear, produce.hash)

  private[this] def toOperation(consume: Consume, peeks: Boolean): TuplespaceOperation =
    TuplespaceOperation(
      Receive,
      if (consume.persistent) NonLinear else if (peeks) Peek else Linear,
      consume.hash
    )

  def from(produce: Produce): (Blake2b256Hash, TuplespaceEvent) =
    produce.channelsHash -> TuplespaceEvent(toOperation(produce), None)

  def from(consume: Consume): Option[(Blake2b256Hash, TuplespaceEvent)] = consume match {
    case Consume(singleChannelHash :: Nil, _, _) =>
      // ???????????????? it is possible that the consume is peek
      Some(singleChannelHash -> TuplespaceEvent(toOperation(consume, false), None))
    case _ => None
  }

  def from(comm: COMM, incomingConsumes: Set[Consume]): Option[(Blake2b256Hash, TuplespaceEvent)] =
    comm match {
      case COMM(consume, produces, peeks, _) if produces.size == 1 => {
        val produce   = produces.head
        val produceOp = toOperation(produce)
        val consumeOp = toOperation(consume, peeks.nonEmpty)

        def peekInitiated = comm.timesRepeated(produce) != 0
        val incoming: TuplespaceOperation =
          if (incomingConsumes.contains(consume) && !peekInitiated) consumeOp
          else produceOp

        val matched: Option[TuplespaceOperation] = Some(
          if (incoming == produceOp) consumeOp
          else produceOp
        )
        Some(produce.channelsHash -> TuplespaceEvent(incoming, matched))
      }
      case _ => None
    }

  implicit class TuplespaceEventOps(val ev: TuplespaceEvent) extends AnyVal {

    def conflicts(other: TuplespaceEvent): Boolean =
      if (ev.incoming.polarity == other.incoming.polarity) {

        val bothPeeks = (ev.incoming.cardinality == Peek) && (other.incoming.cardinality ==
          Peek)

        val bothMatchedSameNonPersistentEvent = for {
          thisMatched  <- ev.matched
          otherMatched <- other.matched
        } yield thisMatched == otherMatched && otherMatched.cardinality != NonLinear

        if (bothPeeks) false
        else bothMatchedSameNonPersistentEvent.getOrElse(false)

      } else ev.unsatisfied && other.unsatisfied

    def unsatisfied: Boolean =
      ev.incoming.cardinality match {
        case Peek      => ev.matched.isEmpty
        case Linear    => ev.matched.forall(_.cardinality == Peek)
        case NonLinear => ev.matched.forall(_.cardinality != NonLinear)
      }

  }
}
