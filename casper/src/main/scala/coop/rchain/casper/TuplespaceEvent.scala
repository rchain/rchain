package coop.rchain.casper

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.trace._

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

  private[this] def toOperation(consume: Consume, peek: Boolean): TuplespaceOperation =
    TuplespaceOperation(
      Receive,
      if (consume.persistent) NonLinear else if (peek) Peek else Linear,
      consume.hash
    )

  def from(produce: Produce): (Blake2b256Hash, TuplespaceEvent) =
    produce.channelsHash -> TuplespaceEvent(toOperation(produce), None)

  def from(consume: Consume): Option[(Blake2b256Hash, TuplespaceEvent)] = consume match {
    case Consume(singleChannelHash :: Nil, _, _, _) =>
      Some(singleChannelHash -> TuplespaceEvent(toOperation(consume, false), None))
    case _ => None
  }

  def from(comm: COMM, produces: Set[Produce]): Option[(Blake2b256Hash, TuplespaceEvent)] =
    comm match {
      case COMM(consume, produce :: Nil, peek) => {
        val incoming: TuplespaceOperation =
          if (produces.contains(produce)) toOperation(produce)
          else toOperation(consume, peek.nonEmpty)
        val matched: Option[TuplespaceOperation] = Some(
          if (incoming == toOperation(produce)) toOperation(consume, peek.nonEmpty)
          else toOperation(produce)
        )
        Some(produce.channelsHash -> TuplespaceEvent(incoming, matched))
      }
      case _ => None
    }

  implicit class TuplespaceEventOps(val ev: TuplespaceEvent) extends AnyVal {

    private[casper] def conflicts(other: TuplespaceEvent): Boolean =
      if (ev.incoming.polarity == other.incoming.polarity) {

        val bothPeeks = (ev.incoming.cardinality == Peek) && (other.incoming.cardinality ==
          Peek)

        val bothMatchedSameLinearEvent = for {
          thisMatched  <- ev.matched
          otherMatched <- other.matched
        } yield (thisMatched == otherMatched) && (otherMatched.cardinality != NonLinear)

        if (bothPeeks) {
          // TODO - should always return false
          bothMatchedSameLinearEvent.getOrElse(false)
        } else bothMatchedSameLinearEvent.getOrElse(false)

      } else {
        ev.peeked.map(peeked => TuplespaceEvent(peeked, None).conflicts(other)).getOrElse(false) ||
        other.peeked.map(peeked => TuplespaceEvent(peeked, None).conflicts(ev)).getOrElse(false) ||
        ev.unsatisfied && other.unsatisfied
      }

    private[casper] def peeked: Option[TuplespaceOperation] =
      if (ev.incoming.cardinality == Peek) ev.matched
      else if (ev.matched.fold(false)(_.cardinality == Peek))
        Some(ev.incoming)
      else None

    private[casper] def unsatisfied: Boolean =
      ev.incoming.cardinality match {
        case Linear    => ev.matched.isEmpty
        case Peek      => ev.matched.isEmpty
        case NonLinear => ev.matched.forall(_.cardinality != NonLinear)
      }

  }
}
