package coop.rchain.rspace.trace

import cats.effect.Sync
import coop.rchain.rspace.StableHashProvider._
import coop.rchain.rspace.internal._
import cats.implicits._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.internal.codecSeq
import coop.rchain.rspace.trace.TuplespaceEvent.TuplespaceEventOps
import coop.rchain.shared.Serialize
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless.syntax.std.tuple.productTupleOps

import scala.collection.SortedSet

/**
  * Broadly speaking, there are two kinds of events in RSpace,
  *
  *   1. [[IOEvent]]s, which are represented as [[Produce]] and [[Consume]]s
  *   2. [[COMM]] Events, which consist of a single [[Consume]] and one or more [[Produce]]s
  */
sealed trait Event

object Event {

  implicit def codecEvent: Codec[Event] =
    discriminated[Event]
      .by(uint2)
      .subcaseP(0) {
        case (comm: COMM) => comm
      }(Codec[COMM])
      .subcaseP(1) {
        case produce: Produce => produce
      }(Codec[Produce])
      .subcaseP(2) {
        case consume: Consume => consume
      }(Codec[Consume])

  implicit def codecLog: Codec[Seq[Event]] = codecSeq[Event](codecEvent)

  /**
    * `Scope` is widely used in descriptions in this file. To better understand `scope` here. I'd like to present
    * some examples to demonstrate that.
    *
    * B2  Rho("for (_ <<- @1) { 0 }")
    * |
    * B1  Rho("@1!(0)")
    *
    * I'd like to use `Volatile` event here to demonstrate the `scope` here.
    *
    * Within the scope which Scope(B2, B3)
    *
    * B2  Rho("for (_ <<- @1) { 0 }") ----+
    * |                                   |---Scope(B2, B1)
    * B1  Rho("@1!(0)")               ----+
    *
    * If you look at within the scope of Scope(B2, B1) like above, all the comm events happened in B1 and B2 are Volatile.
    *
    *
    * B2  Rho("for (_ <<- @1) { 0 }") ---- Scope(B2)
    * |
    * B1  Rho("@1!(0)")
    *
    * But if you look at within only the scope os Scope(B2) like above, the comm and consume events in the B2
    * are not Volatile.
    *
    * `Volatile` means that the produce event and the consume event match within the scope.
    * This produce and consume events within this comm event would not leave any thing left in the
    * tuplespace within the scope.
    */
  def isVolatile(comm: COMM, consumes: Set[Consume], produces: Set[Produce]) =
    !comm.consume.persistent && comm.peeks.isEmpty && consumes.contains(comm.consume) && comm.produces
      .forall(
        produce => !produce.persistent && produces.contains(produce)
      )

  /**
    * @return EventGroup contains all the events which is not volatile in the scope
    */
  def extractRSpaceEventGroup(events: List[Event]): EventGroup = {
    val rspaceEvents = events.toSet
    val (allProduceEvents, allConsumeEvents, allCommEvents) =
      rspaceEvents.foldLeft((Seq.empty[Produce], Seq.empty[Consume], Seq.empty[COMM])) {
        (op, event) =>
          event match {
            case p: Produce => (op._1 :+ p, op._2, op._3)
            case c: Consume => (op._1, op._2 :+ c, op._3)
            case c: COMM    => (op._1, op._2, op._3 :+ c)
          }
      }
    val (volatileCommEvents, nonVolatileCommEvents) = allCommEvents
      .partition(isVolatile(_, allConsumeEvents.toSet, allProduceEvents.toSet))
    val producesInVolatileCommEvents = volatileCommEvents.flatMap(_.produces)
    val consumesInVolatileCommEvents = volatileCommEvents.map(_.consume)
    val produceEvents                = allProduceEvents.filterNot(producesInVolatileCommEvents.contains)
    val consumeEvents                = allConsumeEvents.filterNot(consumesInVolatileCommEvents.contains)
    EventGroup(produceEvents.toSet, consumeEvents.toSet, nonVolatileCommEvents.toSet)
  }

  def extractJoinedChannels(b: EventGroup): Set[Blake2b256Hash] = {

    def joinedChannels(consumes: Set[Consume]) =
      consumes.withFilter(Consume.hasJoins).flatMap(_.channelsHashes)

    joinedChannels(b.consumes) ++ joinedChannels(b.comms.map(_.consume))

  }
  def produceChannels(events: EventGroup) =
    events.produces.map(_.channelsHash).toSet ++ events.comms.flatMap { comm =>
      comm.produces.map(_.channelsHash)
    }.toSet

  def allChannels(events: EventGroup) =
    events.produces.map(_.channelsHash).toSet ++ events.consumes
      .flatMap(_.channelsHashes)
      .toSet ++ events.comms.flatMap { comm =>
      comm.consume.channelsHashes ++ comm.produces.map(_.channelsHash)
    }.toSet

  def tuplespaceEventsPerChannel(
      b: EventGroup
  ): Map[Blake2b256Hash, Set[TuplespaceEvent]] = {
    val nonPersistentProducesInComms = b.comms.flatMap(_.produces).filterNot(_.persistent)
    val nonPersistentConsumesInComms = b.comms.map(_.consume).filterNot(_.persistent)
    val peekedProducesInCommsHashes =
      b.comms.withFilter(_.peeks.nonEmpty).flatMap(_.produces).map(_.hash)

    val freeProduces = b.produces
      .diff(nonPersistentProducesInComms)
      .filterNot(p => peekedProducesInCommsHashes.contains(p.hash) && !p.persistent)

    val freeConsumes = b.consumes.diff(nonPersistentConsumesInComms)

    val produceEvents = freeProduces.map(TuplespaceEvent.from(_))
    val consumeEvents = freeConsumes.flatMap(TuplespaceEvent.from(_))
    val commEvents    = b.comms.flatMap(TuplespaceEvent.from(_, b.consumes))

    (produceEvents ++ consumeEvents ++ commEvents)
      .groupBy(_._1)
      .mapValues[Set[TuplespaceEvent]](_.map(_._2))
  }

  def containConflictingEvents(
      b1Events: EventGroup,
      b2Events: EventGroup
  ): Set[Blake2b256Hash] = {
    def channelConflicts(
        b1Events: Set[TuplespaceEvent],
        b2Events: Set[TuplespaceEvent]
    ): Boolean =
      (for {
        b1  <- b1Events
        b2  <- b2Events
        res = b1.conflicts(b2)
        // TODO: fail fast
      } yield (res)).contains(true)

    val b1Ops = tuplespaceEventsPerChannel(b1Events)
    val b2Ops = tuplespaceEventsPerChannel(b2Events)
    val conflictPerChannel = b1Ops
      .map {
        case (channel, v) =>
          (channel, channelConflicts(v, b2Ops.getOrElse(channel, Set.empty)))
      }
    conflictPerChannel
      .filter { case (_, conflicts) => conflicts }
      .keys
      .toSet
  }

  trait IsConflict
  final case class NonConflict(leftEvents: EventGroup, rightEvents: EventGroup) extends IsConflict
  final case class Conflict(
      leftEvents: EventGroup,
      rightEvents: EventGroup,
      conflictSet: Set[Blake2b256Hash]
  ) extends IsConflict

}

final case class COMM(
    consume: Consume,
    produces: Seq[Produce],
    peeks: SortedSet[Int],
    timesRepeated: Map[Produce, Int]
) extends Event

object COMM {
  def apply[C, A](
      dataCandidates: Seq[ConsumeCandidate[C, A]],
      consumeRef: Consume,
      peeks: SortedSet[Int],
      produceCounters: (Seq[Produce]) => Map[Produce, Int]
  ): COMM = {
    val produceRefs =
      dataCandidates.map(_.datum.source).sortBy(p => (p.channelsHash, p.hash, p.persistent))

    COMM(consumeRef, produceRefs, peeks, produceCounters(produceRefs))
  }
  implicit val codecInt = int32
  implicit val codecCOMM: Codec[COMM] =
    (Codec[Consume] :: Codec[Seq[Produce]] :: sortedSet(uint8) :: Codec[Map[Produce, Int]]).as[COMM]
}

sealed trait IOEvent extends Event

final case class Produce private (
    channelsHash: Blake2b256Hash,
    hash: Blake2b256Hash,
    persistent: Boolean
) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case produce: Produce => produce.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode() * 47

  override def toString: String =
    s"Produce(channels: ${channelsHash.toString}, hash: ${hash.toString})"

}

object Produce {

  def create[C, A](channel: C, datum: A, persistent: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeA: Serialize[A]
  ): Produce =
    new Produce(
      hash(channel)(serializeC),
      hash(channel, datum, persistent),
      persistent
    )

  def createF[F[_]: Sync, C, A](channel: C, datum: A, persistent: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeA: Serialize[A]
  ): F[Produce] = Sync[F].delay(create(channel, datum, persistent))

  def fromHash(
      channelsHash: Blake2b256Hash,
      hash: Blake2b256Hash,
      persistent: Boolean
  ): Produce =
    new Produce(channelsHash, hash, persistent)

  implicit val codecProduce: Codec[Produce] =
    (Codec[Blake2b256Hash] :: Codec[Blake2b256Hash] :: bool).as[Produce]
}

final case class Consume private (
    channelsHashes: Seq[Blake2b256Hash],
    hash: Blake2b256Hash,
    persistent: Boolean
) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case consume: Consume => consume.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode() * 47

  override def toString: String =
    s"Consume(channels: ${channelsHashes.toString}, hash: ${hash.toString}, persistent: $persistent)"
}

final case class EventGroup(produces: Set[Produce], consumes: Set[Consume], comms: Set[COMM]) {
  def events: Seq[Event] = produces.toSeq ++ consumes.toSeq ++ comms.toSeq
}

object Consume {

  def unapply(arg: Consume): Option[(Seq[Blake2b256Hash], Blake2b256Hash, Int)] =
    Some((arg.channelsHashes, arg.hash, 0))

  def create[C, P, K](
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persistent: Boolean
  )(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ): Consume = {
    val channelsByteVectors: Seq[ByteVector] = toOrderedByteVectors(channels)
    new Consume(
      channelsByteVectors.map(Blake2b256Hash.create),
      hash(channelsByteVectors, patterns, continuation, persistent),
      persistent
    )
  }

  def createF[F[_]: Sync, C, P, K](
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persistent: Boolean
  )(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ): F[Consume] = Sync[F].delay(create(channels, patterns, continuation, persistent))

  def fromHash(
      channelsHashes: Seq[Blake2b256Hash],
      hash: Blake2b256Hash,
      persistent: Boolean
  ): Consume =
    new Consume(channelsHashes, hash, persistent)

  implicit val codecConsume: Codec[Consume] =
    (Codec[Seq[Blake2b256Hash]] :: Codec[Blake2b256Hash] :: bool).as[Consume]

  def hasJoins(consume: Consume): Boolean = consume.channelsHashes.size > 1
}
