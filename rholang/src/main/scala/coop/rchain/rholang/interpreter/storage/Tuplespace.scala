package coop.rchain.rholang.interpreter.storage

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Dispatch
import coop.rchain.rspace.pure.PureRSpace
import coop.rchain.rspace.util._

trait Tuplespace[F[_]] {
  def produce(chan: Par, data: ListParWithRandom, persistent: Boolean, sequenceNumber: Int): F[Unit]
  def consume(
      binds: Seq[(BindPattern, Par)],
      body: ParWithRandom,
      persistent: Boolean,
      sequenceNumber: Int
  ): F[Unit]
}

class TuplespaceImpl[F[_], M[_]](
    pureRSpace: PureRSpace[
      F,
      Par,
      BindPattern,
      ListParWithRandom,
      ListParWithRandom,
      TaggedContinuation
    ],
    dispatcher: => Dispatch[F, ListParWithRandom, TaggedContinuation]
)(implicit F: Sync[F], P: Parallel[F, M])
    extends Tuplespace[F] {

  override def produce(
      channel: Par,
      data: ListParWithRandom,
      persistent: Boolean,
      sequenceNumber: Int
  ): F[Unit] = {
    def go: Option[(TaggedContinuation, Seq[ListParWithRandom], Int)] => F[Unit] = {
      case Some((continuation, dataList, updatedSequenceNumber)) =>
        if (persistent)
          List(
            dispatcher.dispatch(continuation, dataList, updatedSequenceNumber),
            produce(channel, data, persistent, sequenceNumber)
          ).parSequence_
        else dispatcher.dispatch(continuation, dataList, updatedSequenceNumber)
      case None => F.unit
    }
    pureRSpace.produce(channel, data, persist = persistent, sequenceNumber) >>= (go(_))
  }

  override def consume(
      binds: Seq[(BindPattern, Par)],
      body: ParWithRandom,
      persistent: Boolean,
      sequenceNumber: Int
  ): F[Unit] = {
    val (patterns: Seq[BindPattern], sources: Seq[Par]) = binds.unzip

    def go: Option[(TaggedContinuation, Seq[ListParWithRandom], Int)] => F[Unit] = {
      case Some((continuation, dataList, updatedSequenceNumber)) =>
        if (persistent)
          List(
            dispatcher.dispatch(continuation, dataList, updatedSequenceNumber),
            consume(binds, body, persistent, sequenceNumber)
          ).parSequence_
        else dispatcher.dispatch(continuation, dataList, updatedSequenceNumber)
      case None => F.unit
    }

    pureRSpace.consume(
      sources.toList,
      patterns.toList,
      TaggedContinuation(ParBody(body)),
      persist = persistent,
      sequenceNumber
    ) >>= (go(_))
  }
}
