package coop.rchain.rspace.merger

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.kernel.Monoid
import cats.syntax.all._
import coop.rchain.rspace.merger.MergingLogic.{combineProducesCopiedByPeek, producesCreated}
import coop.rchain.rspace.trace.{COMM, Consume, Event, Produce}
import coop.rchain.shared.syntax._

import scala.collection.immutable.Set

final case class EventLogIndex(
    producesLinear: Set[Produce],
    producesLinearList: List[Produce],
    producesPersistent: Set[Produce],
    producesConsumed: Set[Produce],
    producesPeeked: Set[Produce],
    producesExistingInPreState: Set[Produce],
    producesTouchingBaseJoins: Set[Produce],
    consumesLinearAndPeeks: Set[Consume],
    consumesPersistent: Set[Consume],
    consumesProduced: Set[Consume]
)

object EventLogIndex {
  def apply[F[_]: Concurrent](
      eventLog: List[Event],
      produceExistsInPreState: Produce => F[Boolean],
      produceTouchPreStateJoin: Produce => F[Boolean]
  ): F[EventLogIndex] =
    for {
      producesLinearRef     <- Ref.of[F, Set[Produce]](Set.empty)
      producesLinearListRef <- Ref.of[F, List[Produce]](List.empty)
      producesPersistentRef <- Ref.of[F, Set[Produce]](Set.empty)

      consumesLinearAndPeeksRef <- Ref.of[F, Set[Consume]](Set.empty)
      consumesPersistentRef     <- Ref.of[F, Set[Consume]](Set.empty)

      producesConsumedRef <- Ref.of[F, Set[Produce]](Set.empty)
      consumesProducedRef <- Ref.of[F, Set[Consume]](Set.empty)

      producesPeekedRef <- Ref.of[F, Set[Produce]](Set.empty)

      producesTouchingJoinsRef      <- Ref.of[F, Set[Produce]](Set.empty)
      producesExistingInPreStateRef <- Ref.of[F, Set[Produce]](Set.empty)

      ps = eventLog.map { e =>
        fs2.Stream.eval(e match {
          case p: Produce =>
            for {
              // As peek is not atomic, when Produce is peaked - exactly the same produce is created in event log.
              // Because of this event log cannot reliably tell whether produce is originated from base state on top
              // of which this event is created, of copied via peek.
              // We can do some tricks to check whether produce is created by a peek, but this logic is not clear yet.
              // So its safer to just read from base for now.
              existsInPreState <- produceExistsInPreState(p)
              // For now all produces touching join are considered as conflicting TODO do analysis to get less conflicts
              touchPreStateJoin <- produceTouchPreStateJoin(p)
              _ <- producesExistingInPreStateRef
                    .update(s => s + p)
                    .whenA(existsInPreState)
              _ <- producesTouchingJoinsRef
                    .update(s => s + p)
                    .whenA(touchPreStateJoin)
              _ <- producesLinearRef
                    .update(s => s + p)
                    .unlessA(p.persistent)
              _ <- producesLinearListRef
                    .update(s => s :+ p)
                    .unlessA(p.persistent)
              _ <- producesPersistentRef
                    .update(s => s + p)
                    .whenA(p.persistent)
            } yield ()
          case c: Consume =>
            for {
              _ <- consumesLinearAndPeeksRef
                    .update(s => s + c)
                    .unlessA(c.persistent)
              _ <- consumesPersistentRef
                    .update(s => s + c)
                    .whenA(c.persistent)
            } yield ()
          case c: COMM => {
            for {
              producesFromBase <- c.produces.toStream.filterA(produceExistsInPreState).map(_.toSet)
              _ <- producesExistingInPreStateRef
                    .update(s => s ++ producesFromBase)
              _ <- producesConsumedRef
                    .update(s => s ++ c.produces)
                    .whenA(c.peeks.isEmpty)
              _ <- producesPeekedRef
                    .update(s => s ++ c.produces)
                    .whenA(c.peeks.nonEmpty)
              _ <- consumesProducedRef
                    .update(s => s + c.consume)
            } yield ()
          }
        })
      }
      _ <- fs2.Stream.emits(ps).parJoinProcBounded.compile.drain

      producesLinear             <- producesLinearRef.get
      producesLinearList         <- producesLinearListRef.get
      producesPersistent         <- producesPersistentRef.get
      producesConsumed           <- producesConsumedRef.get
      producesPeeked             <- producesPeekedRef.get
      consumesLinearAndPeeks     <- consumesLinearAndPeeksRef.get
      consumesPersistent         <- consumesPersistentRef.get
      consumesCommed             <- consumesProducedRef.get
      producesExistingInPreState <- producesExistingInPreStateRef.get
      producesTouchingJoins      <- producesTouchingJoinsRef.get

      // NOTE: this is required due to https://github.com/rchain/rchain/issues/3349#issuecomment-863916745
      //produceConsumedAdjusted = producesConsumed diff (producesExistingInPreState intersect (producesLinear ++ producesPersistent))

    } yield EventLogIndex(
      producesLinear = producesLinear,
      producesLinearList = producesLinearList,
      producesPersistent = producesPersistent,
      producesConsumed = producesConsumed,
      producesPeeked = producesPeeked,
      producesExistingInPreState = producesExistingInPreState,
      producesTouchingBaseJoins = producesTouchingJoins,
      consumesLinearAndPeeks = consumesLinearAndPeeks,
      consumesPersistent = consumesPersistent,
      consumesProduced = consumesCommed
    )

  implicit val monoid: Monoid[EventLogIndex] = new Monoid[EventLogIndex] {
    override def empty: EventLogIndex = EventLogIndex.empty

    override def combine(x: EventLogIndex, y: EventLogIndex): EventLogIndex =
      EventLogIndex.combine(x, y)
  }

  def empty: EventLogIndex =
    EventLogIndex(
      Set.empty,
      List.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      Set.empty
    )

  def combine(x: EventLogIndex, y: EventLogIndex): EventLogIndex = EventLogIndex(
    Seq(x, y).map(_.producesLinear).reduce(_ ++ _),
    Seq(x, y).map(_.producesLinearList).reduce(_ ++ _),
    Seq(x, y).map(_.producesPersistent).reduce(_ ++ _),
    Seq(x, y).map(_.producesConsumed).reduce(_ ++ _),
    Seq(x, y).map(_.producesPeeked).reduce(_ ++ _),
    combineProducesCopiedByPeek(x, y),
    //TODO this joins combination is very restrictive. Join might be originated inside aggregated event log
    Seq(x, y).map(_.producesTouchingBaseJoins).reduce(_ ++ _),
    Seq(x, y).map(_.consumesLinearAndPeeks).reduce(_ ++ _),
    Seq(x, y).map(_.consumesPersistent).reduce(_ ++ _),
    Seq(x, y).map(_.consumesProduced).reduce(_ ++ _)
  )
}
