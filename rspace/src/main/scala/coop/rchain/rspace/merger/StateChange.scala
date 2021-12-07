package coop.rchain.rspace.merger

import cats.Monoid
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.channelStore.ChannelStore
import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
import coop.rchain.rspace.history.HistoryReaderBinary
import coop.rchain.rspace.merger.MergingLogic.{consumesAffected, producesAffected}
import coop.rchain.rspace.serializers.ScodecSerialize.{JoinsB, WaitingContinuationB}
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Serialize
import coop.rchain.shared.syntax._
import fs2.Stream
import scodec.bits.ByteVector

/** Waiting continuation (WK) with channels of consume that put this WK. This class is used to compute joins changes. */
final case class ConsumeChange(body: ByteVector, channels: Seq[Blake2b256Hash])

/** We need this index to compute joins changes base on consume changes. This is because joins store not hashes of
  * channels but channels themselves. So when we have channels from consumes (which are hashes of channels) -
  * we are not able to reconstruct full join ByteVector */
final case class JoinIndex(body: ByteVector, channels: Seq[Blake2b256Hash])

final case class StateChange(
    datumChanges: Map[Blake2b256Hash, ChannelChange[ByteVector]],
    kontChanges: Map[Blake2b256Hash, ChannelChange[ConsumeChange]],
    joinsIndex: Set[JoinIndex]
)

object StateChange {

  private def computeValueChangeOnChannel[F[_]: Sync, T](
      valuePointer: Blake2b256Hash,
      getStartValue: Blake2b256Hash => F[Seq[T]],
      getEndValue: Blake2b256Hash => F[Seq[T]]
  ): F[ChannelChange[T]] =
    for {
      startValue <- getStartValue(valuePointer)
      endValue   <- getEndValue(valuePointer)
      added      = endValue diff startValue
      deleted    = startValue diff endValue
    } yield ChannelChange(added.toVector, deleted.toVector)

  private def recordValueChange[F[_]: Concurrent, T](
      valuePointer: Blake2b256Hash,
      startValueReader: Blake2b256Hash => F[Seq[T]],
      endValueReader: Blake2b256Hash => F[Seq[T]],
      accumulator: Ref[F, Map[Blake2b256Hash, ChannelChange[T]]]
  ): F[Unit] =
    for {
      r <- computeValueChangeOnChannel[F, T](
            valuePointer,
            h => startValueReader(h),
            h => endValueReader(h)
          )
      _ <- accumulator.update(s => {
            val curVal = s.getOrElse(valuePointer, ChannelChange.empty)
            val newVal = curVal.copy(
              added = curVal.added ++ r.added,
              removed = curVal.removed ++ r.removed
            )
            s.updated(valuePointer, newVal)
          })
    } yield ()

  def apply[F[_]: Concurrent, C, P, A, K](
      preStateReader: HistoryReaderBinary[F, C, P, A, K],
      postStateReader: HistoryReaderBinary[F, C, P, A, K],
      eventLogIndex: EventLogIndex,
      channelsStore: ChannelStore[F, C],
      serializeC: Serialize[C]
  ): F[StateChange] =
    for {
      // accumulators for data required to create history actions
      // changes to produces, to consumes and cumulative list of joins
      producesDiffRef <- Ref.of[F, Map[Blake2b256Hash, ChannelChange[ByteVector]]](Map.empty)
      consumesDiffRef <- Ref.of[F, Map[Blake2b256Hash, ChannelChange[ConsumeChange]]](Map.empty)
      joinsIndexRef   <- Ref.of[F, Set[JoinIndex]](Set.empty)

      // TODO remove when channels map is removed
      producePointers <- channelsStore.getProduceMappings(producesAffected(eventLogIndex).toSeq)
      consumePointers <- channelsStore.getConsumeMappings(consumesAffected(eventLogIndex).toSeq)
      joinsPointers <- channelsStore.getJoinMapping(
                        (eventLogIndex.consumesLinearAndPeeks ++ eventLogIndex.consumesPersistent ++ eventLogIndex.consumesProduced).toSeq
                          .flatMap(_.channelsHashes)
                      )

      // `distinct` required here to not compute change for the same hash several times
      // as channel can be mentioned several times in event log
      affectedDatumsValuePointers = producePointers.map(_.historyHash).distinct
      affectedKontsValuePointers  = consumePointers.map(_.historyHash).distinct
      involvedJoinsValuePointers  = joinsPointers.distinct

      datumsChangesComputes = affectedDatumsValuePointers.map(
        datumsPointer =>
          Stream.eval(
            recordValueChange(
              datumsPointer,
              pointer => preStateReader.getData(pointer).map(_.map(_.raw)),
              pointer => postStateReader.getData(pointer).map(_.map(_.raw)),
              producesDiffRef
            )
          )
      )
      kontsChangesComputes = {
        val getKontChangeWithChannels = (k: WaitingContinuationB[P, K]) =>
          ConsumeChange(k.raw, k.decoded.source.channelsHashes)
        affectedKontsValuePointers.map(
          kontsPointer =>
            Stream.eval(
              recordValueChange(
                kontsPointer,
                v => preStateReader.getContinuations(v).map(_.map(getKontChangeWithChannels)),
                v => postStateReader.getContinuations(v).map(_.map(getKontChangeWithChannels)),
                consumesDiffRef
              )
            )
        )
      }
      joinsIndexComputes = {
        implicit val sc: Serialize[C] = serializeC
        val getJoinMappingIndex = (j: JoinsB[C]) =>
          JoinIndex(j.raw, j.decoded.toList.map(channel => StableHashProvider.hash(channel)))
        involvedJoinsValuePointers.map(
          joinsPointer =>
            Stream.eval(for {
              pre  <- preStateReader.getJoins(joinsPointer).map(_.map(getJoinMappingIndex))
              post <- postStateReader.getJoins(joinsPointer).map(_.map(getJoinMappingIndex))
              _    <- joinsIndexRef.update(_ ++ pre ++ post)
            } yield ())
        )
      }
      // compute all changes
      _ <- fs2.Stream
            .emits(datumsChangesComputes ++ kontsChangesComputes ++ joinsIndexComputes)
            .parJoinProcBounded
            .compile
            .drain
      produceChanges <- producesDiffRef.get
      consumeChanges <- consumesDiffRef.get
      joinsTouched   <- joinsIndexRef.get
    } yield StateChange(produceChanges, consumeChanges, joinsTouched)

  def empty: StateChange = StateChange(Map.empty, Map.empty, Set.empty)
  def combine(x: StateChange, y: StateChange): StateChange = {
    def sumMaps[A, B: Monoid](map1: Map[A, B], map2: Map[A, B]): Map[A, B] = map1 ++ map2.map {
      case (k, v) => k -> (v combine map1.getOrElse(k, Monoid[B].empty))
    }
    val newDC = sumMaps(x.datumChanges, y.datumChanges)
    val newKC = sumMaps(x.kontChanges, y.kontChanges)
    val newJI = x.joinsIndex ++ y.joinsIndex
    StateChange(newDC, newKC, newJI)
  }

  implicit def monoid: Monoid[StateChange] =
    new Monoid[StateChange] {
      def empty: StateChange = StateChange.empty
      def combine(x: StateChange, y: StateChange): StateChange =
        StateChange.combine(x, y)
    }
}
