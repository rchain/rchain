package coop.rchain.rspace.merger

import cats.Monoid
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
import coop.rchain.rspace.history.{HistoryReaderBinary, HistoryRepository}
import coop.rchain.rspace.merger.MergingLogic._
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Serialize
import coop.rchain.shared.syntax._
import fs2.Stream
import scodec.bits.ByteVector

/**
  * Datum changes are referenced by channel, continuation changes are references by consume.
  * In addition, map from consume channels to binary representation of a join in trie have to be maintained.
  * This is because only hashes of channels are available in log event, and computing a join binary to be
  * inserted or removed on merge requires channels before hashing.
  */
final case class StateChange(
    datumsChanges: Map[Blake2b256Hash, ChannelChange[ByteVector]],
    kontChanges: Map[Seq[Blake2b256Hash], ChannelChange[ByteVector]],
    consumeChannelsToJoinSerializedMap: Map[Seq[Blake2b256Hash], ByteVector]
)

object StateChange {

  private def computeValueChange[F[_]: Concurrent](
      historyPointer: Blake2b256Hash,
      startValue: Blake2b256Hash => F[Seq[ByteVector]],
      endValue: Blake2b256Hash => F[Seq[ByteVector]]
  ): F[ChannelChange[ByteVector]] =
    for {
      startValue <- startValue(historyPointer).map(_.toVector)
      endValue   <- endValue(historyPointer).map(_.toVector)
      added      = endValue diff startValue
      deleted    = startValue diff endValue
    } yield ChannelChange(added, deleted)

  def compute[F[_]: Concurrent, C, P, A, K](
      eventLogIndex: EventLogIndex,
      historyRepository: HistoryRepository[F, C, P, A, K],
      preStateHash: Blake2b256Hash,
      postStateHash: Blake2b256Hash
  ): F[StateChange] = {
    val preStateReader  = historyRepository.getHistoryReader(preStateHash).readerBinary
    val postStateReader = historyRepository.getHistoryReader(postStateHash).readerBinary
    for {
      stateChanges <- StateChange[F, C, P, A, K](
                       preStateReader = preStateReader,
                       postStateReader = postStateReader,
                       eventLogIndex,
                       historyRepository.getSerializeC
                     )
    } yield stateChanges
  }

  def apply[F[_]: Concurrent, C, P, A, K](
      preStateReader: HistoryReaderBinary[F, C, P, A, K],
      postStateReader: HistoryReaderBinary[F, C, P, A, K],
      eventLogIndex: EventLogIndex,
      serializeC: Serialize[C]
  ): F[StateChange] =
    for {
      datumsDiffRef <- Ref.of[F, Map[Blake2b256Hash, ChannelChange[ByteVector]]](Map.empty)
      kontsDiffRef  <- Ref.of[F, Map[Seq[Blake2b256Hash], ChannelChange[ByteVector]]](Map.empty)

      // Since event log only contains hashes of channels, so to know which join stored corresponds to channels,
      // this index have to be maintained
      joinsMapRef <- Ref.of[F, Map[Seq[Blake2b256Hash], ByteVector]](Map.empty)

      computeProduceChanges = producesAffected(eventLogIndex)
        .map { _.channelsHash }
        .map { produceChannel =>
          val historyPointer = produceChannel
          computeValueChange(
            historyPointer,
            preStateReader.getData(_).map(_.map(_.raw)),
            postStateReader.getData(_).map(_.map(_.raw))
          ).flatMap { change =>
            datumsDiffRef.update(s => {
              val curVal = s.getOrElse(historyPointer, ChannelChange.empty)
              val newVal = curVal.copy(
                added = curVal.added ++ change.added,
                removed = curVal.removed ++ change.removed
              )
              s.updated(historyPointer, newVal)
            })
          }
        }
      channelsOfConsumesAffected = consumesAffected(eventLogIndex).map(_.channelsHashes)
      computeConsumeChanges = channelsOfConsumesAffected
        .map { consumeChannels =>
          val historyPointer = StableHashProvider.hash(consumeChannels)
          computeValueChange(
            historyPointer,
            preStateReader.getContinuations(_).map(_.map(_.raw)),
            postStateReader.getContinuations(_).map(_.map(_.raw))
          ).flatMap { change =>
            kontsDiffRef.update(s => {
              val curVal = s.getOrElse(consumeChannels, ChannelChange.empty)
              val newVal = curVal.copy(
                added = curVal.added ++ change.added,
                removed = curVal.removed ++ change.removed
              )
              s.updated(consumeChannels, newVal)
            })
          }
        }
      // Find match between channels and serialized join.
      // This step is required because event log contains only hashes of channels, not channels themselves,
      // and to get join value actual channels are required.
      computeJoinsMap = {
        channelsOfConsumesAffected.map { consumeChannels =>
          // the join of interest have to be available for each channel in consume,
          // so its enough to process only one of channels
          val historyPointer = consumeChannels.head
          for {
            pre  <- preStateReader.getJoins(historyPointer).map(_.toVector)
            post <- postStateReader.getJoins(historyPointer).map(_.toVector)
            // find join which match channels
            errMsg = "Tuple space inconsistency found: channel of consume does not contain " +
              "join record corresponding to the consume channels."
            rawJoin <- (pre ++ post)
                        .find { j =>
                          val joinsChannels =
                            j.decoded.toList.map(StableHashProvider.hash(_)(serializeC))
                          // sorting is required because channels of a consume in event log and channels of a join in
                          // history might not be ordered the same way
                          consumeChannels.sorted == joinsChannels.sorted
                        }
                        .map(_.raw)
                        .liftTo(new Exception(errMsg))
            _ <- joinsMapRef.update(_.updated(consumeChannels, rawJoin))
          } yield ()
        }
      }

      // compute all changes
      allChanges = (computeProduceChanges ++ computeConsumeChanges ++ computeJoinsMap)
        .map(Stream.eval)
      _ <- fs2.Stream
            .fromIterator(allChanges.iterator)
            .parJoinProcBounded
            .compile
            .drain
      produceChanges <- datumsDiffRef.get
      _ <- new Exception("State change compute logic error: empty channel change for produce.")
            .raiseError[F, StateChange]
            .whenA(
              produceChanges
                .map { case (_, ChannelChange(add, del)) => add.isEmpty && del.isEmpty }
                .exists(_ == true)
            )
      consumeChanges <- kontsDiffRef.get
      _ <- new Exception("State change compute logic error: empty channel change for consume.")
            .raiseError[F, StateChange]
            .whenA(
              consumeChanges
                .map { case (_, ChannelChange(add, del)) => add.isEmpty && del.isEmpty }
                .exists(_ == true)
            )
      joinsMap <- joinsMapRef.get
    } yield StateChange(produceChanges, consumeChanges, joinsMap)

  def empty: StateChange = StateChange(Map.empty, Map.empty, Map.empty)
  def combine(x: StateChange, y: StateChange): StateChange = {
    def sumMaps[A, B: Monoid](map1: Map[A, B], map2: Map[A, B]): Map[A, B] = map1 ++ map2.map {
      case (k, v) => k -> (v combine map1.getOrElse(k, Monoid[B].empty))
    }
    val newDC = sumMaps(x.datumsChanges, y.datumsChanges)
    val newKC = sumMaps(x.kontChanges, y.kontChanges)
    val newJ  = x.consumeChannelsToJoinSerializedMap ++ y.consumeChannelsToJoinSerializedMap
    StateChange(newDC, newKC, newJ)
  }

  implicit def monoid: Monoid[StateChange] =
    new Monoid[StateChange] {
      def empty: StateChange = StateChange.empty
      def combine(x: StateChange, y: StateChange): StateChange =
        StateChange.combine(x, y)
    }
}
