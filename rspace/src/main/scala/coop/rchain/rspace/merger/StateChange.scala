package coop.rchain.rspace.merger

import cats.Monoid
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.channelStore.ChannelStore
import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
import coop.rchain.rspace.history.{HistoryReaderBinary, HistoryReaderRaw}
import coop.rchain.rspace.merger.MergingLogic.{consumesAffected, producesAffected}
import coop.rchain.rspace.serializers.ScodecSerialize.{JoinsB, WaitingContinuationB}
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{HotStoreTrieAction, _}
import coop.rchain.shared.Serialize
import coop.rchain.shared.syntax._
import fs2.Stream
import scodec.bits.ByteVector

// waiting continuation (WK) with channels of consume that put this WK
final case class ConsumeChange(body: ByteVector, channels: Seq[Blake2b256Hash])
// join with channels that are involved in this join
final case class JoinIndex(body: ByteVector, channels: Seq[Blake2b256Hash])

final case class StateChange(
    datumChanges: Map[Blake2b256Hash, ChannelChange[ByteVector]],
    kontChanges: Map[Blake2b256Hash, ChannelChange[ConsumeChange]],
    joinsIndex: Set[JoinIndex]
)

object StateChange {

  def computeChannelChange[F[_]: Sync, T](
      hash: Blake2b256Hash,
      getStartValue: Blake2b256Hash => F[Seq[T]],
      getEndValue: Blake2b256Hash => F[Seq[T]]
  ): F[ChannelChange[T]] =
    for {
      startValue <- getStartValue(hash)
      endValue   <- getEndValue(hash)
      added      = endValue diff startValue
      deleted    = startValue diff endValue
    } yield ChannelChange(added.toVector, deleted.toVector)

  def recordHashValueChange[F[_]: Concurrent, T](
      valuePointerHash: Blake2b256Hash,
      startValueReader: Blake2b256Hash => F[Seq[T]],
      endValueReader: Blake2b256Hash => F[Seq[T]],
      accumulator: Ref[F, Map[Blake2b256Hash, ChannelChange[T]]]
  ): Stream[F, Unit] =
    fs2.Stream.eval(for {
      r <- computeChannelChange[F, T](
            valuePointerHash,
            h => startValueReader(h),
            h => endValueReader(h)
          )
      _ <- accumulator.update(s => {
            val curVal = s.getOrElse(valuePointerHash, ChannelChange.empty)
            val newVal = curVal.copy(
              added = curVal.added ++ r.added,
              removed = curVal.removed ++ r.removed
            )
            s.updated(valuePointerHash, newVal)
          })
    } yield ())

  def computeStateChange[F[_]: Concurrent, C, P, A, K](
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

      // `distinct` required here to not compute for the same hash several times
      // if channel is mentioned several times in event chain
      // hashes storing datums that can potentially be affected
      changedProduceHashes = producePointers.map(_.historyHash).distinct
      // hashes storing waiting continuations that can potentially be affected
      changedConsumeHashes = consumePointers.map(_.historyHash).distinct
      joinsInvolved        = joinsPointers.distinct

      produceChangeComputes = changedProduceHashes.map(
        dataHash =>
          recordHashValueChange(
            dataHash,
            pointer => preStateReader.getData(pointer).map(_.map(_.raw)),
            pointer => postStateReader.getData(pointer).map(_.map(_.raw)),
            producesDiffRef
          )
      )
      consumeChangeComputes = {
        val getKontChangeWithChannels = (k: WaitingContinuationB[P, K]) =>
          ConsumeChange(k.raw, k.decoded.source.channelsHashes)
        changedConsumeHashes.map(
          consumeHash =>
            recordHashValueChange(
              consumeHash,
              v => preStateReader.getContinuations(v).map(_.map(getKontChangeWithChannels)),
              v => postStateReader.getContinuations(v).map(_.map(getKontChangeWithChannels)),
              consumesDiffRef
            )
        )
      }
      joinsIndexComputes = {
        implicit val sc: Serialize[C] = serializeC
        val getJoinsChangeWithChannels = (j: JoinsB[C]) =>
          JoinIndex(j.raw, j.decoded.toList.map(channel => StableHashProvider.hash(channel)))
        joinsInvolved.map(
          joinHash =>
            Stream.eval(for {
              pre  <- preStateReader.getJoins(joinHash).map(_.map(getJoinsChangeWithChannels))
              post <- postStateReader.getJoins(joinHash).map(_.map(getJoinsChangeWithChannels))
              _    <- joinsIndexRef.update(_ ++ pre ++ post)
            } yield ())
        )
      }

      // compute all changes
      _ <- fs2.Stream
            .emits(produceChangeComputes ++ consumeChangeComputes ++ joinsIndexComputes)
            .parJoinProcBounded
            .compile
            .drain
      produceChanges <- producesDiffRef.get
      consumeChanges <- consumesDiffRef.get
      joinChanges    <- joinsIndexRef.get
    } yield StateChange(produceChanges, consumeChanges, joinChanges)

  def computeTrieActions[F[_]: Concurrent](
      baseReader: HistoryReaderRaw[F],
      joinsPointerForChannel: Blake2b256Hash => F[Blake2b256Hash],
      changes: StateChange
  ): F[List[HotStoreTrieAction]] = {
    val produceActionsCompute: Stream[F, HotStoreTrieAction] = Stream
      .emits(changes.datumChanges.toVector)
      .parEvalMapProcBounded {
        case (valuePointerHash, changes) =>
          for {
            init <- baseReader.getData(valuePointerHash)
            r = (init diff changes.removed) ++ changes.added match {
              case Seq()  => TrieDeleteProduce(valuePointerHash)
              case newVal => TrieInsertBinaryProduce(valuePointerHash, newVal)
            }
          } yield r
      }

    final case class ConsumeActionsWithChannels(
        acton: HotStoreTrieAction,
        consumesChannels: Seq[Seq[Blake2b256Hash]]
    )
    val consumeActionsCompute: Stream[F, ConsumeActionsWithChannels] = Stream
      .emits(changes.kontChanges.toVector)
      .parEvalMapProcBounded {
        case (valuePointerHash, ChannelChange(added, removed)) =>
          for {
            init <- baseReader.getContinuations(valuePointerHash)
            r = (init diff removed.map(_.body)) ++ added.map(_.body) match {
              case Seq() =>
                ConsumeActionsWithChannels(
                  TrieDeleteConsume(valuePointerHash),
                  removed.map(_.channels)
                )
              case newVal => //if newVal != init
                ConsumeActionsWithChannels(
                  TrieInsertBinaryConsume(valuePointerHash, newVal),
                  added.map(_.channels)
                )
            }
          } yield r
      }

    for {
      produceTrieActions <- produceActionsCompute.compile.toList
      consumeTrieActions <- consumeActionsCompute.compile.toList

      channelsOfConsumesRemoved = consumeTrieActions.collect {
        case ConsumeActionsWithChannels(TrieDeleteConsume(_), channelsOfRemovedConsumes) =>
          channelsOfRemovedConsumes
      }.flatten
      channelsOfConsumesAdded = consumeTrieActions.collect {
        case ConsumeActionsWithChannels(TrieInsertBinaryConsume(_, _), chanelsOfAddedConsumes) =>
          chanelsOfAddedConsumes
      }.flatten

      // this is populated on indexing to find match between seq of channels and join binary value
      joinsChannelsToBodyMap = changes.joinsIndex.map {
        case JoinIndex(body, channels) => (channels -> body)
      }.toMap

      // for all channels in consumes affected - load joins values from base state
      initJoins <- (channelsOfConsumesRemoved ++ channelsOfConsumesAdded).flatten.distinct
                    .traverse { channel =>
                      for {
                        joinsPointer <- joinsPointerForChannel(channel)
                        joins        <- baseReader.getJoins(joinsPointer)

                      } yield (channel, joins)
                    }
                    .map(_.toMap)

      withAdded <- channelsOfConsumesAdded.foldLeftM[F, Map[Blake2b256Hash, Seq[ByteVector]]](
                    initJoins
                  ) {
                    case (acc, channels) =>
                      // join binary value that corresponds to channels
                      val joinBodyOpt = joinsChannelsToBodyMap.get(channels)
                      joinBodyOpt match {
                        case Some(joinBody) =>
                          for {
                            joinPointers <- channels.toList.traverse(joinsPointerForChannel)
                            // add new join for all channels participating in join
                            next = joinPointers.foldLeft(acc)((a, jp) => {
                              val cur = a.getOrElse(jp, Seq.empty)
                              a.updated(jp, (cur.toSet + joinBody).toSeq)
                            })
                          } yield next
                        // this clause means that consume adding/deleting did not influence joins
                        case None => acc.pure
                      }

                  }
      withRemoved <- channelsOfConsumesRemoved.foldLeftM[F, Map[Blake2b256Hash, Seq[ByteVector]]](
                      withAdded
                    ) {
                      case (acc, channels) =>
                        // join binary value that corresponds to channels
                        val joinBodyOpt = joinsChannelsToBodyMap.get(channels)
                        joinBodyOpt match {
                          case Some(joinBody) =>
                            for {
                              joinPointers <- channels.toList.traverse(joinsPointerForChannel)
                              // add new join for all channels participating in join
                              next = joinPointers.foldLeft(acc)((a, jp) => {
                                val cur = a.getOrElse(jp, Seq.empty)
                                a.updated(jp, (cur.toSet - joinBody).toSeq)
                              })
                            } yield next
                          // this clause means that consume adding/deleting did not influence joins
                          case None => acc.pure
                        }
                    }
      newJoins = withRemoved
      joinsActions = newJoins.map {
        case (joinPointer, joins) =>
          joins match {
            case Seq() => TrieDeleteJoins(joinPointer)
            // Joins must be sorted to produce replayable root hash
            case newVal => TrieInsertBinaryJoins(joinPointer, newVal.sorted(util.ordByteVector))
          }
      }

    } yield produceTrieActions ++ consumeTrieActions.map(_.acton) ++ joinsActions
  }

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
