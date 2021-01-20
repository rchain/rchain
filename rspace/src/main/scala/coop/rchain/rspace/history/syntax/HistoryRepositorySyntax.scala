package coop.rchain.rspace.history.syntax

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.channelStore.{ContinuationHash, DataJoinHash}
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.internal.WaitingContinuation
import coop.rchain.rspace.trace.Event
import coop.rchain.rspace.trace.Event.{Conflict, IsConflict, NonConflict, containConflictingEvents, extractJoinedChannels, extractRSpaceEventGroup, produceChannels}
import coop.rchain.rspace.{Blake2b256Hash, StableHashProvider, internal}
import coop.rchain.shared.Serialize

import scala.language.{higherKinds, implicitConversions}

trait HistoryRepositorySyntax {
  implicit final def syntaxHistoryRepository[F[_]: Concurrent, C, P, A, K](
      historyRepo: HistoryRepository[F, C, P, A, K]
  ): HistoryRepositoryOps[F, C, P, A, K] =
    new HistoryRepositoryOps[F, C, P, A, K](historyRepo)
}
final class HistoryRepositoryOps[F[_]: Concurrent, C, P, A, K](
    private val historyRepo: HistoryRepository[F, C, P, A, K]
) {
  def getData(state: Blake2b256Hash, channel: C): F[Seq[internal.Datum[A]]] =
    historyRepo.reset(state) >>= { h =>
      h.getData(channel)
    }

  def getJoins(state: Blake2b256Hash, channel: C): F[Seq[Seq[C]]] =
    historyRepo.reset(state) >>= { h =>
      h.getJoins(channel)
    }

  def getContinuations(
      state: Blake2b256Hash,
      channels: Seq[C]
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    historyRepo.reset(state) >>= { h =>
      h.getContinuations(channels)
    }

  def getData(
      state: Blake2b256Hash,
      dataHash: Blake2b256Hash
  ): F[Seq[internal.Datum[A]]] =
    historyRepo.reset(state) >>= { h =>
      h.getData(dataHash)
    }

  def getJoins(state: Blake2b256Hash, joinHash: Blake2b256Hash): F[Seq[Seq[C]]] =
    historyRepo.reset(state) >>= { h =>
      h.getJoins(joinHash)
    }

  def getContinuations(
      state: Blake2b256Hash,
      continuationHash: Blake2b256Hash
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    historyRepo.reset(state) >>= { h =>
      h.getContinuations(continuationHash)
    }

  def getDataFromChannelHash(
      channelHash: Blake2b256Hash
  ): F[Seq[internal.Datum[A]]] =
    for {
      maybeDataHash <- historyRepo.getChannelHash(channelHash)
      dataHash <- maybeDataHash match {
                   case Some(DataJoinHash(dataHash, _)) => dataHash.pure[F]
                   case _ =>
                     Sync[F].raiseError[Blake2b256Hash](
                       new Exception(s"not found data hash for $channelHash in channel store")
                     )
                 }
      data <- historyRepo.getData(dataHash)
    } yield data

  def getDataFromChannelHash(
      state: Blake2b256Hash,
      channelHash: Blake2b256Hash
  ): F[Seq[internal.Datum[A]]] =
    historyRepo.reset(state) >>= { h =>
      getDataFromChannelHash(channelHash)
    }

  def getJoinsFromChannelHash(
      channelHash: Blake2b256Hash
  ): F[Seq[Seq[C]]] =
    for {
      maybeJoinHash <- historyRepo.getChannelHash(channelHash)
      joinHash <- maybeJoinHash match {
                   case Some(DataJoinHash(_, joinHash)) => joinHash.pure[F]
                   case _ =>
                     Sync[F].raiseError[Blake2b256Hash](
                       new Exception(s"not found join hash for $channelHash in channel store")
                     )
                 }
      data <- historyRepo.getJoins(joinHash)
    } yield data

  def getJoinsFromChannelHash(
      state: Blake2b256Hash,
      channelHash: Blake2b256Hash
  ): F[Seq[Seq[C]]] =
    historyRepo.reset(state) >>= { h =>
      getJoinsFromChannelHash(channelHash)
    }

  def getContinuationFromChannelHash(
      channelHash: Blake2b256Hash
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    for {
      maybeContinuationHash <- historyRepo.getChannelHash(
                                channelHash
                              )
      continuationHash <- maybeContinuationHash match {
                           case Some(ContinuationHash(continuationHash)) => continuationHash.pure[F]
                           case _ =>
                             Sync[F].raiseError[Blake2b256Hash](
                               new Exception(
                                 s"not found continuation hash for $channelHash in channel store"
                               )
                             )
                         }
      continuations <- historyRepo.getContinuations(continuationHash)
    } yield continuations

  def getContinuationFromChannelHash(
      state: Blake2b256Hash,
      channelHash: Blake2b256Hash
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    historyRepo.reset(state) >>= { h =>
      getContinuationFromChannelHash(channelHash)
    }

  /**
    * detect conflict events between rightEvents and rightEvents.
    * In conflict case, conflict set contains the conflict channel hash.
    * @param baseState baseState needed here for detect conflict in joins
    * @return
    */
  def isConflict(
      baseState: Blake2b256Hash,
      leftEvents: List[Event],
      rightEvents: List[Event]
  )(implicit sc: Serialize[C]): F[IsConflict] = {
    val leftEventGroup  = extractRSpaceEventGroup(leftEvents)
    val rightEventGroup = extractRSpaceEventGroup(rightEvents)
    val conflictJoinInLeft =
      extractJoinedChannels(leftEventGroup).intersect(produceChannels(rightEventGroup))
    val conflictJoinInRight =
      extractJoinedChannels(rightEventGroup).intersect(produceChannels(leftEventGroup))
    val otherConflict          = containConflictingEvents(leftEventGroup, rightEventGroup)
    val normalConflictChannels = conflictJoinInLeft ++ conflictJoinInRight ++ otherConflict
    val nonconflictRightProduceChannels =
      rightEventGroup.produces.filter(p => !normalConflictChannels.contains(p.channelsHash))
    for {
      rightJoins <- nonconflictRightProduceChannels.toList.traverse { produce =>
                     for {
                       joins <- getJoinsFromChannelHash(
                                 baseState,
                                 produce.channelsHash
                               )
                       joinM = joins.filter(_.length > 1)
                     } yield (produce, joinM)
                   }
      leftProduceChannel = leftEventGroup.produces.map(_.channelsHash).toSet
      conflictJoinChannels = rightJoins
        .filter {
          case (produce, joins) => {
            val joinsChannelHashes  = joins.map(_.map(StableHashProvider.hash(_)(sc))).flatten
            val joinsWithoutProduce = joinsChannelHashes diff Seq(produce.channelsHash)
            joinsWithoutProduce.exists(p => leftProduceChannel.contains(p))
          }
        }
        .map(_._1.channelsHash)
        .toSet
    } yield
      if (normalConflictChannels.isEmpty && conflictJoinChannels.isEmpty) {
        NonConflict(leftEventGroup, rightEventGroup)
      } else {
        Conflict(
          leftEventGroup,
          rightEventGroup,
          normalConflictChannels ++ conflictJoinChannels
        )
      }
  }
}
