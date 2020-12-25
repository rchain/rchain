package coop.rchain.rspace.merger.instances

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.channelStore.instances.ChannelStoreImpl.continuationKey
import coop.rchain.rspace.channelStore.{ChannelStore, ContinuationHash, DataJoinHash}
import coop.rchain.rspace.history.HistoryRepositoryImpl.{
  encodeContinuations,
  encodeData,
  encodeJoins
}
import coop.rchain.rspace.history.{
  DeleteAction => HistoryDeleteAction,
  InsertAction => HistoryInsertAction
}
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.history.{
  ColdStore,
  ContinuationsLeaf,
  DataLeaf,
  History,
  HistoryAction,
  HistoryRepository,
  JoinsLeaf,
  PersistedData,
  RootRepository
}
import coop.rchain.rspace.internal.{toOrderedByteVectors, Datum, WaitingContinuation}
import coop.rchain.rspace.merger.StateMerger
import coop.rchain.rspace.trace.Event.IsConflict
import coop.rchain.rspace.trace.{COMM, Consume, Event, Produce}
import coop.rchain.shared.Serialize
import scodec.Codec

import scala.collection.immutable.Set

object StateMergerImpl {

  sealed trait StateMergeStoreAction
  sealed trait StateMergeInsertAction extends StateMergeStoreAction
  final case class MergeInsertData[A](channel: Blake2b256Hash, data: Seq[Datum[A]])
      extends StateMergeInsertAction
  final case class MergeInsertJoins[C](channel: Blake2b256Hash, joins: Seq[Seq[C]])
      extends StateMergeInsertAction
  final case class MergeInsertContinuations[P, K](
      channels: Blake2b256Hash,
      continuations: Seq[WaitingContinuation[P, K]]
  ) extends StateMergeInsertAction
  sealed trait StateMergeDeleteAction                        extends StateMergeStoreAction
  final case class MergeDeleteData(channel: Blake2b256Hash)  extends StateMergeDeleteAction
  final case class MergeDeleteJoins(channel: Blake2b256Hash) extends StateMergeDeleteAction
  final case class MergeDeleteContinuations(
      channels: Blake2b256Hash
  ) extends StateMergeDeleteAction

  final case class StateMergerImpl[F[_]: Sync, C, P, A, K](
      historyRepo: HistoryRepository[F, C, P, A, K],
      history: History[F],
      leafStore: ColdStore[F],
      rootsRepository: RootRepository[F],
      serializeC: Serialize[C],
      joinMap: Ref[F, Map[Blake2b256Hash, Seq[Seq[C]]]]
  )(
      implicit codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K]
  ) extends StateMerger[F] {

    case class ChannelChanges(
        produceChanges: Set[Blake2b256Hash],
        consumeChanges: Set[Seq[Blake2b256Hash]]
    )
    private def excludeEventsInComm(eventsLogs: List[Event]) = {
      val comm = eventsLogs.collect { case c: COMM => c }
      val produceChanges = comm
        .flatMap(_.produces)
        .map(_.channelsHash)
        .toSet ++ eventsLogs.collect {
        case p: Produce => p.channelsHash
      }.toSet
      val consumeChanges = comm.map(_.consume.channelsHashes).toSet ++ eventsLogs.collect {
        case c: Consume => c.channelsHashes
      }.toSet
      ChannelChanges(produceChanges, consumeChanges)
    }

    case class JoinInfo(channelHash: Blake2b256Hash, joinHash: Blake2b256Hash, joins: Seq[Seq[C]])
    private def getJoins(
        mainState: Blake2b256Hash,
        merginState: Blake2b256Hash,
        channelHash: Blake2b256Hash
    ) =
      for {
        maybeJoinHash <- historyRepo.getChannelHash(channelHash)
        joinHash <- maybeJoinHash match {
                     case Some(DataJoinHash(_, j)) => j.pure[F]
                     case _ =>
                       Sync[F].raiseError[Blake2b256Hash](
                         new Exception(s"not found join hash ${channelHash} in channel store")
                       )
                   }
        m <- joinMap.get
        result <- m.get(joinHash) match {
                   case None =>
                     for {
                       mainJoins    <- historyRepo.getJoins(mainState, joinHash)
                       mergingJoins <- historyRepo.getJoins(merginState, joinHash)
                       unionJoins   = (mainJoins.toSet ++ mergingJoins.toSet).toSeq
                       _            <- joinMap.update(_ + (joinHash -> unionJoins))
                     } yield unionJoins
                   case Some(j) => j.pure[F]
                 }
      } yield JoinInfo(channelHash, joinHash, result)

    private def removeIndex[E](col: Seq[E], index: Int): Seq[E] = {
      val (l1, l2) = col splitAt index
      (l1 ++ (l2 tail))
    }

    private def removeJoins(
        mainState: Blake2b256Hash,
        mergingState: Blake2b256Hash,
        channelHash: Blake2b256Hash,
        join: Seq[C]
    ) =
      for {
        joinInfo <- getJoins(mainState, mergingState, channelHash)
        index    = joinInfo.joins.indexOf(join)
        _ <- if (index != -1) {
              joinMap.update { m =>
                val updatedJoins = removeIndex(joinInfo.joins, index)
                m + (joinInfo.joinHash -> updatedJoins)
              }
            } else Sync[F].unit
      } yield ()

    override def merge(
        baseState: Blake2b256Hash,
        mainState: Blake2b256Hash,
        mergingState: Blake2b256Hash,
        eventLogs: List[Event]
    ): F[Blake2b256Hash] =
      for {
        changedChannels <- excludeEventsInComm(eventLogs).pure[F]
        dataChanges <- changedChannels.produceChanges.toList
                        .traverse(getDataChanges(_, mainState, baseState, mergingState))
        consumeChangesOpt <- changedChannels.consumeChanges.toList
                              .traverse(
                                getContinuationChanges(_, mainState, baseState, mergingState)
                              )
        consumeChanges     = consumeChangesOpt.collect { case Some(c) => c }
        joinChanges        <- getJoinChanges
        changes            = dataChanges ++ consumeChanges ++ joinChanges
        transformedActions = changes.map(transform)
        historyActions     <- storeLeaves(transformedActions)
        h                  = history.reset(mainState)
        mergedState        <- h.process(historyActions)
        _                  <- rootsRepository.commit(mergedState.root)
      } yield mergedState.root

    private def getJoinChanges: F[List[StateMergeStoreAction]] =
      for {
        m <- joinMap.get
        changes = m.map {
          case (channelHash, joins) =>
            if (joins.isEmpty) MergeDeleteJoins(channelHash)
            else MergeInsertJoins(channelHash, joins)
        }
      } yield changes.toList

    private def getDataChanges(
        channelHash: Blake2b256Hash,
        mainState: Blake2b256Hash,
        baseState: Blake2b256Hash,
        mergingState: Blake2b256Hash
    ): F[StateMergeStoreAction] =
      for {
        maybeDataHash <- historyRepo.getChannelHash(channelHash)
        dataHash <- maybeDataHash match {
                     case Some(DataJoinHash(dataHash, _)) => dataHash.pure[F]
                     case _ =>
                       Sync[F].raiseError[Blake2b256Hash](
                         new Exception("not found produce hash in channel store")
                       )
                   }
        dataAtMerge       <- historyRepo.getData(mergingState, dataHash)
        dataAtBase        <- historyRepo.getData(baseState, dataHash)
        dataAtMain        <- historyRepo.getData(mainState, dataHash)
        dataAddAtMerge    = dataAtMerge diff dataAtBase
        dataRemoveAtMerge = dataAtBase diff dataAtMerge
        mergedDataResult  = dataAtMain ++ dataAddAtMerge diff dataRemoveAtMerge
        action = if (mergedDataResult.isEmpty) MergeDeleteData(dataHash)
        else MergeInsertData(dataHash, mergedDataResult)
      } yield action

    private def checkJoins(
        mainState: Blake2b256Hash,
        mergingState: Blake2b256Hash,
        channelHash: Blake2b256Hash,
        channelHashes: Seq[Blake2b256Hash],
        shouldRemove: Boolean
    ) =
      for {
        joinInfo <- getJoins(mainState, mergingState, channelHash)
        currentJoin = joinInfo.joins.find(
          j =>
            toOrderedByteVectors(j)(serializeC)
              .map(Blake2b256Hash.create) == channelHashes
        )
        _ <- currentJoin match {
              case Some(join) =>
                if (shouldRemove) removeJoins(mainState, mergingState, channelHash, join)
                else Sync[F].unit
              case None =>
                if (shouldRemove) Sync[F].unit
                else
                  Sync[F].raiseError(
                    new Exception(
                      s"join ${channelHashes} not found in both main and merging ${joinInfo}"
                    )
                  )
            }
      } yield ()

    private def getContinuationChanges(
        channelHashes: Seq[Blake2b256Hash],
        mainState: Blake2b256Hash,
        baseState: Blake2b256Hash,
        mergingState: Blake2b256Hash
    ): F[Option[StateMergeStoreAction]] =
      for {
        maybeConsumeHash <- historyRepo.getChannelHash(
                             continuationKey(channelHashes)
                           )
        result <- maybeConsumeHash match {
                   case Some(ContinuationHash(consumeHash)) =>
                     for {
                       continuationAtMerge <- historyRepo.getContinuations(
                                               mergingState,
                                               consumeHash
                                             )
                       continuationAtBase <- historyRepo.getContinuations(
                                              baseState,
                                              consumeHash
                                            )
                       continuationAtMain <- historyRepo.getContinuations(
                                              mainState,
                                              consumeHash
                                            )
                       continuationAddAtMerge    = continuationAtMerge diff continuationAtBase
                       continuationRemoveAtMerge = continuationAtBase diff continuationAtMerge
                       mergedContinuationResult  = continuationAtMain ++ continuationAddAtMerge diff continuationRemoveAtMerge
                       _ <- channelHashes.toList.traverse { channelHash =>
                             checkJoins(
                               mainState,
                               mergingState,
                               channelHash,
                               channelHashes,
                               mergedContinuationResult.isEmpty
                             )
                           }
                       consumeAction = if (mergedContinuationResult.isEmpty)
                         Some(MergeDeleteContinuations(consumeHash))
                       else
                         Some(
                           MergeInsertContinuations(
                             consumeHash,
                             mergedContinuationResult
                           )
                         )
                     } yield consumeAction
                   case _ =>
                     // why getting no result from channelStore can result in non-store-actions here
                     // this is related to how and when channelHashes put channelHashes into the channelStore
                     // channelStore store channelHashes when there are changes in [[HistoryRepository]].
                     // If you can not find channelHashes in channelStore, it means that there were no changes
                     // from both mainState and merging State.
                     // The idea behind it is implicit which is very bad for those who read the codes
                     // TODO fix the logic above
                     none[StateMergeStoreAction].pure[F]
                 }
      } yield result

    type Result = (Blake2b256Hash, Option[PersistedData], HistoryAction)

    private def transform(action: StateMergeStoreAction): Result =
      action match {
        case i: MergeInsertData[A] =>
          val data     = encodeData(i.data)
          val dataLeaf = DataLeaf(data)
          val dataHash = Blake2b256Hash.create(data)
          (dataHash, Some(dataLeaf), HistoryInsertAction(i.channel.bytes.toSeq.toList, dataHash))
        case i: MergeInsertContinuations[P, K] =>
          val data              = encodeContinuations(i.continuations)
          val continuationsLeaf = ContinuationsLeaf(data)
          val continuationsHash = Blake2b256Hash.create(data)
          (
            continuationsHash,
            Some(continuationsLeaf),
            HistoryInsertAction(i.channels.bytes.toSeq.toList, continuationsHash)
          )
        case i: MergeInsertJoins[C] =>
          val data      = encodeJoins(i.joins)
          val joinsLeaf = JoinsLeaf(data)
          val joinsHash = Blake2b256Hash.create(data)
          (joinsHash, Some(joinsLeaf), HistoryInsertAction(i.channel.bytes.toSeq.toList, joinsHash))
        case d: MergeDeleteData =>
          (d.channel, None, HistoryDeleteAction(d.channel.bytes.toSeq.toList))
        case d: MergeDeleteContinuations =>
          (d.channels, None, HistoryDeleteAction(d.channels.bytes.toSeq.toList))
        case d: MergeDeleteJoins =>
          (d.channel, None, HistoryDeleteAction(d.channel.bytes.toSeq.toList))
      }

    private def storeLeaves(leafs: List[Result]): F[List[HistoryAction]] = {
      val toBeStored = leafs.collect { case (key, Some(data), _) => (key, data) }
      leafStore.put(toBeStored).map(_ => leafs.map(_._3))
    }

  }
}
