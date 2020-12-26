package coop.rchain.rspace.merger.instances

import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.channelStore.instances.ChannelStoreImpl.continuationKey
import coop.rchain.rspace.channelStore.{ContinuationHash, DataJoinHash}
import coop.rchain.rspace.history.HistoryRepositoryImpl.{
  encodeContinuations,
  encodeData,
  encodeJoins
}
import coop.rchain.rspace.history.{
  ColdStore,
  ContinuationsLeaf,
  DataLeaf,
  History,
  HistoryAction,
  HistoryHashReader,
  HistoryRepository,
  JoinsLeaf,
  PersistedData,
  RootRepository,
  DeleteAction => HistoryDeleteAction,
  InsertAction => HistoryInsertAction
}
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.internal.{toOrderedByteVectors, Datum, WaitingContinuation}
import coop.rchain.rspace.merger.StateMerger
import coop.rchain.rspace.trace.Event.IsConflict
import coop.rchain.rspace.trace.{COMM, Consume, Event, Produce}
import coop.rchain.shared.{Log, Serialize, Stopwatch}
import coop.rchain.shared.syntax._
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

  final case class StateMergerImpl[F[_]: Concurrent: Log, C, P, A, K](
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

    override def merge(
        baseState: Blake2b256Hash,
        mainState: Blake2b256Hash,
        mergingState: Blake2b256Hash,
        eventLog: List[Event]
    ): F[Blake2b256Hash] = {

      case class ChannelChanges(
          produceChanges: Vector[Blake2b256Hash],
          consumeChanges: Vector[Seq[Blake2b256Hash]]
      )

      // compute channels that have changes along with changes
      val computeChannelChanges = {
        val comm = eventLog.collect { case c: COMM => c }
        val produceChanges = comm
          .flatMap(_.produces)
          .map(_.channelsHash) ++ eventLog.collect { case p: Produce => p.channelsHash }
        val consumeChanges = comm.map(_.consume.channelsHashes) ++ eventLog.collect {
          case c: Consume => c.channelsHashes
        }
        ChannelChanges(produceChanges.distinct.toVector, consumeChanges.distinct.toVector)
      }

      for {
        // get channels from match store (required due to bug)
        changedChannels <- computeChannelChanges.pure[F]

        // get all stores
        mainHistory    <- historyRepo.reset(mainState)
        baseHistory    <- historyRepo.reset(baseState)
        mergingHistory <- historyRepo.reset(mergingState)

        // streams for compute data and continuation changes
        dataChangesComputes = changedChannels.produceChanges.map(
          ch => fs2.Stream.eval(getDataChanges(ch, mainHistory, baseHistory, mergingHistory))
        )
        contChangesComputes = changedChannels.consumeChanges.map(
          ch =>
            fs2.Stream
              .eval(getContinuationChanges(ch, mainHistory, baseHistory, mergingHistory))
              .filter(_.isDefined)
              .map(_.get)
        )

        // compute data and continuation changes
        dNcChanges <- Stopwatch.time(Log[F].info(_))(
                       s"compute changes: ${dataChangesComputes.size} produces + ${contChangesComputes.size} consumes"
                     )(
                       fs2.Stream
                         .emits(dataChangesComputes ++ contChangesComputes)
                         .parJoinUnbounded
                         .compile
                         .toVector
                     )
        // join changes. NOTE: should be called after continuation changes are computed
        jChanges <- getJoinChanges.map(_.toVector)
        changes  = dNcChanges ++ jChanges

        // transform changes into actions on data store and history store
        transformedActions <- Stopwatch.time(Log[F].info(_))(
                               s"transform ${changes.size} changes"
                             )(
                               changes.par.map(c => transform(c)).pure[F]
                             )

        // apply data actions
        historyActions <- Stopwatch.time(Log[F].info(_))("storeLeaves")(
                           storeLeaves(transformedActions.toList)
                         )

        // apply history actions
        mergedState <- Stopwatch.time(Log[F].info(_))("process historyActions")(
                        mainHistory.history.process(historyActions)
                      )

        // commit root ro roots store
        _ <- rootsRepository.commit(mergedState.root)
      } yield mergedState.root
    }

    private def getDataChanges(
        channelHash: Blake2b256Hash,
        mainStateHistory: HistoryHashReader[F, C, P, A, K],
        baseStateHistory: HistoryHashReader[F, C, P, A, K],
        mergingStateHistory: HistoryHashReader[F, C, P, A, K]
    ): F[StateMergeStoreAction] =
      for {
        dataHash <- historyRepo.getChannelHash(channelHash).flatMap {
                     case Some(DataJoinHash(dataHash, _)) => dataHash.pure[F]
                     case _ =>
                       Sync[F].raiseError[Blake2b256Hash](
                         new Exception("not found produce hash in channel store")
                       )
                   }
        dataAtBase     <- baseStateHistory.getDataWithBytes(dataHash).map(_.toVector)
        dataAtMain     <- mainStateHistory.getDataWithBytes(dataHash).map(_.toVector)
        dataAtMerge    <- mergingStateHistory.getDataWithBytes(dataHash).map(_.toVector)
        atMergeAdded   = dataAtMerge.filterNot(d => dataAtBase.map(_._2).contains(d._2))
        atMergeDeleted = dataAtBase.filterNot(d => dataAtMerge.map(_._2).contains(d._2))
        mergeResult    = dataAtMain.filterNot(d => atMergeDeleted.map(_._2).contains(d._2)) ++ atMergeAdded
        action = if (mergeResult.isEmpty) MergeDeleteData(dataHash)
        else MergeInsertData(dataHash, mergeResult.map(_._1))
      } yield action

    private def getContinuationChanges(
        channelHashes: Seq[Blake2b256Hash],
        mainStateHistory: HistoryHashReader[F, C, P, A, K],
        baseStateHistory: HistoryHashReader[F, C, P, A, K],
        mergingStateHistory: HistoryHashReader[F, C, P, A, K]
    ): F[Option[StateMergeStoreAction]] = {

      case class JoinInfo(channelHash: Blake2b256Hash, joinHash: Blake2b256Hash, joins: Seq[Seq[C]])

      def checkJoins(
          channelHash: Blake2b256Hash,
          channelHashes: Seq[Blake2b256Hash],
          shouldRemove: Boolean
      ) = {
        def removeIndex[E](col: Seq[E], index: Int): Seq[E] = {
          val (l1, l2) = col splitAt index
          (l1 ++ (l2 tail))
        }

        for {
          // get
          hash <- historyRepo
                   .getChannelHash(channelHash)
                   .flatMap {
                     case Some(DataJoinHash(_, j)) => j.pure[F]
                     case _ =>
                       Sync[F].raiseError[Blake2b256Hash](
                         new Exception(
                           s"hash ${channelHash} not found in channel store when requesting join"
                         )
                       )
                   }
          mainJoins    <- mainStateHistory.getJoins(hash)
          mergingJoins <- mergingStateHistory.getJoins(hash)

          err <- joinMap.modify[Boolean] { s =>
                  // get
                  val v = s.get(hash) match {
                    case None =>
                      val unionJoins = (mainJoins ++ mergingJoins).distinct
                      (s.updated(hash, unionJoins), unionJoins)
                    case Some(joins) => (s, joins)
                  }
                  val (s1, joins) = v
                  // check
                  val joinInfo = JoinInfo(channelHash, hash, joins)
                  val currentJoin = joinInfo.joins.find(
                    j =>
                      toOrderedByteVectors(j)(serializeC)
                        .map(Blake2b256Hash.create) == channelHashes
                  )

                  currentJoin match {
                    case Some(join) =>
                      if (shouldRemove) {
                        val index = joinInfo.joins.indexOf(join)
                        if (index != -1) {
                          val updatedJoins = removeIndex(joinInfo.joins, index)
                          (s1.updated(joinInfo.joinHash, updatedJoins), false)
                        } else (s1, false)
                      } else (s1, false)
                    case None =>
                      if (shouldRemove) (s1, false)
                      else (s1, true)
                  }
                }
          _ <- Sync[F]
                .raiseError(
                  new Exception(
                    s"join ${channelHashes} not found in both main and merging"
                  )
                )
                .whenA(err)
        } yield ()
      }

      historyRepo.getChannelHash(continuationKey(channelHashes)).flatMap {
        case Some(ContinuationHash(consumeHash)) =>
          for {
            continuationsAtMerge <- mergingStateHistory.getContinuationsWithBytes(consumeHash)
            continuationsAtBase  <- baseStateHistory.getContinuationsWithBytes(consumeHash)
            continuationsAtMain  <- mainStateHistory.getContinuationsWithBytes(consumeHash)

            atMergeAdded = continuationsAtMerge
              .filterNot(d => continuationsAtBase.map(_._2).contains(d._2))
            atMergeRemoved = continuationsAtBase
              .filterNot(d => continuationsAtMerge.map(_._2).contains(d._2))
            mergeResult = continuationsAtMain
              .filterNot(d => atMergeRemoved.map(_._2).contains(d._2)) ++ atMergeAdded

            _ <- channelHashes.toList.traverse_ { channelHash =>
                  checkJoins(
                    channelHash,
                    channelHashes,
                    mergeResult.isEmpty
                  )
                }

            consumeAction = if (mergeResult.isEmpty)
              Some(MergeDeleteContinuations(consumeHash))
            else
              Some(
                MergeInsertContinuations(
                  consumeHash,
                  mergeResult.map(_._1)
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
    }

    private def getJoinChanges: F[Vector[StateMergeStoreAction]] =
      for {
        m <- joinMap.get
        changes = m.map {
          case (channelHash, joins) =>
            if (joins.isEmpty) MergeDeleteJoins(channelHash)
            else MergeInsertJoins(channelHash, joins)
        }
      } yield changes.toVector

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
      val toBeStored = leafs.par.collect { case (key, Some(data), _) => (key, data) }
      leafStore.put(toBeStored.toList).map(_ => leafs.map(_._3))
    }
  }
}
