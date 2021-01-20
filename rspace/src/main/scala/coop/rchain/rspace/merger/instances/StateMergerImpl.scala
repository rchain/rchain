package coop.rchain.rspace.merger.instances

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
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
import coop.rchain.rspace.internal.{toOrderedByteVectors, Datum, WaitingContinuation}
import coop.rchain.rspace.merger.{EventChain, StateMerger}
import coop.rchain.rspace.trace.{COMM, Consume, Event, Produce}
import coop.rchain.shared.{Log, Serialize, Stopwatch}
import scodec.Codec

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

  final case class ChannelChanges(
      produceChanges: Vector[Blake2b256Hash],
      consumeChanges: Vector[Seq[Blake2b256Hash]]
  )

  final case class StateMergerImpl[F[_]: Concurrent: Log, C, P, A, K](
      historyRepo: HistoryRepository[F, C, P, A, K],
      history: History[F],
      leafStore: ColdStore[F],
      rootsRepository: RootRepository[F],
      serializeC: Serialize[C]
  )(
      implicit codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K]
  ) extends StateMerger[F] {

    private def computeHistoryActions(
        events: Seq[Event],
        mainStateReader: HistoryRepository[F, C, P, A, K],
        toMerge: Seq[(HistoryRepository[F, C, P, A, K], HistoryRepository[F, C, P, A, K])],
        joinMap: Ref[F, Map[Blake2b256Hash, Seq[Seq[C]]]]
    ): F[List[HistoryAction]] = {

      val changedChannels = {
        val comm = events.collect { case c: COMM => c }
        val produceChanges =
          (events.collect { case p: Produce => p } ++ comm.flatMap(_.produces)).toSet
        //.diff(comm.filter(_.peeks.isEmpty).flatMap(_.produces).toSet)
        val consumeChanges = comm.map(_.consume).toSet ++ events.collect {
          case c: Consume => c
        }
        ChannelChanges(
          produceChanges.map(_.channelsHash).toVector,
          consumeChanges.map(_.channelsHashes).toVector
        )
      }

      // streams for compute data and continuation changes
      val dataChangesComputes = changedChannels.produceChanges.map(
        ch =>
          fs2.Stream.eval(
            computeDataChanges(ch, mainStateReader, toMerge)
          )
      )
      val contChangesComputes = changedChannels.consumeChanges.map(
        ch =>
          fs2.Stream
            .eval(
              computeContinuationChanges(ch, mainStateReader, toMerge, joinMap)
            )
            .filter(_.isDefined)
            .map(_.get)
      )

      for {
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
        jChanges <- computeJoinChanges(joinMap).map(_.toVector)
        changes  = dNcChanges ++ jChanges

        // transform changes into actions on data store and history store
        transformedActions = changes.par.map(transform)

        // apply data actions
        historyActions <- storeLeaves(transformedActions.toList)

      } yield historyActions
    }

    override def merge(
        mainState: Blake2b256Hash,
        toMerge: Seq[EventChain]
    ): F[Blake2b256Hash] =
      for {
        joinMap <- Ref.of[F, Map[Blake2b256Hash, Seq[Seq[C]]]](Map.empty)
        // get all stores
        mainHistory <- historyRepo.reset(mainState)
        mergingHistories <- toMerge.toList.traverse(
                             m =>
                               for {
                                 start  <- historyRepo.reset(m.startState)
                                 finish <- historyRepo.reset(m.endState)
                               } yield (start, finish)
                           )
        allEvents = toMerge.flatMap(_.events)

        historyActions <- Stopwatch.time(Log[F].info(_))(
                           s"compute historyActions: ${allEvents.size} events, merged ${mergingHistories.size} states"
                         )(
                           computeHistoryActions(
                             allEvents,
                             mainHistory,
                             mergingHistories,
                             joinMap
                           )
                         )

        // apply history actions
        mergedState <- Stopwatch.time(Log[F].info(_))("process historyActions")(
                        mainHistory.history.process(historyActions)
                      )

        // commit root to roots store
        _ <- rootsRepository.commit(mergedState.root)
      } yield mergedState.root

    private def computeDataChanges(
        channelHash: Blake2b256Hash,
        mainStateHistory: HistoryHashReader[F, C, P, A, K],
        mergingStatesHistories: Seq[
          (HistoryRepository[F, C, P, A, K], HistoryRepository[F, C, P, A, K])
        ]
    ): F[StateMergeStoreAction] =
      for {
        dataHash <- historyRepo.getChannelHash(channelHash).flatMap {
                     case Some(DataJoinHash(dataHash, _)) => dataHash.pure[F]
                     case _ =>
                       Sync[F].raiseError[Blake2b256Hash](
                         new Exception("not found produce hash in channel store")
                       )
                   }
        dataAtMain <- mainStateHistory.getDataWithBytes(dataHash).map(_.toVector)

        mergeResult <- mergingStatesHistories.toList.foldLeftM(dataAtMain)(
                        (acc, mergingStateHistory) =>
                          for {
                            // This getDataWithBytes is used to make computation faster
                            // using `dataAtMerge diff dataAtBase` compared with `filterNot` using ByteVector key
                            // is about 30% slower
                            dataAtBase <- mergingStateHistory._1
                                           .getDataWithBytes(dataHash)
                                           .map(_.toVector)
                            dataAtMerge <- mergingStateHistory._2
                                            .getDataWithBytes(dataHash)
                                            .map(_.toVector)
                            atMergeAdded = dataAtMerge.filterNot(
                              d => dataAtBase.map(_._2).contains(d._2)
                            )
                            atMergeDeleted = dataAtBase.filterNot(
                              d => dataAtMerge.map(_._2).contains(d._2)
                            )
                            mergeResult = acc.filterNot(
                              d => atMergeDeleted.map(_._2).contains(d._2)
                            ) ++ atMergeAdded
//                            _ = println(
//                              Seq(
//                                dataAtBase.map(_._1).size,
//                                dataAtMain.map(_._1).size,
//                                dataAtMerge.map(_._1).size,
//                                mergeResult.size
//                              ).mkString(";")
//                            )
//                          _ = if (mergeResult.size > 1)
//                            println(
//                              Seq(
//                                dataAtBase.map(_._1),
//                                dataAtMain.map(_._1),
//                                dataAtMerge.map(_._1),
//                                mergeResult.map(_._1)
//                              ).mkString("\n")
//                            )
                          } yield mergeResult
                      )
        action = if (mergeResult.isEmpty) MergeDeleteData(dataHash)
        else MergeInsertData(dataHash, mergeResult.map(_._1))
      } yield action

    private def computeContinuationChanges(
        channelHashes: Seq[Blake2b256Hash],
        mainStateHistory: HistoryHashReader[F, C, P, A, K],
        mergingStatesHistories: Seq[
          (HistoryHashReader[F, C, P, A, K], HistoryHashReader[F, C, P, A, K])
        ],
        joinMap: Ref[F, Map[Blake2b256Hash, Seq[Seq[C]]]]
    ): F[Option[StateMergeStoreAction]] = {

      case class JoinInfo(channelHash: Blake2b256Hash, joinHash: Blake2b256Hash, joins: Seq[Seq[C]])

      def checkJoins(
          channelHash: Blake2b256Hash,
          channelHashes: Seq[Blake2b256Hash],
          shouldRemove: Boolean,
          mergingStateHistory: HistoryHashReader[F, C, P, A, K]
      ): F[Unit] = {
        def removeIndex[E](col: Seq[E], index: Int): Seq[E] = {
          val (l1, l2) = col splitAt index
          (l1 ++ (l2 tail))
        }

        for {
          // get
          joinHash <- historyRepo
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
          mainJoins    <- mainStateHistory.getJoins(joinHash)
          mergingJoins <- mergingStateHistory.getJoins(joinHash)

          err <- joinMap.modify[Boolean] { s =>
                  // get
                  val (s1, joins) = s.get(joinHash) match {
                    case None =>
                      val newVal = (mainJoins ++ mergingJoins).distinct
                      (s.updated(joinHash, newVal), newVal)
                    case Some(joins) =>
                      val newVal = (joins ++ mergingJoins).distinct
                      (s.updated(joinHash, newVal), newVal)
                  }
                  // check
                  val currentJoin = joins.find(
                    j =>
                      toOrderedByteVectors(j)(serializeC)
                        .map(Blake2b256Hash.create) == channelHashes
                  )

                  currentJoin match {
                    case Some(join) =>
                      if (shouldRemove) {
                        val index = joins.indexOf(join)
                        if (index != -1) {
                          val updatedJoins = removeIndex(joins, index)
                          (s1.updated(joinHash, updatedJoins), false)
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
            continuationsAtMain <- mainStateHistory.getContinuationsWithBytes(consumeHash)

            mergeResult <- mergingStatesHistories.toList.foldLeftM(continuationsAtMain)(
                            (acc, mergingStateHistory) =>
                              for {
                                // This getDataWithBytes is used to make coputation faster
                                // using `dataAtMerge diff dataAtBase` compared with `filterNot` using ByteVector key
                                // is about 30% slower
                                continuationsAtBase <- mergingStateHistory._1
                                                        .getContinuationsWithBytes(consumeHash)
                                continuationsAtMerge <- mergingStateHistory._2
                                                         .getContinuationsWithBytes(consumeHash)
                                atMergeAdded = continuationsAtMerge
                                  .filterNot(d => continuationsAtBase.map(_._2).contains(d._2))
                                atMergeRemoved = continuationsAtBase
                                  .filterNot(d => continuationsAtMerge.map(_._2).contains(d._2))
                                mergeResult = acc
                                  .filterNot(d => atMergeRemoved.map(_._2).contains(d._2)) ++ atMergeAdded
                                _ <- channelHashes.toList.traverse_ { channelHash =>
                                      checkJoins(
                                        channelHash,
                                        channelHashes,
                                        mergeResult.isEmpty,
                                        mergingStateHistory._2
                                      )
                                    }
                              } yield mergeResult
                          )

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

    private def computeJoinChanges(
        joinMap: Ref[F, Map[Blake2b256Hash, Seq[Seq[C]]]]
    ): F[Vector[StateMergeStoreAction]] =
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
      val toBeStored = leafs.collect { case (key, Some(data), _) => (key, data) }
      leafStore.put(toBeStored).map(_ => leafs.map(_._3))
    }
  }
}
