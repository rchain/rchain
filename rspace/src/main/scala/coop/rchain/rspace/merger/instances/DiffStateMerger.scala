package coop.rchain.rspace.merger.instances

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.rspace._
import coop.rchain.rspace.channelStore.syntax.ConsumeMapping
import coop.rchain.rspace.channelStore.{ChannelStore, DataJoinHash}
import coop.rchain.crypto.Blake2b256Hash
import coop.rchain.rspace.history._
import coop.rchain.rspace.merger.{computeChannelChange, ChannelChange, EventChain, StateMerger}
import coop.rchain.rspace.serializers.ScodecSerialize._
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import coop.rchain.shared.Language._
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Serialize, Stopwatch}
import coop.rchain.store.LazyKeyValueCache

/**
  * State merger using diff of start and enf state to compute changes
  * @param historyRepo repository for histories
  * @param serializeC serizlized for channel (required for joins computations)
  */
final case class DiffStateMerger[F[_]: Concurrent: Log, C, P, A, K](
    historyRepo: HistoryRepository[F, C, P, A, K],
    serializeC: Serialize[C]
) extends StateMerger[F] {

  override def merge(mainState: History[F], toMerge: Seq[EventChain[F]]): F[History[F]] =
    for {
      // compute trie actions
      trieActions <- Stopwatch.time(Log[F].debug(_))(
                      s"compute historyActions: ${toMerge.flatMap(_.events).size} events, " +
                        s"merged ${toMerge.size} states"
                    )(
                      computeTrieActions(
                        mainState.root,
                        historyRepo,
                        toMerge
                      )
                    )

      // apply trie actions
      mergedState <- Stopwatch.time(Log[F].info(_))("process trieActions")(
                      historyRepo.reset(mainState.root).flatMap(_.doCheckpoint(trieActions))
                    )
    } yield mergedState.history

  private def computeTrieActions(
      mainState: Blake2b256Hash,
      channelsStore: ChannelStore[F, C],
      mergings: Seq[EventChain[F]]
  ): F[Seq[HotStoreTrieAction]] = {
    val mainReader = historyRepo.getHistoryReader(mainState).readerBinary

    for {
      // accumulators for data required to create history actions
      // changes to produces, to consumes and cumulative list of joins
      pDiffRef <- Ref.of[F, Map[Blake2b256Hash, ChannelChange[DatumB[A]]]](Map.empty)
      cDiffRef <- Ref
                   .of[F, Map[Blake2b256Hash, ChannelChange[WaitingContinuationB[P, K]]]](Map.empty)
      jAccRef <- Ref.of[F, Map[Blake2b256Hash, Seq[JoinsB[C]]]](Map.empty)

      // main state data readers, used in computation for all mergings => so this is lazy cache
      readProduces = (hash: Blake2b256Hash) => mainReader.getData(hash)
      readConsumes = (hash: Blake2b256Hash) => mainReader.getContinuations(hash)
      readJoin     = (hash: Blake2b256Hash) => mainReader.getJoins(hash)
      mainProduces <- LazyKeyValueCache(readProduces)
      mainConsumes <- LazyKeyValueCache(readConsumes)
      mainJoins    <- LazyKeyValueCache(readJoin)

      // map consume -> channels, used for optimisation
      channelsPerConsumesRef <- Ref.of[F, Map[Blake2b256Hash, Seq[Blake2b256Hash]]](Map.empty)

      // hash in history containing joins for channel
      joinLeafForChannel <- LazyKeyValueCache(
                             (hash: Blake2b256Hash) =>
                               historyRepo
                                 .getChannelHash(hash)
                                 .flatMap {
                                   case Some(DataJoinHash(_, j)) => j.pure[F]
                                   case _ =>
                                     Concurrent[F].raiseError[Blake2b256Hash](
                                       new Exception(
                                         s"hash $hash not found in channel store when requesting join"
                                       )
                                     )
                                 }
                           )

      // each merging can be processed in parallel
      work = fs2.Stream.emits(mergings.map(m => {
        // history reader for end state
        val startStateReader = historyRepo.getHistoryReader(m.startState.root).readerBinary
        val endStateReader   = historyRepo.getHistoryReader(m.endState.root).readerBinary

        // compute channels which are affected in merging
        val (comms, prods, conss) =
          m.events.foldLeft((Seq.empty[COMM], Seq.empty[Produce], Seq.empty[Consume]))(
            (acc, v) =>
              v match {
                case c: COMM => acc.copy(_1 = acc._1 :+ c)
                // `distinct` here is required because when persistent consume or produce created
                // and immediately comm-ed, produce/consume event appears twice
                case p: Produce => acc.copy(_2 = (acc._2 :+ p).distinct)
                case c: Consume => acc.copy(_3 = (acc._3 :+ c).distinct)
              }
          )

        // changes that history actions have to be computed for
        val mergeChanges = {
          // only produces and consumes that are not volatile in the scope of event chain are subject to merge
          // volatile events cannot have any effect outside of the scope
          val newActiveProduces =
            prods.filter(
              p => p.persistent || !comms.exists(c => c.produces.contains(p) && c.peeks.isEmpty)
            )
          val newActiveConsumes = conss.filter(c => c.persistent || !comms.exists(_.consume == c))
          val destroyedExternalProduces =
            comms                                // for all comm events
              .filter(_.peeks.isEmpty)           // peeks won't affect channel state
              .flatMap(_.produces)               // get produces
              .filterNot(p => prods.contains(p)) // that are created outside of this event chain
              .filterNot(_.persistent)           // and not persistent, therefore might be destroyed
          val destroyedExternalConsumes =
            comms                                // for all comm events
              .map(_.consume)                    // get consumes
              .filterNot(c => conss.contains(c)) // that are created outside of this event chain
              .filterNot(c => c.persistent)      // and not persistent, therefore might be destroyed
          (
            newActiveProduces ++ destroyedExternalProduces,
            newActiveConsumes ++ destroyedExternalConsumes
          )
        }

        // processing of a single merging
        fs2.Stream.eval(for {
          // this step is required because hashes in event log are not the same hashes which point to content in history
          produceMappings <- channelsStore.getProduceMappings(mergeChanges._1.toList)
          consumeMappings <- channelsStore.getConsumeMappings(mergeChanges._2.toList)

          _ <- fs2.Stream
                .emits(
                  consumeMappings.map(
                    m =>
                      fs2.Stream.eval(channelsPerConsumesRef.update { s =>
                        s.updated(m.historyHash, m.eventLogHashes)
                      })
                  )
                )
                .parJoinProcBounded
                .compile
                .drain

          // `distinct` required here to not compute for the same hash several times
          // if channel is mentioned several times in event chain
          // hashes storing datums that can potentially be affected
          changedProduceHashes = produceMappings.map(_.historyHash).distinct
          // hashes storing waiting continuations that can potentially be affected
          changedConsumeHashes = consumeMappings.map(_.historyHash).distinct

          // streams for compute data changes
          produceChangeComputes = changedProduceHashes.map(
            dataHash =>
              fs2.Stream.eval(for {
                r <- computeChannelChange[F, DatumB[A]](
                      dataHash,
                      startStateReader.getData,
                      endStateReader.getData
                    )
                // populate main state content store with data on this key
                _ <- mainProduces.get(dataHash)
                _ <- pDiffRef.update(s => {
                      val curVal = s.getOrElse(dataHash, ChannelChange())
                      val newVal = curVal.copy(
                        added = curVal.added ++ r.added,
                        removed = curVal.removed ++ r.removed
                      )
                      s.updated(dataHash, newVal)
                    })

              } yield ())
          )
          // streams for compute Kont changes
          consumeChangeComputes = changedConsumeHashes.map(
            consumeHash =>
              fs2.Stream
                .eval(for {
                  r <- computeChannelChange[F, WaitingContinuationB[P, K]](
                        consumeHash,
                        startStateReader.getContinuations,
                        endStateReader.getContinuations
                      )
                  // populate main state content store with continuations on this key
                  _ <- mainConsumes.get(consumeHash)
                  _ <- cDiffRef.update(s => {
                        val curVal = s.getOrElse(consumeHash, ChannelChange())
                        val newVal = curVal.copy(
                          added = curVal.added ++ r.added,
                          removed = curVal.removed ++ r.removed
                        )
                        s.updated(consumeHash, newVal)
                      })
                } yield ())
          )
          // compute cumulative joins on channel across all states that are merged
          // during accumulation joins can be duplicated
          joinsComputes = consumeMappings
            .flatMap {
              case ConsumeMapping(_, channelsInvolved) => channelsInvolved
            }
            .map {
              channelHash =>
                fs2.Stream
                  .eval(
                    for {
                      joinHash    <- joinLeafForChannel.get(channelHash)
                      joinAtMain  <- mainJoins.get(joinHash)
                      joinAtMerge <- endStateReader.getJoins(joinHash)
                      _ <- jAccRef.update(s => {
                            val curVal = s.getOrElse(joinHash, joinAtMain)
                            val newVal = (curVal ++ joinAtMerge).distinct
                            s.updated(joinHash, newVal)
                          })
                    } yield ()
                  )
            }

          // compute data, continuation changes and cumulative joins
          _ <- Stopwatch.time(Log[F].info(_))(
                s"compute changes: ${produceChangeComputes.size} produces + ${consumeChangeComputes.size} consumes"
              )(
                fs2.Stream
                  .emits(produceChangeComputes ++ consumeChangeComputes ++ joinsComputes)
                  .parJoinProcBounded
                  .compile
                  .drain
              )
        } yield ())
      }))

      // compute all changes
      _ <- work.parJoinProcBounded.compile.drain

      // get all data computed
      initData           <- mainProduces.toMap
      initCons           <- mainConsumes.toMap
      initJoins          <- mainJoins.toMap
      joinForChannelMap  <- joinLeafForChannel.toMap
      produceChanges     <- pDiffRef.get
      consumeChanges     <- cDiffRef.get
      joinsCumulative    <- jAccRef.get
      channelsPerConsume <- channelsPerConsumesRef.get

      // transform changes into actions over trie
      produceActions = produceChanges.toList.par.map {
        case (dataHash, changes) =>
          val newDatumsAtProduce =
            (initData(dataHash) diff changes.removed) ++ changes.added

          if (newDatumsAtProduce.isEmpty) TrieDeleteProduce(dataHash)
          else TrieInsertProduce(dataHash, newDatumsAtProduce.map(_.decoded))
      }
      // TODO we don't need actually check changes that are void. But this breaks logic of actions compute
      consumeActions = consumeChanges.toList.par.map {
        case (consumeHash, changes) =>
          val newKontsAtConsume =
            (initCons(consumeHash) diff changes.removed) ++ changes.added

          if (newKontsAtConsume.isEmpty)
            TrieDeleteConsume(consumeHash)
          else
            TrieInsertConsume(
              consumeHash,
              newKontsAtConsume.map(_.decoded)
            )
      }
      // "joins" is effectively list of consumes that channel participates in
      joinsOptActions = joinsCumulative.toList.par.map {
        case (joinHash, consumes) =>
          val consumesToDelete = {
            consumeActions.map {
              case TrieDeleteConsume(consHash) =>
                if (channelsPerConsume(consHash)
                      .map(joinForChannelMap)
                      .contains(joinHash))
                  consHash.some
                else none[Blake2b256Hash]
              case _ => none[Blake2b256Hash]
            }
          }.collect { case Some(v) => v }

          val mergeResult = consumesToDelete
            .foldLeft(consumes) { (acc, hashToDel) =>
              val idxToDel = acc
                .find { j =>
                  val chansOfRemovedConsume = channelsPerConsume(hashToDel)
                  val join = toOrderedByteVectors(j.decoded)(serializeC)
                    .map(Blake2b256Hash.create)
                  join == chansOfRemovedConsume
                }
                .map(acc.indexOf)

              if (idxToDel.isDefined) removeIndex(acc, idxToDel.get) else acc
            }
            .distinct
            .map(_.decoded)

          if (mergeResult.isEmpty && initJoins.nonEmpty) TrieDeleteJoins(joinHash).some
          else if (initJoins(joinHash).map(_.decoded) != mergeResult)
            TrieInsertJoins(joinHash, mergeResult).some
          else none[HotStoreTrieAction]
      }
      joinsActions = joinsOptActions
        .filter(_.isDefined)
        .map(_.get)

    } yield (produceActions ++ consumeActions ++ joinsActions).toVector
  }
}
