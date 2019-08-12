package coop.rchain.rspace

import java.lang
import java.nio.file.{Files, Path}

import cats.effect.{Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.temp.par.Par
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace.history.{Branch, HistoryRepository}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace._
import coop.rchain.shared.SyncVarOps._
import coop.rchain.shared.{Cell, Log}
import monix.execution.atomic.AtomicAny
import scodec.Codec

import scala.collection.SortedSet
import scala.concurrent.ExecutionContext
import scala.util.Random

class RSpace[F[_], C, P, A, K] private[rspace] (
    historyRepository: HistoryRepository[F, C, P, A, K],
    storeAtom: AtomicAny[HotStore[F, C, P, A, K]],
    branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val m: Match[F, P, A],
    val concurrent: Concurrent[F],
    logF: Log[F],
    contextShift: ContextShift[F],
    scheduler: ExecutionContext,
    metricsF: Metrics[F],
    val spanF: Span[F]
) extends RSpaceOps[F, C, P, A, K](historyRepository, storeAtom, branch)
    with ISpace[F, C, P, A, K] {

  def store: HotStore[F, C, P, A, K] = storeAtom.get()

  protected[this] override val logger: Logger = Logger[this.type]

  implicit protected[this] lazy val MetricsSource: Source = RSpaceMetricsSource
  private[this] val consumeCommLabel                      = "comm.consume"
  private[this] val produceCommLabel                      = "comm.produce"

  /*
   * Here, we create a cache of the data at each channel as `channelToIndexedData`
   * which is used for finding matches.  When a speculative match is found, we can
   * remove the matching datum from the remaining data candidates in the cache.
   *
   * Put another way, this allows us to speculatively remove matching data without
   * affecting the actual store contents.
   */
  private[this] def fetchChannelToIndexData(channels: Seq[C]): F[Map[C, Seq[(Datum[A], Int)]]] =
    channels
      .traverse { c: C =>
        for {
          data <- store.getData(c)
        } yield c -> Random.shuffle(data.zipWithIndex)
      }
      .map(_.toMap)

  private[this] def storePersistentData(
      dataCandidates: Seq[DataCandidate[C, A]],
      peeks: SortedSet[Int],
      channelsToIndex: Map[C, Int]
  ): F[List[Unit]] = {
    def shouldRemove(persist: Boolean, channel: C): Boolean =
      !persist && !peeks.contains(channelsToIndex(channel))
    dataCandidates.toList
      .sortBy(_.datumIndex)(Ordering[Int].reverse)
      .traverse {
        case DataCandidate(
            candidateChannel,
            Datum(_, persistData, _),
            _,
            dataIndex
            ) if shouldRemove(persistData, candidateChannel) =>
          store.removeDatum(candidateChannel, dataIndex)
        case _ =>
          ().pure[F]
      }
  }

  override def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int,
      peeks: SortedSet[Int] = SortedSet.empty
  ): F[MaybeActionResult] = {

    def wrapResult(
        consumeRef: Consume,
        dataCandidates: Seq[DataCandidate[C, A]]
    ): MaybeActionResult = {
      val contSequenceNumber: Int =
        nextSequenceNumber(consumeRef, dataCandidates)
      Some(
        (
          ContResult(
            continuation,
            persist,
            channels,
            patterns,
            contSequenceNumber,
            peeks.nonEmpty
          ),
          dataCandidates
            .map(dc => Result(dc.datum.a, dc.datum.persist))
        )
      )
    }

    contextShift.evalOn(scheduler) {
      if (channels.isEmpty) {
        val msg = "channels can't be empty"
        logF.error(msg) >> syncF.raiseError(new IllegalArgumentException(msg))
      } else if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logF.error(msg) >> syncF.raiseError(new IllegalArgumentException(msg))
      } else
        for {
          _ <- spanF.mark("before-consume-ref-compute")
          consumeRef <- syncF.delay {
                         Consume.create(channels, patterns, continuation, persist, sequenceNumber)
                       }
          result <- consumeLockF(channels) {
                     for {
                       _ <- spanF.mark("consume-lock-acquired")
                       _ <- logF
                             .debug(
                               s"""|consume: searching for data matching <patterns: $patterns>
                      |at <channels: $channels>""".stripMargin.replace('\n', ' ')
                             )
                       channelToIndexedData <- fetchChannelToIndexData(channels)
                       _                    <- spanF.mark("channel-to-indexed-data-fetched")
                       _ <- syncF.delay {
                             eventLog.update(consumeRef +: _)
                           }
                       _ <- spanF.mark("event-log-updated")
                       options <- extractDataCandidates(
                                   channels.zip(patterns),
                                   channelToIndexedData,
                                   Nil
                                 ).map(_.sequence)
                       _ <- spanF.mark("extract-consume-candidate")
                       result <- options match {
                                  case None =>
                                    storeWaitingContinuation(
                                      channels,
                                      WaitingContinuation(
                                        patterns,
                                        continuation,
                                        persist,
                                        peeks,
                                        consumeRef
                                      )
                                    )
                                  case Some(dataCandidates) =>
                                    for {
                                      _ <- metricsF.incrementCounter(consumeCommLabel)
                                      _ <- syncF.delay {
                                            eventLog.update(
                                              COMM(
                                                consumeRef,
                                                dataCandidates.map(_.datum.source),
                                                peeks
                                              ) +: _
                                            )
                                          }
                                      channelsToIndex = channels.zipWithIndex.toMap
                                      _ <- storePersistentData(
                                            dataCandidates,
                                            peeks,
                                            channelsToIndex
                                          )
                                      _ <- logF.debug(
                                            s"consume: data found for <patterns: $patterns> at <channels: $channels>"
                                          )
                                    } yield wrapResult(consumeRef, dataCandidates)
                                }
                       _ <- spanF.mark("extract-consume-candidate")
                     } yield result

                   }
          _ <- spanF.mark("post-consume-lock")
        } yield result
    }
  }

  private[this] def nextSequenceNumber(
      consumeRef: Consume,
      dataCandidates: Seq[DataCandidate[C, A]]
  ): Int =
    Math.max(
      consumeRef.sequenceNumber,
      dataCandidates.map {
        case DataCandidate(_, Datum(_, _, source), _, _) => source.sequenceNumber
      }.max
    ) + 1

  /*
   * Find produce candidate
   */

  type MaybeProduceCandidate = Option[ProduceCandidate[C, P, A, K]]

  type CandidateChannels = Seq[C]

  private[this] def extractProduceCandidate(
      groupedChannels: Seq[CandidateChannels],
      batChannel: C,
      data: Datum[A]
  ): F[MaybeProduceCandidate] = {

    def go(
        acc: Seq[CandidateChannels]
    ): F[Either[Seq[CandidateChannels], MaybeProduceCandidate]] =
      acc match {
        case Nil =>
          none[ProduceCandidate[C, P, A, K]].asRight[Seq[CandidateChannels]].pure[F]
        case channels :: remaining =>
          for {
            matchCandidates <- for {
                                data <- store.getContinuations(channels)
                              } yield (Random.shuffle(data.zipWithIndex))

            /*
             * Here, we create a cache of the data at each channel as `channelToIndexedData`
             * which is used for finding matches.  When a speculative match is found, we can
             * remove the matching datum from the remaining data candidates in the cache.
             *
             * Put another way, this allows us to speculatively remove matching data without
             * affecting the actual store contents.
             *
             * In this version, we also add the produced data directly to this cache.
             */
            channelToIndexedDataList <- channels.traverse { c: C =>
                                         (for {
                                           data <- store.getData(c)
                                         } yield (Random.shuffle(data.zipWithIndex)))
                                           .map(
                                             as =>
                                               c -> {
                                                 if (c == batChannel)
                                                   (data, -1) +: as
                                                 else as
                                               }
                                           )
                                       }
            firstMatch <- extractFirstMatch(
                           channels,
                           matchCandidates,
                           channelToIndexedDataList.toMap
                         )
          } yield firstMatch match {
            case None             => remaining.asLeft[MaybeProduceCandidate]
            case produceCandidate => produceCandidate.asRight[Seq[CandidateChannels]]
          }
      }
    groupedChannels.tailRecM(go)
  }

  private[this] def processMatchFound(
      pc: ProduceCandidate[C, P, A, K]
  ): F[MaybeActionResult] =
    pc match {
      case ProduceCandidate(
          channels,
          WaitingContinuation(
            patterns,
            continuation,
            persistK,
            peeks,
            consumeRef
          ),
          continuationIndex,
          dataCandidates
          ) =>
        def registerCOMM: F[Unit] =
          syncF.delay {
            eventLog
              .update(
                COMM(consumeRef, dataCandidates.map(_.datum.source), peeks) +: _
              )
          }

        def maybePersistWaitingContinuation: F[Unit] =
          if (!persistK) {
            store.removeContinuation(
              channels,
              continuationIndex
            )
          } else ().pure[F]

        def removeMatchedDatumAndJoin(channelsToIndex: Map[C, Int]): F[Seq[Unit]] =
          dataCandidates
            .sortBy(_.datumIndex)(Ordering[Int].reverse)
            .traverse {
              case DataCandidate(
                  candidateChannel,
                  Datum(_, persistData, _),
                  _,
                  dataIndex
                  ) => {
                def shouldRemove: Boolean = {
                  val idx = channelsToIndex(candidateChannel)
                  dataIndex >= 0 && (!persistData && !peeks.contains(idx))
                }
                (if (shouldRemove) {
                   store.removeDatum(candidateChannel, dataIndex)
                 } else ().pure[F]) >> store.removeJoin(candidateChannel, channels)
              }
            }

        def constructResult: MaybeActionResult = {
          val contSequenceNumber = nextSequenceNumber(consumeRef, dataCandidates)
          Some(
            (
              ContResult[C, P, K](
                continuation,
                persistK,
                channels,
                patterns,
                contSequenceNumber,
                peeks.nonEmpty
              ),
              dataCandidates.map(dc => Result(dc.datum.a, dc.datum.persist))
            )
          )
        }

        for {
          _               <- metricsF.incrementCounter(produceCommLabel)
          _               <- registerCOMM
          _               <- maybePersistWaitingContinuation
          indexedChannels = channels.zipWithIndex.toMap
          _               <- removeMatchedDatumAndJoin(indexedChannels)
          _ <- logF.debug(
                s"produce: matching continuation found at <channels: $channels>"
              )
        } yield constructResult
    }

  private[this] def storeData(
      channel: C,
      data: A,
      persist: Boolean,
      produceRef: Produce
  ): F[MaybeActionResult] =
    for {
      _ <- logF.debug(s"produce: no matching continuation found")
      _ <- store.putDatum(channel, Datum(data, persist, produceRef))
      _ <- logF.debug(s"produce: persisted <data: $data> at <channel: $channel>")
    } yield None

  override def produce(
      channel: C,
      data: A,
      persist: Boolean,
      sequenceNumber: Int
  ): F[MaybeActionResult] =
    contextShift.evalOn(scheduler) {
      for {
        _ <- spanF.mark("before-produce-ref-computed")
        produceRef <- syncF.delay {
                       Produce.create(channel, data, persist, sequenceNumber)
                     }
        _ <- spanF.mark("before-produce-lock")
        result <- produceLockF(channel) {
                   for {
                     _ <- spanF.mark("produce-lock-acquired")
                     //TODO fix double join fetch
                     groupedChannels <- store.getJoins(channel)
                     _               <- spanF.mark("grouped-channels")
                     _ <- logF.debug(
                           s"""|produce: searching for matching continuations
                    |at <groupedChannels: $groupedChannels>""".stripMargin
                             .replace('\n', ' ')
                         )
                     _ <- syncF.delay {
                           eventLog.update(produceRef +: _)
                         }
                     _ <- spanF.mark("event-log-updated")
                     extracted <- extractProduceCandidate(
                                   groupedChannels,
                                   channel,
                                   Datum(data, persist, produceRef)
                                 )
                     _ <- spanF.mark("extract-produce-candidate")
                     r <- extracted match {
                           case Some(pc) =>
                             for {
                               a               <- processMatchFound(pc)
                               indexedChannels = pc.channels.zipWithIndex.toMap
                               _ <- if (pc.continuation.peeks.contains(indexedChannels(channel))) {
                                     storeData(channel, data, persist, produceRef)
                                   } else
                                     ().pure[F]
                             } yield a

                           case None =>
                             storeData(channel, data, persist, produceRef)
                         }
                     _ <- spanF.mark("process-matching")
                   } yield r
                 }
        _ <- spanF.mark("post-produce-lock")
      } yield result
    }

  override def createCheckpoint(): F[Checkpoint] =
    for {
      changes     <- storeAtom.get().changes()
      nextHistory <- historyRepositoryAtom.get().checkpoint(changes.toList)
      _           = historyRepositoryAtom.set(nextHistory)
      _           <- createNewHotStore(nextHistory)(serializeK.toCodec)
      log         = eventLog.take()
      _           = eventLog.put(Seq.empty)
      _           <- restoreInstalls()
    } yield Checkpoint(nextHistory.history.root, log)
}

object RSpace {
  val parallelism = lang.Runtime.getRuntime.availableProcessors() * 2

  def create[F[_], C, P, A, K](
      historyRepository: HistoryRepository[F, C, P, A, K],
      store: HotStore[F, C, P, A, K],
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      concurrent: Concurrent[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F],
      spanF: Span[F]
  ): F[ISpace[F, C, P, A, K]] = {
    val space: ISpace[F, C, P, A, K] =
      new RSpace[F, C, P, A, K](historyRepository, AtomicAny(store), branch)

    space.pure[F]

  }

  def createWithReplay[F[_], C, P, A, K](dataDir: Path, mapSize: Long)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      concurrent: Concurrent[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F],
      spanF: Span[F],
      par: Par[F]
  ): F[(ISpace[F, C, P, A, K], IReplaySpace[F, C, P, A, K])] = {
    val v2Dir = dataDir.resolve("v2")
    for {
      setup                  <- setUp[F, C, P, A, K](v2Dir, mapSize, Branch.MASTER)
      (historyReader, store) = setup
      space                  = new RSpace[F, C, P, A, K](historyReader, AtomicAny(store), Branch.MASTER)
      replayStore            <- inMemoryStore(historyReader)(sk.toCodec, concurrent)
      replay = new ReplayRSpace[F, C, P, A, K](
        historyReader,
        AtomicAny(replayStore),
        Branch.REPLAY
      )
    } yield (space, replay)
  }

  def create[F[_], C, P, A, K](
      dataDir: Path,
      mapSize: Long,
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[F, P, A],
      concurrent: Concurrent[F],
      logF: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext,
      metricsF: Metrics[F],
      spanF: Span[F],
      par: Par[F]
  ): F[ISpace[F, C, P, A, K]] =
    setUp[F, C, P, A, K](dataDir, mapSize, branch).map {
      case (historyReader, store) =>
        new RSpace[F, C, P, A, K](historyReader, AtomicAny(store), branch)
    }

  def setUp[F[_], C, P, A, K](
      dataDir: Path,
      mapSize: Long,
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      concurrent: Concurrent[F],
      par: Par[F]
  ): F[(HistoryRepository[F, C, P, A, K], HotStore[F, C, P, A, K])] = {

    import coop.rchain.rspace.history._
    implicit val cc = sc.toCodec
    implicit val cp = sp.toCodec
    implicit val ca = sa.toCodec
    implicit val ck = sk.toCodec

    val coldStore    = StoreConfig(dataDir.resolve("cold"), mapSize)
    val historyStore = StoreConfig(dataDir.resolve("history"), mapSize)
    val rootsStore   = StoreConfig(dataDir.resolve("roots"), mapSize)
    val config       = LMDBRSpaceStorageConfig(coldStore, historyStore, rootsStore)

    def checkCreateDir(dir: Path): F[Unit] =
      for {
        notexists <- Sync[F].delay(Files.notExists(dir))
        _         <- if (notexists) Sync[F].delay(Files.createDirectories(dir)) else ().pure[F]
      } yield ()

    for {
      _ <- checkCreateDir(coldStore.path)
      _ <- checkCreateDir(historyStore.path)
      _ <- checkCreateDir(rootsStore.path)
      historyReader <- HistoryRepositoryInstances
                        .lmdbRepository[F, C, P, A, K](config)
      store <- inMemoryStore(historyReader)
    } yield (historyReader, store)

  }

  private def inMemoryStore[F[_], C, P, A, K](
      historyReader: HistoryReader[F, C, P, A, K]
  )(implicit ck: Codec[K], sync: Sync[F]) =
    for {
      cache <- Cell.refCell[F, Cache[C, P, A, K]](Cache())
      store = HotStore.inMem(Sync[F], cache, historyReader, ck)
    } yield store
}
