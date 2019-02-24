package coop.rchain.rspace

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext
import cats.effect.{Concurrent, ContextShift}
import cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._
import coop.rchain.shared.Log
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Produce, Log => RSpaceLog, _}
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import kamon._
import scodec.Codec

class ReplayRSpace[F[_], C, P, E, A, R, K](store: IStore[F, C, P, A, K], branch: Branch)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val concurrent: Concurrent[F],
    logF: Log[F],
    contextShift: ContextShift[F],
    scheduler: ExecutionContext
) extends RSpaceOps[F, C, P, E, A, R, K](store, branch)
    with IReplaySpace[F, C, P, E, A, R, K] {

  override protected[this] val logger: Logger = Logger[this.type]

  private[this] val MetricsSource = RSpaceMetricsSource + ".replay"

  private[this] val consumeCommCounter = Kamon.counter(MetricsSource + ".comm.consume")
  private[this] val produceCommCounter = Kamon.counter(MetricsSource + ".comm.produce")

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // TODO remove when Kamon replaced with Metrics API
  def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int
  )(
      implicit m: Match[P, E, A, R]
  ): F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]] =
    contextShift.evalOn(scheduler) {
      if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logF.error(msg) *> syncF.raiseError(new IllegalArgumentException(msg))
      } else
        consumeLockF(channels) {
          lockedConsume(channels, patterns, continuation, persist, sequenceNumber)
        }
    }

  type MaybeConsumeResult = Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // TODO remove when Kamon replaced with Metrics API
  private[this] def lockedConsume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int
  )(
      implicit m: Match[P, E, A, R]
  ): F[MaybeConsumeResult] = {
    def runMatcher(comm: COMM): F[Option[Seq[DataCandidate[C, R]]]] =
      for {
        channelToIndexedDataList <- channels.toList.traverse { c: C =>
                                     store
                                       .withTxnF(store.createTxnReadF()) { txn =>
                                         store.getData(txn, Seq(c)).zipWithIndex.filter {
                                           case (Datum(_, _, source), _) =>
                                             comm.produces.contains(source)
                                         }
                                       }
                                       .map(v => c -> v) // TODO inculde map in traverse?
                                   }
        result = extractDataCandidates(channels.zip(patterns), channelToIndexedDataList.toMap, Nil)
          .flatMap(_.toOption)
          .sequence
      } yield result

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // TODO remove when Kamon replaced with Metrics API
    def storeWaitingContinuation(
        consumeRef: Consume,
        maybeCommRef: Option[COMM]
    ): F[None.type] =
      store
        .withTxnF(store.createTxnWriteF()) { txn =>
          store.putWaitingContinuation(
            txn,
            channels,
            WaitingContinuation(patterns, continuation, persist, consumeRef)
          )
          for (channel <- channels) store.addJoin(txn, channel, channels)
        }
        .map { _ =>
          logger.debug(s"""|consume: no data found,
                         |storing <(patterns, continuation): ($patterns, $continuation)>
                         |at <channels: $channels>""".stripMargin.replace('\n', ' '))
          None
        }

    def handleMatches(
        mats: Seq[DataCandidate[C, R]],
        consumeRef: Consume,
        comms: Multiset[COMM]
    ): F[Option[(ContResult[C, P, K], Seq[Result[R]])]] =
      for {
        _       <- consumeCommCounter.increment().pure[F]
        commRef = COMM(consumeRef, mats.map(_.datum.source))
        _       = assert(comms.contains(commRef), "COMM Event was not contained in the trace")
        r <- mats.toList
              .sortBy(_.datumIndex)(Ordering[Int].reverse)
              .traverse {
                case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                  if (!persistData) {
                    store.withTxnF(store.createTxnWriteF()) { txn =>
                      store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                    }
                  } else ().pure[F]
              }
              .map { _ =>
                logger.debug(
                  s"consume: data found for <patterns: $patterns> at <channels: $channels>"
                )
                removeBindingsFor(commRef)
                val contSequenceNumber = commRef.nextSequenceNumber
                Some(
                  (
                    ContResult(continuation, persist, channels, patterns, contSequenceNumber),
                    mats.map(dc => Result(dc.datum.a, dc.datum.persist))
                  )
                )
              }
      } yield r

//    @tailrec <- todo fix tailrec
    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    // TODO stop throwing exceptions
    def getCommOrDataCandidates(comms: Seq[COMM]): F[Either[COMM, Seq[DataCandidate[C, R]]]] =
      comms match {
        case Nil =>
          val msg = "List comms must not be empty"
          logger.error(msg)
          throw new IllegalArgumentException(msg)
        case commRef :: Nil =>
          runMatcher(commRef).map {
            case Some(x) => Right(x)
            case None    => Left(commRef)
          }
        case commRef :: rem =>
          runMatcher(commRef).flatMap {
            case Some(x) => Right(x).asInstanceOf[Either[COMM, Seq[DataCandidate[C, R]]]].pure[F]
            case None    => getCommOrDataCandidates(rem)
          }
      }

    for {
      _          <- logger.debug(s"""|consume: searching for data matching <patterns: $patterns>
                     |at <channels: $channels>""".stripMargin.replace('\n', ' ')).pure[F]
      consumeRef = Consume.create(channels, patterns, continuation, persist, sequenceNumber)
      r <- replayData.get(consumeRef) match {
            case None =>
              storeWaitingContinuation(consumeRef, None).map(_.asRight[E])
            case Some(comms) =>
              val commOrDataCandidates: F[Either[COMM, Seq[DataCandidate[C, R]]]] =
                getCommOrDataCandidates(comms.iterator().asScala.toList)

              val x: F[MaybeConsumeResult] = commOrDataCandidates flatMap {
                case Left(commRef) =>
                  storeWaitingContinuation(consumeRef, Some(commRef)).map(_.asRight[E])
                case Right(dataCandidates) =>
                  handleMatches(dataCandidates, consumeRef, comms).map(_.asRight[E])
              }
              x
          }
    } yield r
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // TODO remove when Kamon replaced with Metrics API
  def produce(channel: C, data: A, persist: Boolean, sequenceNumber: Int)(
      implicit m: Match[P, E, A, R]
  ): F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]] =
    contextShift.evalOn(scheduler) {
      produceLockF(channel) {
        lockedProduce(channel, data, persist, sequenceNumber)
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // TODO remove when Kamon replaced with Metrics API
  private[this] def lockedProduce(channel: C, data: A, persist: Boolean, sequenceNumber: Int)(
      implicit m: Match[P, E, A, R]
  ): F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]] = {
    //@tailrec <- todo fix tailrec
    def runMatcher(
        comm: COMM,
        produceRef: Produce,
        groupedChannels: Seq[Seq[C]]
    ): F[Option[ProduceCandidate[C, P, R, K]]] =
      groupedChannels match {
        case Nil => None.asInstanceOf[Option[ProduceCandidate[C, P, R, K]]].pure[F]
        case channels :: remaining =>
          for {
            matchCandidates <- store.withTxnF(store.createTxnReadF()) { txn =>
                                store.getWaitingContinuation(txn, channels).zipWithIndex.filter {
                                  case (WaitingContinuation(_, _, _, source), _) =>
                                    comm.consume == source
                                }
                              }
            channelToIndexedDataList <- store.withTxnF(store.createTxnReadF()) { txn =>
                                         channels.map { c: C =>
                                           val as = store.getData(txn, Seq(c)).zipWithIndex.filter {
                                             case (Datum(_, _, source), _) =>
                                               comm.produces.contains(source)
                                           }
                                           c -> {
                                             if (c == channel)
                                               Seq((Datum(data, persist, produceRef), -1))
                                             else as
                                           }
                                         }
                                       }
            result <- extractFirstMatch(channels, matchCandidates, channelToIndexedDataList.toMap) match {
                       case Right(None)             => runMatcher(comm, produceRef, remaining)
                       case Right(produceCandidate) => produceCandidate.pure[F]
                       case Left(_)                 => ???
                     }
          } yield result

      }

    def storeDatum(
        produceRef: Produce,
        maybeCommRef: Option[COMM]
    ): F[None.type] =
      store
        .withTxnF(store.createTxnWriteF()) { txn =>
          store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
        }
        .map { _ =>
          logger.debug(s"""|produce: no matching continuation found
                         |storing <data: $data> at <channel: $channel>""".stripMargin)
          None
        }

    def handleMatch(
        mat: ProduceCandidate[C, P, R, K],
        produceRef: Produce,
        comms: Multiset[COMM]
    ): F[Option[(ContResult[C, P, K], Seq[Result[R]])]] =
      mat match {
        case ProduceCandidate(
            channels,
            WaitingContinuation(patterns, continuation, persistK, consumeRef),
            continuationIndex,
            dataCandidates
            ) =>
          for {
            _       <- syncF.delay { produceCommCounter.increment() }
            commRef = COMM(consumeRef, dataCandidates.map(_.datum.source))
            _       = assert(comms.contains(commRef), "COMM Event was not contained in the trace")
            _ <- if (!persistK) {
                  store.withTxnF(store.createTxnWriteF()) { txn =>
                    store.removeWaitingContinuation(txn, channels, continuationIndex)
                  }
                } else {
                  ().pure[F]
                }
            _ <- dataCandidates.toList
                  .sortBy(_.datumIndex)(Ordering[Int].reverse)
                  .traverse {
                    case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                      store.withTxnF(store.createTxnWriteF()) { txn =>
                        if (!persistData && dataIndex >= 0) {
                          store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                        }
                        store.removeJoin(txn, candidateChannel, channels)
                      }
                  }
            r = {
              logger.debug(s"produce: matching continuation found at <channels: $channels>")
              removeBindingsFor(commRef)
              val contSequenceNumber = commRef.nextSequenceNumber
              Some(
                (
                  ContResult(continuation, persistK, channels, patterns, contSequenceNumber),
                  dataCandidates.map(dc => Result(dc.datum.a, dc.datum.persist))
                )
              )
            }
          } yield r
      }

    for {
      groupedChannels <- store.withTxnF(store.createTxnReadF()) { txn =>
                          store.getJoin(txn, channel)
                        }

      _ = logger.debug(s"""|produce: searching for matching continuations
                       |at <groupedChannels: $groupedChannels>""".stripMargin.replace('\n', ' '))

      produceRef = Produce.create(channel, data, persist, sequenceNumber)

      result <- replayData.get(produceRef) match {
                 case None =>
                   storeDatum(produceRef, None).map(r => Right(r))
                 case Some(comms) =>
                   //@tailrec <-- todo fix tailrec
                   @SuppressWarnings(Array("org.wartremover.warts.Throw"))
                   // TODO stop throwing exceptions
                   def getCommOrProduceCandidate(
                       comms: Seq[COMM]
                   ): F[Either[COMM, ProduceCandidate[C, P, R, K]]] =
                     comms match {
                       case Nil =>
                         val msg = "comms must not be empty"
                         logger.error(msg)
                         throw new IllegalArgumentException(msg)
                       case commRef :: Nil =>
                         runMatcher(commRef, produceRef, groupedChannels) map {
                           case Some(x) => Right(x)
                           case None    => Left(commRef)
                         }
                       case commRef :: rem =>
                         runMatcher(commRef, produceRef, groupedChannels) flatMap {
                           case Some(x) => x.asRight[COMM].pure[F]
                           case None    => getCommOrProduceCandidate(rem)
                         }
                     }
                   val commOrProduceCandidate: F[Either[COMM, ProduceCandidate[C, P, R, K]]] =
                     getCommOrProduceCandidate(comms.iterator().asScala.toList)
                   val r: F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]] = commOrProduceCandidate flatMap {
                     case Left(comm) =>
                       storeDatum(produceRef, Some(comm)).map(r => Right(r))
                     case Right(produceCandidate) =>
                       handleMatch(produceCandidate, produceRef, comms).map(r => Right(r))
                   }
                   r
               }
    } yield result
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  @inline
  private def removeBindingsFor(
      commRef: COMM
  ): Unit =
    commRef.produces.foldLeft(replayData.removeBinding(commRef.consume, commRef)) {
      case (updatedReplays, produceRef) =>
        updatedReplays.removeBinding(produceRef, commRef)
    }

  def createCheckpoint(): F[Checkpoint] =
    for {
      isEmpty <- syncF.delay(replayData.isEmpty)
      checkpoint <- isEmpty.fold(
                     Checkpoint(store.createCheckpoint(), Seq.empty).pure[F], {
                       val msg =
                         s"unused comm event: replayData multimap has ${replayData.size} elements left"
                       logF.error(msg) *> syncF.raiseError[Checkpoint](new ReplayException(msg))
                     }
                   )
    } yield checkpoint

  override def clear(): F[Unit] =
    for {
      _ <- syncF.delay {
            replayData.clear()
          }
      _ <- super.clear()
    } yield ()
}

object ReplayRSpace {

  def create[F[_], C, P, E, A, R, K](context: Context[F, C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      concurrent: Concurrent[F],
      log: Log[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext
  ): F[ReplayRSpace[F, C, P, E, A, R, K]] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val mainStore: IStore[F, C, P, A, K] = context match {
      case lmdbContext: LMDBContext[F, C, P, A, K] =>
        LMDBStore.create[F, C, P, A, K](lmdbContext, branch)

      case memContext: InMemoryContext[F, C, P, A, K] =>
        InMemoryStore.create(memContext.trieStore, branch)

      case mixedContext: MixedContext[F, C, P, A, K] =>
        LockFreeInMemoryStore.create(mixedContext.trieStore, branch)
    }

    val replaySpace = new ReplayRSpace[F, C, P, E, A, R, K](mainStore, branch)

    /*
     * history.initialize returns true if the history trie contains no root (i.e. is empty).
     *
     * In this case, we create a checkpoint for the empty store so that we can reset
     * to the empty store state with the clear method.
     */
    if (history.initialize(mainStore.trieStore, branch)) {
      replaySpace.createCheckpoint().map(_ => replaySpace)
    } else {
      replaySpace.pure[F]
    }
  }

}
