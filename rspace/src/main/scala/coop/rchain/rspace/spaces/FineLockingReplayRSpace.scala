package coop.rchain.rspace.spaces

import coop.rchain.rspace._
import cats.implicits._
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.history.{Branch, ITrieStore, InMemoryTrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Produce, _}
import coop.rchain.shared.SyncVarOps._
import scodec.Codec

import scala.Function.const
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar
import scala.util.Random
import kamon._

class FineLockingReplayRSpace[C, P, E, A, R, K](store: IStore[C, P, A, K], branch: Branch)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K]
) extends FineLockingRSpaceOps[C, P, E, A, R, K](store, branch)
    with IReplaySpace[cats.Id, C, P, E, A, R, K] {

  override protected[this] val logger: Logger = Logger[this.type]

  private[this] val consumeCommCounter = Kamon.counter("replayrspace.comm.consume")
  private[this] val produceCommCounter = Kamon.counter("replayrspace.comm.produce")

  private[this] val consumeSpan   = Kamon.buildSpan("replayrspace.consume")
  private[this] val produceSpan   = Kamon.buildSpan("replayrspace.produce")
  protected[this] val installSpan = Kamon.buildSpan("replayrspace.install")

  def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, E, A, R]): Either[E, Option[(K, Seq[R])]] =
    Kamon.withSpan(consumeSpan.start(), finishSpan = true) {
      if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logger.error(msg)
        throw new IllegalArgumentException(msg)
      }
      consumeLock(channels) {
        def runMatcher(comm: COMM): Option[Seq[DataCandidate[C, R]]] = {
          val channelToIndexedData = channels.map { (c: C) =>
            c -> {
              store.withTxn(store.createTxnRead()) { txn =>
                store.getData(txn, Seq(c)).zipWithIndex.filter {
                  case (Datum(_, _, source), _) => comm.produces.contains(source)
                }
              }
            }
          }.toMap
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil)
            .flatMap(_.toOption)
            .sequence
        }

        def storeWaitingContinuation(replays: ReplayData,
                                     consumeRef: Consume,
                                     maybeCommRef: Option[COMM]): None.type = {
          store.withTxn(store.createTxnRead()) { txn =>
            store.putWaitingContinuation(
              txn,
              channels,
              WaitingContinuation(patterns, continuation, persist, consumeRef))
          }
          store.withTxn(store.createTxnWrite()) { txn =>
            for (channel <- channels) store.addJoin(txn, channel, channels)
          }
          logger.debug(s"""|consume: no data found,
                           |storing <(patterns, continuation): ($patterns, $continuation)>
                           |at <channels: $channels>""".stripMargin.replace('\n', ' '))
          replayData.put(replays)
          None
        }

        def handleMatches(mats: Seq[DataCandidate[C, R]],
                          replays: ReplayData,
                          consumeRef: Consume,
                          comms: Multiset[COMM]): Option[(K, Seq[R])] = {
          consumeCommCounter.increment()
          val commRef = COMM(consumeRef, mats.map(_.datum.source))
          assert(comms.contains(commRef), "COMM Event was not contained in the trace")
          mats
            .sortBy(_.datumIndex)(Ordering[Int].reverse)
            .foreach {
              case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                if (!persistData) {
                  store.withTxn(store.createTxnWrite()) { txn =>
                    store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                  }
                }
            }
          logger.debug(s"consume: data found for <patterns: $patterns> at <channels: $channels>")
          replayData.put(replaysLessCommRef(replays, commRef))
          Some((continuation, mats.map(_.datum.a)))
        }

        @tailrec
        def getCommOrDataCandidates(comms: Seq[COMM]): Either[COMM, Seq[DataCandidate[C, R]]] =
          comms match {
            case Nil =>
              val msg = "List comms must not be empty"
              logger.error(msg)
              throw new IllegalArgumentException(msg)
            case commRef :: Nil =>
              runMatcher(commRef) match {
                case Some(x) => Right(x)
                case None    => Left(commRef)
              }
            case commRef :: rem =>
              runMatcher(commRef) match {
                case Some(x) => Right(x)
                case None    => getCommOrDataCandidates(rem)
              }
          }

        logger.debug(s"""|consume: searching for data matching <patterns: $patterns>
                         |at <channels: $channels>""".stripMargin.replace('\n', ' '))

        val consumeRef = Consume.create(channels, patterns, continuation, persist)
        val replays    = replayData.take()

        replays.get(consumeRef) match {
          case None =>
            Right(storeWaitingContinuation(replays, consumeRef, None))
          case Some(comms) =>
            val commOrDataCandidates: Either[COMM, Seq[DataCandidate[C, R]]] =
              getCommOrDataCandidates(comms.iterator().asScala.toList)

            commOrDataCandidates match {
              case Left(commRef) =>
                Right(storeWaitingContinuation(replays, consumeRef, Some(commRef)))
              case Right(dataCandidates) =>
                Right(handleMatches(dataCandidates, replays, consumeRef, comms))
            }
        }
      }
    }

  def produce(channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, E, A, R]): Either[E, Option[(K, Seq[R])]] =
    Kamon.withSpan(produceSpan.start(), finishSpan = true) {
      produceLock(channel) {
        @tailrec
        def runMatcher(comm: COMM,
                       produceRef: Produce,
                       groupedChannels: Seq[Seq[C]]): Option[ProduceCandidate[C, P, R, K]] =
          groupedChannels match {
            case Nil => None
            case channels :: remaining =>
              val matchCandidates: Seq[(WaitingContinuation[P, K], Int)] =
                store.withTxn(store.createTxnRead()) { txn =>
                  store.getWaitingContinuation(txn, channels).zipWithIndex.filter {
                    case (WaitingContinuation(_, _, _, source), _) =>
                      comm.consume == source
                  }
                }

              val channelToIndexedData: Map[C, Seq[(Datum[A], Int)]] = store
                .withTxn(store.createTxnRead()) { txn =>
                  channels.map { (c: C) =>
                    val as = store.getData(txn, Seq(c)).zipWithIndex.filter {
                      case (Datum(_, _, source), _) => comm.produces.contains(source)
                    }
                    c -> {
                      if (c == channel) Seq((Datum(data, persist, produceRef), -1)) else as
                    }
                  }
                }
                .toMap

              extractFirstMatch(channels, matchCandidates, channelToIndexedData) match {
                case Right(None)             => runMatcher(comm, produceRef, remaining)
                case Right(produceCandidate) => produceCandidate
              }
          }

        def storeDatum(replays: ReplayData,
                       produceRef: Produce,
                       maybeCommRef: Option[COMM]): None.type = {
          store.withTxn(store.createTxnWrite()) { txn =>
            store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
          }
          logger.debug(s"""|produce: no matching continuation found
                           |storing <data: $data> at <channel: $channel>""".stripMargin)
          replayData.put(replays)
          None
        }

        def handleMatch(mat: ProduceCandidate[C, P, R, K],
                        replays: ReplayData,
                        produceRef: Produce,
                        comms: Multiset[COMM]): Option[(K, Seq[R])] =
          mat match {
            case ProduceCandidate(channels,
                                  WaitingContinuation(_, continuation, persistK, consumeRef),
                                  continuationIndex,
                                  dataCandidates) =>
              produceCommCounter.increment()
              val commRef = COMM(consumeRef, dataCandidates.map(_.datum.source))
              assert(comms.contains(commRef), "COMM Event was not contained in the trace")
              if (!persistK) {
                store.withTxn(store.createTxnWrite()) { txn =>
                  store.removeWaitingContinuation(txn, channels, continuationIndex)
                }
              }
              dataCandidates
                .sortBy(_.datumIndex)(Ordering[Int].reverse)
                .foreach {
                  case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                    store.withTxn(store.createTxnWrite()) { txn =>
                      if (!persistData && dataIndex >= 0) {
                        store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                      }
                      store.removeJoin(txn, candidateChannel, channels)
                    }
                }
              logger.debug(s"produce: matching continuation found at <channels: $channels>")
              replayData.put(replaysLessCommRef(replays, commRef))
              Some((continuation, dataCandidates.map(_.datum.a)))
          }

        store.withTxn(store.createTxnRead()) { txn =>
          val groupedChannels: Seq[Seq[C]] = store.getJoin(txn, channel)

          logger.debug(s"""|produce: searching for matching continuations
                         |at <groupedChannels: $groupedChannels>""".stripMargin.replace('\n', ' '))

          val produceRef = Produce.create(channel, data, persist)
          val replays    = replayData.take()

          @tailrec
          def getCommOrProduceCandidate(
              comms: Seq[COMM]): Either[COMM, ProduceCandidate[C, P, R, K]] =
            comms match {
              case Nil =>
                val msg = "comms must not be empty"
                logger.error(msg)
                throw new IllegalArgumentException(msg)
              case commRef :: Nil =>
                runMatcher(commRef, produceRef, groupedChannels) match {
                  case Some(x) => Right(x)
                  case None    => Left(commRef)
                }
              case commRef :: rem =>
                runMatcher(commRef, produceRef, groupedChannels) match {
                  case Some(x) => Right(x)
                  case None    => getCommOrProduceCandidate(rem)
                }
            }

          replays.get(produceRef) match {
            case None =>
              Right(storeDatum(replays, produceRef, None))
            case Some(comms) =>
              val commOrProduceCandidate: Either[COMM, ProduceCandidate[C, P, R, K]] =
                getCommOrProduceCandidate(comms.iterator().asScala.toList)
              commOrProduceCandidate match {
                case Left(comm) =>
                  Right(storeDatum(replays, produceRef, Some(comm)))
                case Right(produceCandidate) =>
                  Right(handleMatch(produceCandidate, replays, produceRef, comms))
              }
          }
        }
      }
    }

  private def replaysLessCommRef(replays: ReplayData,
                                 commRef: COMM): MultisetMultiMap[IOEvent, COMM] =
    commRef.produces.foldLeft(replays.removeBinding(commRef.consume, commRef)) {
      case (updatedReplays, produceRef) =>
        updatedReplays.removeBinding(produceRef, commRef)
    }

  def createCheckpoint(): Checkpoint =
    if (replayData.get.isEmpty) {
      val root = store.createCheckpoint()
      Checkpoint(root, Seq.empty)
    } else {
      // TODO: Make error message more informative
      val msg = s"unused comm event: replayData multimap has ${replayData.get.size} elements left"
      logger.error(msg)
      throw new ReplayException(msg)
    }

  override def clear(): Unit = {
    replayData.update(const(ReplayData.empty))
    super.clear()
  }
}

object FineLockingReplayRSpace {

  def create[C, P, E, A, R, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): FineLockingReplayRSpace[C, P, E, A, R, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val mainStore = context match {
      case lmdbContext: LMDBContext[C, P, A, K] =>
        LMDBStore.create[C, P, A, K](lmdbContext, branch)

      case memContext: InMemoryContext[C, P, A, K] =>
        InMemoryStore.create(memContext.trieStore, branch)

      case mixedContext: MixedContext[C, P, A, K] =>
        InMemoryStore.create(mixedContext.trieStore, branch)

      case ctx: FineGrainedLMDBContext[C, P, A, K] =>
        ctx.createStore(branch)
    }

    val replaySpace = new FineLockingReplayRSpace[C, P, E, A, R, K](mainStore, branch)

    /*
     * history.initialize returns true if the history trie contains no root (i.e. is empty).
     *
     * In this case, we create a checkpoint for the empty store so that we can reset
     * to the empty store state with the clear method.
     */
    val _ = if (history.initialize(mainStore.trieStore, branch)) {
      replaySpace.createCheckpoint()
    }

    replaySpace
  }

}
