package coop.rchain.rspace

import cats.implicits._
import cats.{Eval, Monoid}
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.Match.MatchResult
import coop.rchain.rspace.history.{Branch, ITrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Produce, _}
import coop.rchain.shared.SyncVarOps._
import kamon._
import scodec.Codec

import scala.Function.const
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar

class ReplayRSpace[C, P, E, A, S, R, K](store: IStore[C, P, A, K], branch: Branch)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    monoid: Monoid[S]
) extends RSpaceOps[C, P, E, A, S, R, K](store, branch) {

  override protected[this] val logger: Logger = Logger[this.type]

  private[rspace] val replayData: SyncVar[ReplayData] = {
    val sv = new SyncVar[ReplayData]()
    sv.put(ReplayData.empty)
    sv
  }

  private[this] val consumeCommCounter = Kamon.counter("replayrspace.comm.consume")
  private[this] val produceCommCounter = Kamon.counter("replayrspace.comm.produce")

  private[this] val consumeSpan   = Kamon.buildSpan("replayrspace.consume")
  private[this] val produceSpan   = Kamon.buildSpan("replayrspace.produce")
  protected[this] val installSpan = Kamon.buildSpan("replayrspace.install")

  override def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, E, A, S, R]): MatchResult[(K, Seq[R]), S, E] =
    Kamon.withSpan(consumeSpan.start(), finishSpan = true) {
      if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logger.error(msg)
        throw new IllegalArgumentException(msg)
      }
      store.withTxn(store.createTxnWrite()) { txn =>
        def runMatcher(comm: COMM): Option[Seq[DataCandidate[C, R]]] = {
          val channelToIndexedData = channels.map { (c: C) =>
            c -> {
              store.getData(txn, Seq(c)).zipWithIndex.filter {
                case (Datum(_, _, source), _) => comm.produces.contains(source)
              }
            }
          }.toMap
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).value
            .flatMap(_.toEither.toOption) // TODO(mateusz.gorski): I am not sure about this
            .sequence
        }

        def storeWaitingContinuation(replays: ReplayData,
                                     consumeRef: Consume,
                                     maybeCommRef: Option[COMM]): None.type = {
          store.putWaitingContinuation(
            txn,
            channels,
            WaitingContinuation(patterns, continuation, persist, consumeRef))
          for (channel <- channels) store.addJoin(txn, channel, channels)
          logger.debug(s"""|consume: no data found,
                           |storing <(patterns, continuation): ($patterns, $continuation)>
                           |at <channels: $channels>""".stripMargin.replace('\n', ' '))
          replayData.put(replays)
          None
        }

        def handleMatches(mats: Seq[DataCandidate[C, R]],
                          replays: ReplayData,
                          consumeRef: Consume,
                          comms: Multiset[COMM]): (K, Seq[R]) = {
          consumeCommCounter.increment()
          val commRef = COMM(consumeRef, mats.map(_.datum.source))
          assert(comms.contains(commRef), "COMM Event was not contained in the trace")
          mats
            .sortBy(_.datumIndex)(Ordering[Int].reverse)
            .foreach {
              case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                if (!persistData) {
                  store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                }
            }
          logger.debug(s"consume: data found for <patterns: $patterns> at <channels: $channels>")
          replayData.put(replaysLessCommRef(replays, commRef))
          (continuation, mats.map(_.datum.a))
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
            storeWaitingContinuation(replays, consumeRef, None)
            MatchResult.NotFound(monoid.empty)
          case Some(comms) =>
            val commOrDataCandidates: Either[COMM, Seq[DataCandidate[C, R]]] =
              getCommOrDataCandidates(comms.iterator().asScala.toList)

            commOrDataCandidates match {
              case Left(commRef) =>
                storeWaitingContinuation(replays, consumeRef, Some(commRef))
                MatchResult.NotFound(monoid.empty)
              case Right(dataCandidates) =>
                MatchResult.Found(monoid.empty,
                                  handleMatches(dataCandidates, replays, consumeRef, comms))
            }
        }
      }
    }

  def produce(channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, E, A, S, R]): MatchResult[(K, Seq[R]), S, E] =
    Kamon.withSpan(produceSpan.start(), finishSpan = true) {
      store.withTxn(store.createTxnWrite()) { txn =>
        def runMatcher(
            comm: COMM,
            produceRef: Produce,
            groupedChannels: Seq[Seq[C]]): Eval[MatchResult[ProduceCandidate[C, P, R, K], S, E]] =
          groupedChannels match {
            case Nil => Eval.later(MatchResult.NotFound(monoid.empty))
            case channels :: remaining =>
              val matchCandidates: Seq[(WaitingContinuation[P, K], Int)] =
                store.getWaitingContinuation(txn, channels).zipWithIndex.filter {
                  case (WaitingContinuation(_, _, _, source), _) =>
                    comm.consume == source
                }
              //TODO: move out from the `runMatcher` method
              val channelToIndexedData: Map[C, Seq[(Datum[A], Int)]] = channels.map { (c: C) =>
                val as = store.getData(txn, Seq(c)).zipWithIndex.filter {
                  case (Datum(_, _, source), _) => comm.produces.contains(source)
                }
                c -> { if (c == channel) Seq((Datum(data, persist, produceRef), -1)) else as }
              }.toMap
              extractFirstMatch(channels, matchCandidates, channelToIndexedData).flatMap {
                case MatchResult.NotFound(s1) =>
                  runMatcher(comm, produceRef, remaining).map(_.map(s2 => monoid.combine(s1, s2)))
                case MatchResult.Error(s1, e) => Eval.later(MatchResult.Error(s1, e))
                case MatchResult.Found(s, produceCandidate) =>
                  Eval.later(MatchResult.Found(s, produceCandidate))
              }
          }

        def storeDatum(replays: ReplayData,
                       produceRef: Produce,
                       maybeCommRef: Option[COMM]): None.type = {
          store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
          logger.debug(s"""|produce: no matching continuation found
                           |storing <data: $data> at <channel: $channel>""".stripMargin)
          replayData.put(replays)
          None
        }

        def handleMatch(mat: ProduceCandidate[C, P, R, K],
                        replays: ReplayData,
                        produceRef: Produce,
                        comms: Multiset[COMM]): (K, Seq[R]) =
          mat match {
            case ProduceCandidate(channels,
                                  WaitingContinuation(_, continuation, persistK, consumeRef),
                                  continuationIndex,
                                  dataCandidates) =>
              produceCommCounter.increment()
              val commRef = COMM(consumeRef, dataCandidates.map(_.datum.source))
              assert(comms.contains(commRef), "COMM Event was not contained in the trace")
              if (!persistK) {
                store.removeWaitingContinuation(txn, channels, continuationIndex)
              }
              dataCandidates
                .sortBy(_.datumIndex)(Ordering[Int].reverse)
                .foreach {
                  case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                    if (!persistData && dataIndex >= 0) {
                      store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                    }
                    store.removeJoin(txn, candidateChannel, channels)
                }
              logger.debug(s"produce: matching continuation found at <channels: $channels>")
              replayData.put(replaysLessCommRef(replays, commRef))
              (continuation, dataCandidates.map(_.datum.a))
          }

        val groupedChannels: Seq[Seq[C]] = store.getJoin(txn, channel)

        logger.debug(s"""|produce: searching for matching continuations
                         |at <groupedChannels: $groupedChannels>""".stripMargin.replace('\n', ' '))

        val produceRef = Produce.create(channel, data, persist)
        val replays    = replayData.take()

        def getCommOrProduceCandidate(
            comms: Seq[COMM]): Eval[MatchResult[Either[COMM, ProduceCandidate[C, P, R, K]], S, E]] =
          comms match {
            case Nil =>
              val msg = "comms must not be empty"
              logger.error(msg)
              Eval.later(throw new IllegalArgumentException(msg))
            case commRef :: Nil =>
              runMatcher(commRef, produceRef, groupedChannels).map {
                case MatchResult.Found(s, v) => MatchResult.Found(s, Right(v))
                case MatchResult.NotFound(s) => MatchResult.Found(s, Left(commRef))
                case MatchResult.Error(s, e) => MatchResult.Error(s, e)
              }
            case commRef :: rem =>
              runMatcher(commRef, produceRef, groupedChannels).flatMap {
                case MatchResult.Found(s, v) => Eval.later(MatchResult.Found(s, Right(v)))
                case MatchResult.NotFound(s1) =>
                  getCommOrProduceCandidate(rem).map(_.map(s2 => monoid.combine(s1, s2)))
                case MatchResult.Error(s, e) => Eval.later(MatchResult.Error(s, e))
              }
          }

        replays.get(produceRef) match {
          case None =>
            storeDatum(replays, produceRef, None)
            MatchResult.NotFound(monoid.empty)
          case Some(comms) =>
            val commOrProduceCandidate
              : Eval[MatchResult[Either[COMM, ProduceCandidate[C, P, R, K]], S, E]] =
              getCommOrProduceCandidate(comms.iterator().asScala.toList)
            //TODO(mateusz.gorski): match may not be exhaustive.
            //It would fail on the following inputs: Error(_, _), NotFound(_)
            commOrProduceCandidate.value match {
              case MatchResult.Found(s, Left(comm)) =>
                storeDatum(replays, produceRef, Some(comm))
                MatchResult.NotFound(s)
              case MatchResult.Found(s, Right(produceCandidate)) =>
                val mat = handleMatch(produceCandidate, replays, produceRef, comms)
                MatchResult.Found(monoid.empty, mat)
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

  def getReplayData: ReplayData = replayData.get

  def rig(startRoot: Blake2b256Hash, log: trace.Log): Unit = {
    // create a set of the "new" IOEvents
    val newStuff: Set[Event] = log.filter {
      case Produce(_, _) => true
      case Consume(_, _) => true
      case _             => false
    }.toSet
    // create and prepare the ReplayData table
    val rigs: ReplayData = ReplayData.empty
    log.foreach {
      case comm @ COMM(consume, produces) =>
        (consume +: produces).foreach { ioEvent =>
          if (newStuff(ioEvent)) {
            rigs.addBinding(ioEvent, comm)
          }
        }
      case _ =>
        ()
    }
    // reset to the starting checkpoint
    reset(startRoot)
    // update the replay data
    replayData.update(const(rigs))
  }

  override def clear(): Unit = {
    replayData.update(const(ReplayData.empty))
    super.clear()
  }
}

object ReplayRSpace {

  def create[C, P, E, A, S, R, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Monoid[S]): ReplayRSpace[C, P, E, A, S, R, K] = {

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
    }

    val replaySpace = new ReplayRSpace[C, P, E, A, S, R, K](mainStore, branch)

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

  def createInMemory[C, P, E, A, S, R, K](
      trieStore: ITrieStore[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]],
                            Blake2b256Hash,
                            GNAT[C, P, A, K]],
      branch: Branch)(implicit
                      sc: Serialize[C],
                      sp: Serialize[P],
                      sa: Serialize[A],
                      sk: Serialize[K],
                      m: Monoid[S]): ReplayRSpace[C, P, E, A, S, R, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val mainStore: IStore[C, P, A, K] = InMemoryStore
      .create[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]], C, P, A, K](
        trieStore,
        branch)
    val replaySpace = new ReplayRSpace[C, P, E, A, S, R, K](mainStore, branch)

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
