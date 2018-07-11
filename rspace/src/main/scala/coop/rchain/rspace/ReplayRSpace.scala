package coop.rchain.rspace

import cats.implicits._
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.history.Branch
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

class ReplayRSpace[C, P, A, R, K](val store: IStore[C, P, A, K], val branch: Branch)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K]
) extends ISpace[C, P, A, R, K] {

  private val logger: Logger = Logger[this.type]

  private val replayData: SyncVar[ReplayData] = {
    val sv = new SyncVar[ReplayData]()
    sv.put(ReplayData.empty)
    sv
  }

  def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, A, R]): Option[(K, Seq[R])] =
    store.eventsCounter.registerConsume {
      if (channels.length =!= patterns.length) {
        val msg = "channels.length must equal patterns.length"
        logger.error(msg)
        throw new IllegalArgumentException(msg)
      }
      store.withTxn(store.createTxnWrite()) { txn =>
        def runMatcher(maybeComm: Option[COMM]): Option[Seq[DataCandidate[C, R]]] = {
          val channelToIndexedData = channels.map { (c: C) =>
            c -> {
              maybeComm match {
                case Some(comm) =>
                  store.getData(txn, Seq(c)).zipWithIndex.filter {
                    case (Datum(_, _, source), _) => comm.produces.contains(source)
                  }
                case None =>
                  Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
              }
            }
          }.toMap
          extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence
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
          replayData.put(maybeCommRef match {
            case Some(commRef: COMM) => replays.removeBinding(consumeRef, commRef)
            case None                => replays
          })
          None
        }

        def handleMatches(mats: Seq[DataCandidate[C, R]],
                          replays: ReplayData,
                          consumeRef: Consume,
                          comms: Multiset[COMM]): Option[(K, Seq[R])] = {
          store.eventsCounter.registerConsumeCommEvent()
          val commRef = COMM(consumeRef, mats.map(_.datum.source))
          assert(comms.contains(commRef), "COMM Event was not contained in the trace")
          mats
            .sortBy(_.datumIndex)(Ordering[Int].reverse)
            .foreach {
              case DataCandidate(candidateChannel, Datum(_, persistData, prodRef), dataIndex) =>
                if (!persistData) {
                  store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                }
                replays.removeBinding(prodRef, commRef)
            }
          logger.debug(s"consume: data found for <patterns: $patterns> at <channels: $channels>")
          replayData.put(replays.removeBinding(consumeRef, commRef))
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
              runMatcher(Some(commRef)) match {
                case Some(x) => Right(x)
                case None    => Left(commRef)
              }
            case commRef :: rem =>
              runMatcher(Some(commRef)) match {
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
            runMatcher(None) match {
              case None =>
                storeWaitingContinuation(replays, consumeRef, None)
              case Some(_) =>
                val msg = "untraced event resulted in a comm event"
                logger.error(msg)
                replayData.put(replays)
                throw new ReplayException(msg)
            }
          case Some(comms) =>
            val commOrDataCandidates: Either[COMM, Seq[DataCandidate[C, R]]] =
              getCommOrDataCandidates(comms.iterator().asScala.toList)

            commOrDataCandidates match {
              case Left(commRef) =>
                storeWaitingContinuation(replays, consumeRef, Some(commRef))
              case Right(dataCandidates) =>
                handleMatches(dataCandidates, replays, consumeRef, comms)
            }
        }
      }
    }

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, A, R]): Option[(K, Seq[R])] = None

  def produce(channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, A, R]): Option[(K, Seq[R])] =
    store.eventsCounter.registerProduce {
      store.withTxn(store.createTxnWrite()) { txn =>
        @tailrec
        def runMatcher(maybeComm: Option[COMM],
                       produceRef: Produce,
                       groupedChannels: Seq[Seq[C]]): Option[ProduceCandidate[C, P, R, K]] =
          groupedChannels match {
            case Nil => None
            case channels :: remaining =>
              val matchCandidates: Seq[(WaitingContinuation[P, K], Int)] =
                maybeComm match {
                  case Some(comm) =>
                    store.getWaitingContinuation(txn, channels).zipWithIndex.filter {
                      case (WaitingContinuation(_, _, _, source), _) =>
                        comm.consume == source
                    }
                  case None =>
                    Random.shuffle(store.getWaitingContinuation(txn, channels).zipWithIndex)
                }
              val channelToIndexedData: Map[C, Seq[(Datum[A], Int)]] = channels.map { (c: C) =>
                val as = maybeComm match {
                  case Some(comm) =>
                    store.getData(txn, Seq(c)).zipWithIndex.filter {
                      case (Datum(_, _, source), _) => comm.produces.contains(source)
                    }
                  case None =>
                    Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
                }
                c -> { if (c == channel) (Datum(data, persist, produceRef), -1) +: as else as }
              }.toMap
              extractFirstMatch(channels, matchCandidates, channelToIndexedData) match {
                case None             => runMatcher(maybeComm, produceRef, remaining)
                case produceCandidate => produceCandidate
              }
          }

        def storeDatum(replays: ReplayData,
                       produceRef: Produce,
                       maybeCommRef: Option[COMM]): None.type = {
          store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
          logger.debug(s"""|produce: no matching continuation found
                           |storing <data: $data> at <channel: $channel>""".stripMargin)
          replayData.put(maybeCommRef match {
            case Some(commRef: COMM) => replays.removeBinding(produceRef, commRef)
            case None                => replays
          })
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
              store.eventsCounter.registerProduceCommEvent()
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
              replayData.put(
                replays
                  .removeBinding(produceRef, commRef)
                  .removeBinding(consumeRef, commRef))
              Some((continuation, dataCandidates.map(_.datum.a)))
          }

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
              runMatcher(Some(commRef), produceRef, groupedChannels) match {
                case Some(x) => Right(x)
                case None    => Left(commRef)
              }
            case commRef :: rem =>
              runMatcher(Some(commRef), produceRef, groupedChannels) match {
                case Some(x) => Right(x)
                case None    => getCommOrProduceCandidate(rem)
              }
          }

        replays.get(produceRef) match {
          case None =>
            runMatcher(None, produceRef, groupedChannels) match {
              case None =>
                storeDatum(replays, produceRef, None)
              case Some(ProduceCandidate(channels, _, _, _)) =>
                logger.debug(s"produce: matching continuation found at <channels: $channels>")
                val msg = "untraced event resulted in a comm event"
                logger.error(msg)
                replayData.put(replays)
                throw new ReplayException(msg)
            }
          case Some(comms) =>
            val commOrProduceCandidate: Either[COMM, ProduceCandidate[C, P, R, K]] =
              getCommOrProduceCandidate(comms.iterator().asScala.toList)
            commOrProduceCandidate match {
              case Left(comm) =>
                storeDatum(replays, produceRef, Some(comm))
              case Right(produceCandidate) =>
                handleMatch(produceCandidate, replays, produceRef, comms)
            }
        }
      }
    }

  def createCheckpoint(): Checkpoint = {
    val root = store.createCheckpoint()
    Checkpoint(root, Seq.empty)
  }

  def getReplayData: ReplayData = replayData.get

  def rig(startRoot: Blake2b256Hash, log: trace.Log): Unit = {
    // create a set of the "new" IOEvents
    val newStuff: Set[Event] = log.filter {
      case Produce(_) => true
      case Consume(_) => true
      case _          => false
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
}

object ReplayRSpace {

  def create[C, P, A, R, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): ReplayRSpace[C, P, A, R, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    history.initialize(context.trieStore, branch)

    val mainStore = LMDBStore.create[C, P, A, K](context, branch)

    new ReplayRSpace[C, P, A, R, K](mainStore, branch)
  }
}
