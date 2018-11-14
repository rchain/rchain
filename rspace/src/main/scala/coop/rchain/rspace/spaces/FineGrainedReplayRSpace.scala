package coop.rchain.rspace.spaces

import cats.effect.{ContextShift, Sync}
import coop.rchain.rspace._
import cats.implicits._
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Produce, _}
import scodec.Codec

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext
import kamon._

class FineGrainedReplayRSpace[F[_], C, P, E, A, R, K](store: IStore[C, P, A, K], branch: Branch)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    val syncF: Sync[F],
    contextShift: ContextShift[F],
    scheduler: ExecutionContext
) extends FineGrainedRSpaceOps[F, C, P, E, A, R, K](store, branch)
    with IReplaySpace[F, C, P, E, A, R, K] {

  override protected[this] val logger: Logger = Logger[this.type]

  private[this] val consumeCommCounter = Kamon.counter("replayrspace.comm.consume")
  private[this] val produceCommCounter = Kamon.counter("replayrspace.comm.produce")

  private[this] val consumeSpan   = Kamon.buildSpan("replayrspace.consume")
  private[this] val produceSpan   = Kamon.buildSpan("replayrspace.produce")
  protected[this] val installSpan = Kamon.buildSpan("replayrspace.install")

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
      syncF.delay {
        Kamon.withSpan(consumeSpan.start(), finishSpan = true) {
          if (channels.length =!= patterns.length) {
            val msg = "channels.length must equal patterns.length"
            logger.error(msg)
            throw new IllegalArgumentException(msg)
          }
          val span = Kamon.currentSpan()
          span.mark("before-consume-lock")
          consumeLock(channels) {
            span.mark("consume-lock-acquired")
            lockedConsume(channels, patterns, continuation, persist, sequenceNumber)
          }
        }
      }
    }

  private[this] def lockedConsume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int
  )(
      implicit m: Match[P, E, A, R]
  ): Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]] = {
    val span = Kamon.currentSpan()
    def runMatcher(comm: COMM): Option[Seq[DataCandidate[C, R]]] = {

      span.mark("before-channel-to-indexed-data")
      val channelToIndexedData = channels.map { (c: C) =>
        c -> {
          store.withTxn(store.createTxnRead()) { txn =>
            store.getData(txn, Seq(c)).zipWithIndex.filter {
              case (Datum(_, _, source), _) => comm.produces.contains(source)
            }
          }
        }
      }.toMap
      span.mark("before-extract-data-candidates")
      extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil)
        .flatMap(_.toOption)
        .sequence
    }

    def storeWaitingContinuation(
        consumeRef: Consume,
        maybeCommRef: Option[COMM]
    ): None.type = {
      span.mark("acquire-write-lock")
      store.withTxn(store.createTxnWrite()) { txn =>
        span.mark("before-put-continuation")
        store.putWaitingContinuation(
          txn,
          channels,
          WaitingContinuation(patterns, continuation, persist, consumeRef)
        )
        span.mark("after-put-continuation")
        for (channel <- channels) store.addJoin(txn, channel, channels)
        span.mark("after-add-joins")
      }
      logger.debug(s"""|consume: no data found,
                       |storing <(patterns, continuation): ($patterns, $continuation)>
                       |at <channels: $channels>""".stripMargin.replace('\n', ' '))
      None
    }

    def handleMatches(
        mats: Seq[DataCandidate[C, R]],
        consumeRef: Consume,
        comms: Multiset[COMM]
    ): Option[(ContResult[C, P, K], Seq[Result[R]])] = {
      span.mark("handle-matches-begin")
      consumeCommCounter.increment()
      val commRef = COMM(consumeRef, mats.map(_.datum.source))
      assert(comms.contains(commRef), "COMM Event was not contained in the trace")
      mats
        .sortBy(_.datumIndex)(Ordering[Int].reverse)
        .foreach {
          case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
            if (!persistData) {
              span.mark("acquire-write-lock")
              store.withTxn(store.createTxnWrite()) { txn =>
                span.mark("before-remove-datum")
                store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                span.mark("after-remove-datum")
              }
            }
        }
      logger.debug(s"consume: data found for <patterns: $patterns> at <channels: $channels>")
      removeBindingsFor(commRef)
      span.mark("handle-matches-end")
      val contSequenceNumber = commRef.nextSequenceNumber
      Some(
        (
          ContResult(continuation, persist, channels, patterns, contSequenceNumber),
          mats.map(dc => Result(dc.datum.a, dc.datum.persist))
        )
      )
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

    val consumeRef = Consume.create(channels, patterns, continuation, persist, sequenceNumber)

    replayData.get(consumeRef) match {
      case None =>
        span.mark("no-consume-ref-found")
        Right(storeWaitingContinuation(consumeRef, None))
      case Some(comms) =>
        span.mark("ref-found")
        val commOrDataCandidates: Either[COMM, Seq[DataCandidate[C, R]]] =
          getCommOrDataCandidates(comms.iterator().asScala.toList)

        commOrDataCandidates match {
          case Left(commRef) =>
            span.mark("no-data-candidates-found")
            Right(storeWaitingContinuation(consumeRef, Some(commRef)))
          case Right(dataCandidates) =>
            span.mark("data-candidates-found")
            Right(handleMatches(dataCandidates, consumeRef, comms))
        }
    }
  }

  def produce(channel: C, data: A, persist: Boolean, sequenceNumber: Int)(
      implicit m: Match[P, E, A, R]
  ): F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]] =
    contextShift.evalOn(scheduler) {
      syncF.delay {
        Kamon.withSpan(produceSpan.start(), finishSpan = true) {
          val span = Kamon.currentSpan()
          span.mark("before-produce-lock")
          produceLock(channel) {
            span.mark("produce-lock-acquired")
            lockedProduce(channel, data, persist, sequenceNumber)
          }
        }
      }
    }

  private[this] def lockedProduce(channel: C, data: A, persist: Boolean, sequenceNumber: Int)(
      implicit m: Match[P, E, A, R]
  ): Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]] = {
    val span = Kamon.currentSpan()
    @tailrec
    def runMatcher(
        comm: COMM,
        produceRef: Produce,
        groupedChannels: Seq[Seq[C]]
    ): Option[ProduceCandidate[C, P, R, K]] =
      groupedChannels match {
        case Nil => None
        case channels :: remaining =>
          val matchCandidates: Seq[(WaitingContinuation[P, K], Int)] =
            store.withTxn(store.createTxnRead()) { txn =>
              span.mark("before-get-continuation")
              store.getWaitingContinuation(txn, channels).zipWithIndex.filter {
                case (WaitingContinuation(_, _, _, source), _) =>
                  comm.consume == source
              }
            }
          span.mark("after-get-continuation")

          val channelToIndexedData: Map[C, Seq[(Datum[A], Int)]] = store
            .withTxn(store.createTxnRead()) { txn =>
              channels.map { (c: C) =>
                span.mark("before-get-data")
                val as = store.getData(txn, Seq(c)).zipWithIndex.filter {
                  case (Datum(_, _, source), _) => comm.produces.contains(source)
                }
                span.mark("after-get-data")
                c -> {
                  if (c == channel) Seq((Datum(data, persist, produceRef), -1)) else as
                }
              }.toMap
            }
          extractFirstMatch(channels, matchCandidates, channelToIndexedData) match {
            case Right(None)             => runMatcher(comm, produceRef, remaining)
            case Right(produceCandidate) => produceCandidate
            case Left(_)                 => ???
          }
      }

    def storeDatum(
        produceRef: Produce,
        maybeCommRef: Option[COMM]
    ): None.type = {
      span.mark("acquire-write-lock")
      store.withTxn(store.createTxnWrite()) { txn =>
        span.mark("before-put-datum")
        store.putDatum(txn, Seq(channel), Datum(data, persist, produceRef))
        span.mark("after-put-datum")
      }
      logger.debug(s"""|produce: no matching continuation found
                       |storing <data: $data> at <channel: $channel>""".stripMargin)
      None
    }

    def handleMatch(
        mat: ProduceCandidate[C, P, R, K],
        produceRef: Produce,
        comms: Multiset[COMM]
    ): Option[(ContResult[C, P, K], Seq[Result[R]])] =
      mat match {
        case ProduceCandidate(
            channels,
            WaitingContinuation(patterns, continuation, persistK, consumeRef),
            continuationIndex,
            dataCandidates
            ) =>
          span.mark("handle-match-begin")
          produceCommCounter.increment()
          val commRef = COMM(consumeRef, dataCandidates.map(_.datum.source))
          assert(comms.contains(commRef), "COMM Event was not contained in the trace")
          if (!persistK) {
            span.mark("acquire-write-lock")
            store.withTxn(store.createTxnWrite()) { txn =>
              span.mark("before-put-continuation")
              store.removeWaitingContinuation(txn, channels, continuationIndex)
              span.mark("after-put-continuation")
            }
          }
          dataCandidates
            .sortBy(_.datumIndex)(Ordering[Int].reverse)
            .foreach {
              case DataCandidate(candidateChannel, Datum(_, persistData, _), dataIndex) =>
                span.mark("acquire-write-lock")
                store.withTxn(store.createTxnWrite()) { txn =>
                  if (!persistData && dataIndex >= 0) {
                    span.mark("before-remove-datum")
                    store.removeDatum(txn, Seq(candidateChannel), dataIndex)
                    span.mark("after-remove-datum")
                  }
                  store.removeJoin(txn, candidateChannel, channels)
                  span.mark("after-remove-join")
                }
            }
          logger.debug(s"produce: matching continuation found at <channels: $channels>")
          removeBindingsFor(commRef)
          span.mark("handle-match-end")
          val contSequenceNumber = commRef.nextSequenceNumber
          Some(
            (
              ContResult(continuation, persistK, channels, patterns, contSequenceNumber),
              dataCandidates.map(dc => Result(dc.datum.a, dc.datum.persist))
            )
          )
      }

    store.withTxn(store.createTxnRead()) { txn =>
      val groupedChannels: Seq[Seq[C]] = store.getJoin(txn, channel)

      logger.debug(s"""|produce: searching for matching continuations
                       |at <groupedChannels: $groupedChannels>""".stripMargin.replace('\n', ' '))

      val produceRef = Produce.create(channel, data, persist, sequenceNumber)

      @tailrec
      def getCommOrProduceCandidate(
          comms: Seq[COMM]
      ): Either[COMM, ProduceCandidate[C, P, R, K]] =
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

      replayData.get(produceRef) match {
        case None =>
          span.mark("no-produce-ref-found")
          Right(storeDatum(produceRef, None))
        case Some(comms) =>
          val commOrProduceCandidate: Either[COMM, ProduceCandidate[C, P, R, K]] =
            getCommOrProduceCandidate(comms.iterator().asScala.toList)
          commOrProduceCandidate match {
            case Left(comm) =>
              span.mark("no-produce-candidate-found")
              Right(storeDatum(produceRef, Some(comm)))
            case Right(produceCandidate) =>
              span.mark("produce-candidate-found")
              Right(handleMatch(produceCandidate, produceRef, comms))
          }
      }
    }
  }

  @inline
  private def removeBindingsFor(
      commRef: COMM
  ): Unit =
    commRef.produces.foldLeft(replayData.removeBinding(commRef.consume, commRef)) {
      case (updatedReplays, produceRef) =>
        updatedReplays.removeBinding(produceRef, commRef)
    }

  def createCheckpoint(): F[Checkpoint] = syncF.delay {
    if (replayData.isEmpty) {
      val root = store.createCheckpoint()
      Checkpoint(root, Seq.empty)
    } else {
      // TODO: Make error message more informative
      val msg = s"unused comm event: replayData multimap has ${replayData.size} elements left"
      logger.error(msg)
      throw new ReplayException(msg)
    }
  }

  override def clear(): F[Unit] =
    for {
      _ <- syncF.delay {
            replayData.clear()
          }
      _ <- super.clear()
    } yield ()
}

object FineGrainedReplayRSpace {

  def create[F[_], C, P, E, A, R, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      sync: Sync[F],
      contextShift: ContextShift[F],
      scheduler: ExecutionContext
  ): F[FineGrainedReplayRSpace[F, C, P, E, A, R, K]] = {

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
        LockFreeInMemoryStore.create(mixedContext.trieStore, branch)
    }

    val replaySpace = new FineGrainedReplayRSpace[F, C, P, E, A, R, K](mainStore, branch)

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
