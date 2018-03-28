package coop.rchain

import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.internal._

import scala.annotation.tailrec
import scala.util.Random

package object rspace {

  private val logger: Logger = Logger[this.type]

  /* Consume */

  @tailrec
  private[rspace] final def findMatchingDataCandidate[C, P, A](
      channel: C,
      data: List[(Datum[A], Int)],
      pattern: P
  )(implicit m: Match[P, A]): Option[DataCandidate[C, A]] =
    data match {
      case Nil => None
      case (Datum(matchCandidate, persist), dataIndex) :: remaining =>
        m.get(pattern, matchCandidate) match {
          case None      => findMatchingDataCandidate(channel, remaining, pattern)
          case Some(mat) => Some(DataCandidate(channel, Datum(mat, persist), dataIndex))
        }
    }

  private[rspace] def extractDataCandidates[C, P, A, K](store: IStore[C, P, A, K],
                                                        channels: List[C],
                                                        patterns: List[P])(txn: store.T)(
      implicit m: Match[P, A]): Option[List[DataCandidate[C, A]]] = {
    val options: List[Option[DataCandidate[C, A]]] =
      channels.zip(patterns).map {
        case (channel, pattern) =>
          val indexedData: List[(Datum[A], Int)] = store.getAs(txn, List(channel)).zipWithIndex
          findMatchingDataCandidate(channel, Random.shuffle(indexedData), pattern)
      }
    options.sequence[Option, DataCandidate[C, A]]
  }

  private[rspace] def extractDataCandidates[C, P, A, K](
      store: IStore[C, P, A, K],
      channels: List[C],
      patterns: List[P],
      batChannel: C,
      data: Datum[A])(txn: store.T)(implicit m: Match[P, A]): Option[List[DataCandidate[C, A]]] = {
    val options: List[Option[DataCandidate[C, A]]] =
      channels.zip(patterns).map {
        case (channel, pattern) if channel == batChannel =>
          val indexedData: List[(Datum[A], Int)] = List((data, -1))
          findMatchingDataCandidate(channel, indexedData, pattern)
        case (channel, pattern) =>
          val indexedData: List[(Datum[A], Int)] = store.getAs(txn, List(channel)).zipWithIndex
          findMatchingDataCandidate(channel, Random.shuffle(indexedData), pattern)
      }
    options.sequence[Option, DataCandidate[C, A]]
  }

  /** Searches the store for data matching all the given patterns at the given channels.
    *
    * If no match is found, then the continuation and patterns are put in the store at the given
    * channels.
    *
    * If a match is found, then the continuation is returned along with the matching data.
    *
    * Matching data stored with the `persist` flag set to `true` will not be removed when it is
    * retrieved. See below for more information about using the `persist` flag.
    *
    * '''NOTE''':
    *
    * A call to [[consume]] that is made with the persist flag set to `true` only persists when
    * there is no matching data.
    *
    * This means that in order to make a continuation "stick" in the store, the user will have to
    * continue to call [[consume]] until a `None` is received.
    *
    * @param store A store which satisfies the [[IStore]] interface.
    * @param channels A list of channels on which to search for matching data
    * @param patterns A list of patterns with which to search for matching data
    * @param continuation A continuation
    * @param persist Whether or not to attempt to persist the data
    * @tparam C A type representing a channel
    * @tparam P A type representing a pattern
    * @tparam A A type representing a piece of data
    * @tparam K A type representing a continuation
    */
  def consume[C, P, A, K](store: IStore[C, P, A, K],
                          channels: List[C],
                          patterns: List[P],
                          continuation: K,
                          persist: Boolean)(implicit m: Match[P, A]): Option[(K, List[A])] = {
    if (channels.length =!= patterns.length) {
      val msg = "channels.length must equal patterns.length"
      logger.error(msg)
      throw new IllegalArgumentException(msg)
    }
    store.withTxn(store.createTxnWrite()) { txn =>
      logger.debug(
        s"consume: searching for data matching <patterns: $patterns> at <channels: $channels>")
      extractDataCandidates(store, channels, patterns)(txn) match {
        case None =>
          store.putK(txn, channels, WaitingContinuation(patterns, continuation, persist))
          for (channel <- channels) store.addJoin(txn, channel, channels)
          logger.debug(
            s"consume: no data found, storing <(patterns, continuation): ($patterns, $continuation)> at <channels: $channels>")
          None
        case Some(dataCandidates) =>
          dataCandidates.foreach {
            case DataCandidate(candidateChannel, Datum(_, persistData), dataIndex)
                if !persistData =>
              store.removeA(txn, candidateChannel, dataIndex)
            case _ =>
              ()
          }
          logger.debug(s"consume: data found for <patterns: $patterns> at <channels: $channels>")
          Some((continuation, dataCandidates.map(_.datum.a)))
      }
    }
  }

  /* Produce */

  @tailrec
  private[rspace] final def extractFirstMatch[C, P, A, K](
      store: IStore[C, P, A, K],
      channels: List[C],
      matchCandidates: List[(WaitingContinuation[P, K], Int)],
      channel: C,
      data: Datum[A])(txn: store.T)(implicit m: Match[P, A]): Option[ProduceCandidate[C, P, A, K]] =
    matchCandidates match {
      case Nil => None
      case (p @ WaitingContinuation(patterns, _, _), index) :: remaining =>
        extractDataCandidates(store, channels, patterns, channel, data)(txn) match {
          case None =>
            extractFirstMatch(store, channels, remaining, channel, data)(txn)
          case Some(dataCandidates) =>
            Some(ProduceCandidate(channels, p, index, dataCandidates))
        }
    }

  @tailrec
  private[rspace] final def extractProduceCandidateAlt[C, P, A, K](
      store: IStore[C, P, A, K],
      groupedChannels: List[List[C]],
      channel: C,
      data: Datum[A])(txn: store.T)(implicit m: Match[P, A]): Option[ProduceCandidate[C, P, A, K]] =
    groupedChannels match {
      case Nil => None
      case channels :: remaining =>
        val matchCandidates: List[(WaitingContinuation[P, K], Int)] =
          store.getPsK(txn, channels).zipWithIndex
        extractFirstMatch(store, channels, Random.shuffle(matchCandidates), channel, data)(txn) match {
          case None             => extractProduceCandidateAlt(store, remaining, channel, data)(txn)
          case produceCandidate => produceCandidate
        }
    }

  /** Searches the store for a continuation that has patterns that match the given data at the
    * given channel.
    *
    * If no match is found, then the data is put in the store at the given channel.
    *
    * If a match is found, then the continuation is returned along with the matching data.
    *
    * Matching data or continuations stored with the `persist` flag set to `true` will not be
    * removed when they are retrieved. See below for more information about using the `persist`
    * flag.
    *
    * '''NOTE''':
    *
    * A call to [[produce]] that is made with the persist flag set to `true` only persists when
    * there are no matching continuations.
    *
    * This means that in order to make a piece of data "stick" in the store, the user will have to
    * continue to call [[produce]] until a `None` is received.
    *
    * @param store A store which satisfies the [[IStore]] interface.
    * @param channel A channel on which to search for matching continuations and/or store data
    * @param data A piece of data
    * @param persist Whether or not to attempt to persist the data
    * @tparam C A type representing a channel
    * @tparam P A type representing a pattern
    * @tparam A A type representing a piece of data
    * @tparam K A type representing a continuation
    */
  def produce[C, P, A, K](store: IStore[C, P, A, K], channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, A]): Option[(K, List[A])] =
    store.withTxn(store.createTxnWrite()) { txn =>
      val groupedChannels: List[List[C]] = store.getJoin(txn, channel)
      logger.debug(
        s"produce: searching for matching continuations at <groupedChannels: $groupedChannels>")
      extractProduceCandidateAlt(store, groupedChannels, channel, Datum(data, persist))(txn) match {
        case Some(
            ProduceCandidate(channels,
                             WaitingContinuation(_, continuation, persistK),
                             continuationIndex,
                             dataCandidates)) =>
          if (!persistK) {
            store.removePsK(txn, channels, continuationIndex)
          }
          dataCandidates.foreach {
            case DataCandidate(candidateChannel, Datum(_, persistData), dataIndex) =>
              if (!persistData && dataIndex >= 0) {
                store.removeA(txn, candidateChannel, dataIndex)
              }
              store.removeJoin(txn, candidateChannel, channels)
            case _ =>
              ()
          }
          logger.debug(s"produce: matching continuation found at <channels: $channels>")
          Some(continuation, dataCandidates.map(_.datum.a))
        case None =>
          logger.debug(s"produce: no matching continuation found")
          store.putA(txn, List(channel), Datum(data, persist))
          logger.debug(s"produce: persisted <data: $data> at <channel: $channel>")
          None
      }
    }
}
