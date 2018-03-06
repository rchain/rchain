package coop.rchain.storage

import cats.implicits._
import coop.rchain.storage.util.ignore

import scala.annotation.tailrec

// TODO(ht): improve docstrings
trait StorageActions {

  /* Consume */

  @tailrec
  private[storage] final def findMatchingDataCandidate[C, P, A](
      data: List[(A, Int)],
      pattern: P
  )(implicit m: Match[P, A]): Option[(A, Int)] =
    data match {
      case Nil => None
      case (matchCandidate, dataIndex) :: remaining =>
        m.get(pattern, matchCandidate) match {
          case None      => findMatchingDataCandidate(remaining, pattern)
          case Some(mat) => Some((mat, dataIndex))
        }
    }

  private[storage] def extractDataCandidates[C, P, A, K](
      store: IStore[C, P, A, K],
      channels: List[C],
      patterns: List[P])(txn: store.T)(implicit m: Match[P, A]): Option[List[(A, C, Int)]] = {
    val options: List[Option[(A, C, Int)]] = channels.zip(patterns).map {
      case (channel, pattern) =>
        val indexedData: List[(A, Int)] = store.getAs(txn, List(channel)).zipWithIndex
        findMatchingDataCandidate(indexedData, pattern).map {
          case (data, dataIndex) => (data, channel, dataIndex)
        }
    }
    options.sequence[Option, (A, C, Int)]
  }

  /** `consume` does the "consume" thing
    *
    * @param store
    * @param channels
    * @param patterns
    * @param continuation
    * @tparam C a type representing a channel
    * @tparam P a type representing a pattern
    * @tparam A a type representing a piece of data
    * @tparam K a type representing a continuation
    */
  def consume[C, P, A, K](store: IStore[C, P, A, K],
                          channels: List[C],
                          patterns: List[P],
                          continuation: K)(implicit m: Match[P, A]): Option[(K, List[A])] = {
    if (channels.length =!= patterns.length)
      throw new IllegalArgumentException("channels.length must equal patterns.length")
    store.withTxn(store.createTxnWrite()) { txn =>
      extractDataCandidates(store, channels, patterns)(txn) match {
        case None =>
          store.putK(txn, channels, patterns, continuation)
          for (channel <- channels) store.addJoin(txn, channel, channels)
          None
        case Some(candidates) =>
          candidates.foreach {
            case (_, candidateChannel, dataIndex) =>
              store.removeA(txn, candidateChannel, dataIndex)
          }
          Some((continuation, candidates.map(_._1)))
      }
    }
  }

  /* Produce */

  @tailrec
  private[storage] final def extractProduceCandidates[C, P, A, K](store: IStore[C, P, A, K],
                                                                  groupedChannels: List[List[C]])(
      txn: store.T)(implicit m: Match[P, A]): Option[(K, List[(A, C, Int)], List[C], List[P])] =
    groupedChannels match {
      case Nil =>
        None
      case channels :: remaining =>
        val matchCandidates: List[(List[P], K)] = store.getPsK(txn, channels)
        val matches: List[(K, List[(A, C, Int)], List[C], List[P])] = matchCandidates.flatMap {
          case (patterns, continuation) =>
            extractDataCandidates(store, channels, patterns)(txn)
              .map(candidates => (continuation, candidates, channels, patterns))
              .toList
        }
        matches.headOption match {
          case None => extractProduceCandidates(store, remaining)(txn)
          case res  => res
        }
    }

  /** `produce` does the "produce" thing
    *
    * @param store
    * @param channel
    * @param data
    * @tparam C a type representing a channel
    * @tparam P a type representing a pattern
    * @tparam A a type representing a piece of data
    * @tparam K a type representing a continuation
    */
  def produce[C, P, A, K, T](store: IStore[C, P, A, K], channel: C, data: A)(
      implicit m: Match[P, A]): Option[(K, List[A])] =
    store.withTxn(store.createTxnWrite()) { txn =>
      val groupedChannels: List[List[C]] = store.getJoin(txn, channel)
      store.putA(txn, List(channel), data)
      extractProduceCandidates(store, groupedChannels)(txn).map {
        case (continuation, candidates, channels, patterns) =>
          candidates.foreach {
            case (_, candidateChannel, dataIndex) =>
              store.removeA(txn, candidateChannel, dataIndex)
              ignore { store.removeJoin(txn, candidateChannel, channels) }
          }
          store.removePsK(txn, channels, patterns)
          (continuation, candidates.map(_._1))
      }
    }
}
