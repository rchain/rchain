package coop.rchain.storage

import cats.implicits._
import coop.rchain.storage.util.ignore

import scala.annotation.tailrec

// TODO(ht): improve docstrings
trait StorageActions {

  /* Consume */

  @tailrec
  private[storage] final def findMatchingDataCandidates[C, P, A](
      data: List[(A, Int)],
      pattern: P,
      channel: C)(implicit m: Match[P, A]): Option[(A, C, Int)] =
    data match {
      case Nil => None
      case (a, i) :: as =>
        m.get(pattern, a) match {
          case None      => findMatchingDataCandidates(as, pattern, channel)
          case Some(mat) => Some((mat, channel, i))
        }
    }

  private[storage] def extractDataCandidates[C, P, A, K](
      store: IStore[C, P, A, K],
      channels: List[C],
      patterns: List[P])(txn: store.T)(implicit m: Match[P, A]): Option[List[(A, C, Int)]] = {
    val options = channels.zip(patterns).map {
      case (c, p) =>
        val as: List[(A, Int)] = store.getAs(txn, c.pure[List]).zipWithIndex
        findMatchingDataCandidates(as, p, c)
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
      throw new IllegalArgumentException("cs.length must equal ps.length")
    store.withTxn(store.createTxnWrite()) { txn =>
      extractDataCandidates(store, channels, patterns)(txn) match {
        case None =>
          store.putK(txn, channels, patterns, continuation)
          for (c <- channels) store.addJoin(txn, c, channels)
          None
        case Some(acis) =>
          acis.foreach {
            case (_, c, i) =>
              store.removeA(txn, c.pure[List], i)
              store.removeK(txn, channels, i)
              ignore { store.removeJoin(txn, c, channels) }
          }
          Some((continuation, acis.map(_._1)))
      }
    }
  }

  /* Produce */

  // TODO(ht): write a recursive version with early return
  private[storage] def extractProduceCandidates[C, P, A, K](
      store: IStore[C, P, A, K],
      groupedKeys: List[List[C]],
      channel: C,
      data: A)(txn: store.T)(implicit m: Match[P, A]): Option[(K, List[(A, C, Int)])] =
    groupedKeys.foldRight(None: Option[(K, List[(A, C, Int)])]) { (cs: List[C], acc) =>
      store.getK(txn, cs).flatMap {
        case (ps, k) =>
          extractDataCandidates(store, channel.pure[List], ps)(txn) match {
            case None       => acc
            case Some(acis) => Some((k, acis))
          }
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
      val groupedKeys: List[List[C]] = store.getJoin(txn, channel)
      store.putA(txn, channel.pure[List], data)
      extractProduceCandidates(store, groupedKeys, channel, data)(txn).map {
        case (k, acis) =>
          acis.foreach {
            case (_, c, i) =>
              store.removeA(txn, c.pure[List], i)
              store.removeK(txn, c.pure[List], i)
              ignore { store.removeAllJoins(txn, c) }
          }
          (k, acis.map(_._1))
      }
    }
}
