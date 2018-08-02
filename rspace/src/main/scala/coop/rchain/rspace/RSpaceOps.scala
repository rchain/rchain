package coop.rchain.rspace

import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib._
import coop.rchain.rspace.history.{Branch, Leaf}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.Consume
import coop.rchain.shared.SyncVarOps._

import scala.Function.const
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar
import scala.util.Random

import kamon._
import kamon.trace.Tracer.SpanBuilder

abstract class RSpaceOps[C, P, A, R, K](val store: IStore[C, P, A, K], val branch: Branch)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeK: Serialize[K]
) extends ISpace[C, P, A, R, K] {

  protected[this] val logger: Logger
  protected[this] val installSpan: SpanBuilder

  private[this] val installs: SyncVar[Installs[C, P, A, R, K]] = {
    val installs = new SyncVar[Installs[C, P, A, R, K]]()
    installs.put(Map.empty)
    installs
  }

  protected[this] def restoreInstalls(txn: store.Transaction): Unit =
    installs.get.foreach {
      case (channels, Install(patterns, continuation, _match)) =>
        install(txn, channels, patterns, continuation)(_match)
    }

  private[this] def install(txn: store.Transaction,
                            channels: Seq[C],
                            patterns: Seq[P],
                            continuation: K)(implicit m: Match[P, A, R]): Option[(K, Seq[R])] = {
    if (channels.length =!= patterns.length) {
      val msg = "channels.length must equal patterns.length"
      logger.error(msg)
      throw new IllegalArgumentException(msg)
    }
    logger.debug(s"""|install: searching for data matching <patterns: $patterns>
                     |at <channels: $channels>""".stripMargin.replace('\n', ' '))

    val consumeRef = Consume.create(channels, patterns, continuation, true)

    /*
     * Here, we create a cache of the data at each channel as `channelToIndexedData`
     * which is used for finding matches.  When a speculative match is found, we can
     * remove the matching datum from the remaining data candidates in the cache.
     *
     * Put another way, this allows us to speculatively remove matching data without
     * affecting the actual store contents.
     */

    val channelToIndexedData = channels.map { (c: C) =>
      c -> Random.shuffle(store.getData(txn, Seq(c)).zipWithIndex)
    }.toMap

    val options: Option[Seq[DataCandidate[C, R]]] =
      extractDataCandidates(channels.zip(patterns), channelToIndexedData, Nil).sequence

    options match {
      case None =>
        installs.update(_.updated(channels, Install(patterns, continuation, m)))
        store.installWaitingContinuation(
          txn,
          channels,
          WaitingContinuation(patterns, continuation, persist = true, consumeRef))
        for (channel <- channels) store.addJoin(txn, channel, channels)
        logger.debug(s"""|storing <(patterns, continuation): ($patterns, $continuation)>
                         |at <channels: $channels>""".stripMargin.replace('\n', ' '))
        None
      case Some(_) =>
        throw new RuntimeException("Installing can be done only on startup")
    }

  }

  override def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, A, R]): Option[(K, Seq[R])] =
    Kamon.withSpan(installSpan.start(), finishSpan = true) {
      store.withTxn(store.createTxnWrite()) { txn =>
        install(txn, channels, patterns, continuation)
      }
    }

  override def retrieve(root: Blake2b256Hash,
                        channelsHash: Blake2b256Hash): Option[GNAT[C, P, A, K]] =
    history.lookup(store.trieStore, root, channelsHash)

  override def reset(root: Blake2b256Hash): Unit =
    store.withTxn(store.createTxnWrite()) { txn =>
      store.withTrieTxn(txn) { trieTxn =>
        store.trieStore.validateAndPutRoot(trieTxn, store.trieBranch, root)
        val leaves = store.trieStore.getLeaves(trieTxn, root)
        eventLog.update(const(Seq.empty))
        store.clearTrieUpdates()
        store.clear(txn)
        restoreInstalls(txn)
        store.bulkInsert(txn, leaves.map { case Leaf(k, v) => (k, v) })
      }
    }

  override def clear(): Unit = {
    val emptyRootHash: Blake2b256Hash =
      store.withTxn(store.createTxnRead()) { txn =>
        store.withTrieTxn(txn) { trieTxn =>
          store.trieStore.getAllPastRoots(trieTxn).last
        }
      }
    reset(emptyRootHash)
  }
}
