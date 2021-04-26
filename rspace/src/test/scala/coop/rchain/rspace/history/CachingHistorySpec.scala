package coop.rchain.rspace.history

import cats.implicits.catsSyntaxApplicativeId
import coop.rchain.rspace.history.HistoryInstances.{CachingHistoryStore, MergingHistory}
import coop.rchain.rspace.history.TestData._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.concurrent.duration._

class CachingHistorySpec
    extends FlatSpec
    with Matchers
    with OptionValues
    with InMemoryHistoryTestBase {

  def create: (MergingHistory[Task], CachingHistoryStore[Task]) = {
    val historyStore = inMemHistoryStore
    val caching      = CachingHistoryStore(historyStore)
    val history      = new MergingHistory[Task](History.emptyRootHash, caching)
    (history, caching)
  }

  protected def withEmptyTrie(
      f: (MergingHistory[Task], CachingHistoryStore[Task]) => Task[Unit]
  ): Unit = {
    val (history, caching) = create
    runEffect(f(history, caching))
  }

  /**
    * the expected shape of the in-mem history trie is:
    *
    * PB(0)
    *  |
    * S(28x0)
    *  |
    * PB(0, 1, 2)
    *  | (2)
    * S(000)
    *  |
    * Leaf (leafs aren't cached)
  **/
  "processing of multiple mixed actions" should "remove data from cache" in withEmptyTrie {
    (history, cache) =>
      def setupActions() =
        insert(prefixWithZeros("0001")) ::
          insert(prefixWithZeros("0010")) ::
          delete(prefixWithZeros("0011")) ::
          insert(prefixWithZeros("1000")) ::
          delete(prefixWithZeros("1001")) ::
          insert(prefixWithZeros("2000")) ::
          Nil
      val zero = setupActions()
      for {
        _ <- history.processSubtree(EmptyTrie)(0.toByte, zero)
        _ = cache.cache.size shouldBe 4
      } yield ()
  }
}
