package coop.rchain.rspace.history

import coop.rchain.rspace.hashing.Blake2b256Hash

import scala.collection.concurrent.TrieMap
import monix.eval.Task

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

trait InMemoryHistoryTestBase {

  def runEffect[A](task: Task[A]): A = task.runSyncUnsafe(20.seconds)

  def inMemHistoryStore: HistoryStore[Task] = new HistoryStore[Task] {
    val data: TrieMap[Blake2b256Hash, Trie] = TrieMap.empty

    override def put(tries: List[Trie]): Task[Unit] = Task.delay {
      tries.foreach { t =>
        val key = Trie.hash(t)
        data.put(key, t)
      }
    }

    override def get(key: Blake2b256Hash): Task[Trie] =
      Task.delay { data.getOrElse(key, EmptyTrie) }

  }
}
