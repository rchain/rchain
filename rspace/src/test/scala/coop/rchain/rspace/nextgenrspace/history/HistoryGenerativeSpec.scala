package coop.rchain.rspace.nextgenrspace.history
import coop.rchain.rspace.Blake2b256Hash
import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.collection.concurrent.TrieMap
import History._
import org.scalacheck.{Arbitrary, Gen, Shrink}
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.shared.GeneratorUtils.distinctListOf
import monix.eval.Task
import org.scalatest.prop._

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

class HistoryGenerativeSpec
    extends FlatSpec
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with InMemoryHistoryTestBase {

  // disable shrinking since this test operates on a persistent structure
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  implicit val propertyCheckConfigParam =
    PropertyCheckConfiguration(minSuccessful = 1000)

  type Key  = List[Byte]
  type Data = (Key, Blake2b256Hash)

  "history" should "accept new leafs (insert, update, delete)" in forAll(
    distinctListOf(arbitraryInsertAction)
  ) { actions: List[Data] =>
    val emptyHistory =
      HistoryInstances.noMerging[Task](emptyRootHash, inMemHistoryStore, inMemPointerBlockStore)

    val emptyState = Map.empty[Key, (Data, History[Task])] // accumulate actions performed on the trie

    val (result, map) = actions.foldLeft((emptyHistory, emptyState)) {
      case ((history, prevActions), action) =>
        val (postHistory, _) =
          insertAndVerify(history, action :: Nil, prevActions.values.map(_._1).toList)
        val actions = prevActions + (action._1 -> (action, postHistory))
        (postHistory, actions)
    }

    map.values.foreach {
      case ((k, v), postModifyHistory) =>
        val actions           = DeleteAction(k) :: Nil
        val postDeleteHistory = postModifyHistory.process(actions).runSyncUnsafe(20.seconds)

        fetchData(postModifyHistory, k)._1 shouldBe Leaf(v)
        fetchData(postDeleteHistory, k)._1 shouldBe EmptyTrie
    }

    val deletions    = map.values.map(_._1).map(v => DeleteAction(v._1)).toList
    val finalHistory = result.process(deletions)
    finalHistory.runSyncUnsafe(20.seconds).root shouldBe emptyHistory.root
  }

  val arbitraryRandomThreeBytes: Arbitrary[Key] =
    Arbitrary(
      Gen
        .listOfN(3, Arbitrary.arbitrary[Int])
        .map(ints => (List.fill(29)(0) ++ ints).map(_.toByte))
    )

  implicit val arbitraryInsertAction: Arbitrary[Data] =
    Arbitrary(for {
      key  <- arbitraryRandomThreeBytes.arbitrary
      hash <- arbitraryBlake2b256Hash.arbitrary
    } yield (key, hash))

  def insertAndVerify(
      history: History[Task],
      toBeProcessed: List[Data],
      pastData: List[Data]
  ): (History[Task], List[Data]) = {
    val inserts      = toBeProcessed.map(v => InsertAction(v._1, v._2))
    val insertResult = history.process(inserts).runSyncUnsafe(20.seconds)
    insertResult.root should not be history.root

    val deletions      = toBeProcessed.map(v => DeleteAction(v._1))
    val deletionResult = insertResult.process(deletions).runSyncUnsafe(20.seconds)
    fetchData(deletionResult, toBeProcessed.head)._1 shouldBe EmptyTrie

    val notDeletedData = pastData.filter(v => v._1 != toBeProcessed.head._1)
    notDeletedData.foreach(action => {
      fetchData(deletionResult, action)._1 shouldBe Leaf(action._2)
    })

    val allUniqueData = pastData.toMap ++ toBeProcessed.toMap
    allUniqueData.foreach(action => {
      fetchData(insertResult, action)._1 shouldBe Leaf(action._2)
    })
    (insertResult, allUniqueData.toList)
  }

  def fetchData(h: History[Task], data: Data): (Trie, TriePath) =
    fetchData(h, data._1)

  def fetchData(h: History[Task], k: Key): (Trie, TriePath) =
    h.findPath(k).runSyncUnsafe(20.seconds)
}

trait InMemoryHistoryTestBase {
  def inMemPointerBlockStore: PointerBlockStore[Task] = new PointerBlockStore[Task] {
    val data: TrieMap[Blake2b256Hash, PointerBlock] = TrieMap.empty

    override def put(key: Blake2b256Hash, pb: PointerBlock): Task[Unit] =
      Task.delay { data.put(key, pb) }

    override def get(key: Blake2b256Hash): Task[Option[PointerBlock]] =
      Task.delay { data.get(key) }

    override def close(): Task[Unit] = Task.now(())
  }

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

    override def close(): Task[Unit] = Task.now(())
  }
}
