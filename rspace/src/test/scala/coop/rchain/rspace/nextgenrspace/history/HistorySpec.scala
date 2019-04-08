package coop.rchain.rspace.nextgenrspace.history
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.test.TestKey32
import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.collection.concurrent.TrieMap
import History._
import org.scalacheck.{Arbitrary, Gen, Shrink}
import coop.rchain.rspace.test.ArbitraryInstances._
import monix.eval.Task
import org.scalatest.prop._
import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

class HistorySpec
    extends FlatSpec
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with InMemoryHistoryTestBase {

  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  implicit val propertyCheckConfigParam = PropertyCheckConfiguration(minSuccessful = 1000)

  "history" should "accept new leafs (insert, update, delete)" in forAll {
    actions: List[(TestKey32, Blake2b256Hash)] =>
      val emptyHistory =
        new History[Task, TestKey32](emptyRootHash, inMemHistoryStore, inMemPointerBlockStore)

      val emptyState =
        Map.empty[
          TestKey32,
          ((TestKey32, Blake2b256Hash), History[Task, TestKey32], History[Task, TestKey32])
        ]

      val (result, map) = actions.foldLeft((emptyHistory, emptyState)) {
        case ((history, prevActions), action) =>
          val (postHistory, _) =
            insertAndVerify(history, action :: Nil, prevActions.values.map(_._1).toList)
          val actions = prevActions + (action._1 -> (action, history, postHistory))
          (postHistory, actions)
      }

      map.values.foreach {
        case ((k, v), _, postModifyHistory) =>
          val actions           = DeleteAction(k) :: Nil
          val postDeleteHistory = postModifyHistory.process(actions).runSyncUnsafe(20.seconds)

          postModifyHistory
            .traverseTrie(k.bytes.toSeq.toList)
            .runSyncUnsafe(20.seconds)
            ._1 shouldBe Leaf(v)
          postDeleteHistory
            .traverseTrie(k.bytes.toSeq.toList)
            .runSyncUnsafe(20.seconds)
            ._1 shouldBe EmptyTrie
      }

      val deletions    = map.values.map(_._1).map(v => DeleteAction(v._1)).toList
      val finalHistory = result.process(deletions)
      finalHistory.runSyncUnsafe(20.seconds).root shouldBe emptyHistory.root
  }

  val arbitraryTestKey32: Arbitrary[TestKey32] =
    Arbitrary(Gen.sized { _ =>
      Gen
        .listOfN(32, Arbitrary.arbitrary[Int])
        .map(ints => TestKey32.create(ints))
    })

  val arbitrary2TestKey32: Arbitrary[TestKey32] =
    Arbitrary(Gen.sized { _ =>
      Gen
        .listOfN(3, Arbitrary.arbitrary[Int])
        .map(ints => TestKey32.create(List.fill(29)(0) ++ ints))
    })

  implicit val arbitraryInsertAction: Arbitrary[(TestKey32, Blake2b256Hash)] =
    Arbitrary(for {
      key  <- arbitrary2TestKey32.arbitrary
      hash <- arbitraryBlake2b256Hash.arbitrary
    } yield (key, hash))

  def insertAndVerify(
      history: History[Task, TestKey32],
      data: List[(TestKey32, Blake2b256Hash)],
      toVerify: List[(TestKey32, Blake2b256Hash)]
  ): (History[Task, TestKey32], List[(TestKey32, Blake2b256Hash)]) = {
    val actions = data.map(v => InsertAction(v._1, v._2))
    val result  = history.process(actions).runSyncUnsafe(20.seconds)
    result.root should not be history.root

    val deletions = data.map(v => DeleteAction(v._1))
    val deletionResult =
      result.process(deletions).runSyncUnsafe(20.seconds)
    deletionResult
      .traverseTrie(data.head._1.asBytes)
      .runSyncUnsafe(20.seconds)
      ._1 shouldBe EmptyTrie

    val notDeleted = toVerify.filter(v => v._1 != data.head._1)
    notDeleted.foreach(action => {
      deletionResult.traverseTrie(action._1.asBytes).runSyncUnsafe(20.seconds)._1 shouldBe Leaf(
        action._2
      )
    })

    val all = toVerify.toMap ++ data.toMap
    all.foreach(action => {
      result.traverseTrie(action._1.asBytes).runSyncUnsafe(20.seconds)._1 shouldBe Leaf(action._2)
    })
    (result, all.toList)
  }

}

trait InMemoryHistoryTestBase {
  def inMemPointerBlockStore: PointerBlockStore[Task] = new PointerBlockStore[Task] {
    val data: TrieMap[Blake2b256Hash, PointerBlock] = TrieMap.empty

    override def put(key: Blake2b256Hash, pb: PointerBlock): Task[Unit] =
      Task.delay { data.put(key, pb) }

    override def get(key: Blake2b256Hash): Task[Option[PointerBlock]] =
      Task.delay { data.get(key) }
  }

  def inMemHistoryStore: HistoryStore[Task] = new HistoryStore[Task] {
    val data: TrieMap[Blake2b256Hash, Trie] = TrieMap.empty
    override def put(tries: List[Trie]): Task[Unit] = Task.delay {
      tries.foreach { t =>
        val key = Trie.hash(t)
        data.put(key, t)
      }
    }
    override def get(key: Blake2b256Hash): Task[Trie] = Task.delay {
      data.getOrElse(key, EmptyTrie)
    }
  }

  def insertAction(key: Seq[Int], value: Blake2b256Hash): (TestKey32, Blake2b256Hash) =
    (TestKey32.create(key), value)

  val emptyRoot: Trie               = EmptyTrie
  val emptyRootHash: Blake2b256Hash = Trie.hash(emptyRoot)
}
