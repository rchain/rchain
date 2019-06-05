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

  "process" should "accept new leafs (insert, update, delete)" in forAll(
    distinctListOf(arbitraryInsertAction)
  ) { actions: List[Data] =>
    val emptyMergingHistory = HistoryInstances.merging[Task](emptyRootHash, inMemHistoryStore)

    val emptySimplisticHistory: History[Task] =
      SimplisticHistory.noMerging[Task](emptyRootHash, inMemHistoryStore)

    val emptyState = Map.empty[Key, (Data, History[Task], History[Task])] // accumulate actions performed on the trie

    val (resultMergingHistory, resultSimplisticHistory, map) =
      actions.foldLeft((emptyMergingHistory, emptySimplisticHistory, emptyState)) {
        case ((mergingHistory, simplisticHistory, prevActions), action) =>
          val (postMergingHistory, _) =
            insertAndVerify(mergingHistory, action :: Nil, prevActions.values.map(_._1).toList)
          val (postSimplisticHistory, _) =
            insertAndVerify(simplisticHistory, action :: Nil, prevActions.values.map(_._1).toList)

          postSimplisticHistory.root shouldBe postMergingHistory.root
          val actions = prevActions + (action._1 -> (action, postMergingHistory, postSimplisticHistory))
          (postMergingHistory, postSimplisticHistory, actions)
      }

    map.values.foreach {
      case ((k, v), postModifyMergingHistory, postModifySimplisticHistory) =>
        val actions = DeleteAction(k) :: Nil
        val postDeleteMergingHistory =
          postModifyMergingHistory.process(actions).runSyncUnsafe(20.seconds)
        val postDeleteSimplisticHistory =
          postModifySimplisticHistory.process(actions).runSyncUnsafe(20.seconds)

        fetchData(postModifyMergingHistory, k)._1 shouldBe LeafPointer(v)
        fetchData(postDeleteMergingHistory, k)._1 shouldBe EmptyPointer

        fetchData(postModifySimplisticHistory, k)._1 shouldBe LeafPointer(v)
        fetchData(postDeleteSimplisticHistory, k)._1 shouldBe EmptyPointer
        postDeleteSimplisticHistory.root shouldBe postDeleteMergingHistory.root
    }

    val deletions           = map.values.map(_._1).map(v => DeleteAction(v._1)).toList
    val finalMergingHistory = resultMergingHistory.process(deletions)
    finalMergingHistory.runSyncUnsafe(20.seconds).root shouldBe emptyRootHash

    val finalSimplisticHistory = resultSimplisticHistory.process(deletions)
    finalSimplisticHistory.runSyncUnsafe(20.seconds).root shouldBe emptyRootHash
  }

  "process" should "accept new leafs in bulk" in forAll(
    distinctListOf(arbitraryInsertAction)
  ) { actions: List[Data] =>
    val emptyMergingHistory = HistoryInstances.merging[Task](emptyRootHash, inMemHistoryStore)

    val emptySimplisticHistory: History[Task] =
      SimplisticHistory.noMerging[Task](emptyRootHash, inMemHistoryStore)

    val inserts                  = actions.map { case (k, v) => InsertAction(k, v) }
    val postInsertMergingHistory = emptyMergingHistory.process(inserts).runSyncUnsafe(20.seconds)
    val postInsertNonMergingHistory =
      emptySimplisticHistory.process(inserts).runSyncUnsafe(20.seconds)

    postInsertMergingHistory.root shouldBe postInsertNonMergingHistory.root

    val deletions = actions.map { case (k, _) => DeleteAction(k) }

    val postDeletionMergingHistory =
      postInsertMergingHistory.process(deletions).runSyncUnsafe(20.seconds)
    val postDeletionNonMergingHistory =
      postInsertNonMergingHistory.process(deletions).runSyncUnsafe(20.seconds)

    postDeletionMergingHistory.root shouldBe emptyRootHash
    postDeletionNonMergingHistory.root shouldBe emptyRootHash
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
    fetchData(deletionResult, toBeProcessed.head)._1 shouldBe EmptyPointer

    val notDeletedData = pastData.filter(v => v._1 != toBeProcessed.head._1)
    notDeletedData.foreach(action => {
      fetchData(deletionResult, action)._1 shouldBe LeafPointer(action._2)
    })

    val allUniqueData = pastData.toMap ++ toBeProcessed.toMap
    allUniqueData.foreach(action => {
      fetchData(insertResult, action)._1 shouldBe LeafPointer(action._2)
    })
    (insertResult, allUniqueData.toList)
  }

  def fetchData(h: History[Task], data: Data): (TriePointer, Vector[Trie]) =
    fetchData(h, data._1)

  def fetchData(h: History[Task], k: Key): (TriePointer, Vector[Trie]) =
    h.find(k).runSyncUnsafe(20.seconds)
}

trait InMemoryHistoryTestBase {

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
