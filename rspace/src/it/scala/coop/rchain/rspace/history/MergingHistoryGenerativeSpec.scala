package coop.rchain.rspace.history

import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.GeneratorUtils.distinctListOf
import monix.eval.Task
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop._
import org.scalatest.{FlatSpec, Matchers, OptionValues}

class MergingHistoryGenerativeSpec
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
    distinctListOf(arbitraryRandomThreeBytes)
  ) { keys: List[Key] =>
    val actions = keys.map(k => (k, MergingTestData.randomBlake))
    val emptyMergingHistory =
      HistoryMergingInstances
        .merging[Task](HistoryMergingInstances.emptyRootHash, inMemHistoryStore)

    val emptySimplisticHistory: HistoryWithFind[Task] =
      SimplisticHistory.noMerging[Task](HistoryMergingInstances.emptyRootHash, inMemHistoryStore)

    val emptyState = Map.empty[Key, (Data, HistoryWithFind[Task], HistoryWithFind[Task])] // accumulate actions performed on the trie

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
          runEffect(postModifyMergingHistory.process(actions))
        val postDeleteSimplisticHistory =
          runEffect(postModifySimplisticHistory.process(actions))

        fetchData(postModifyMergingHistory, k)._1 shouldBe LeafPointer(v)
        fetchData(postDeleteMergingHistory, k)._1 shouldBe EmptyPointer

        fetchData(postModifySimplisticHistory, k)._1 shouldBe LeafPointer(v)
        fetchData(postDeleteSimplisticHistory, k)._1 shouldBe EmptyPointer
        postDeleteSimplisticHistory.root shouldBe postDeleteMergingHistory.root
    }

    val deletions           = map.values.map(_._1).map(v => DeleteAction(v._1)).toList
    val finalMergingHistory = resultMergingHistory.process(deletions)
    runEffect(finalMergingHistory).root shouldBe HistoryMergingInstances.emptyRootHash

    val finalSimplisticHistory = resultSimplisticHistory.process(deletions)
    runEffect(finalSimplisticHistory).root shouldBe HistoryMergingInstances.emptyRootHash
  }

  "process" should "accept new leafs in bulk" in forAll(distinctListOf(arbitraryRandomThreeBytes)) {
    keys: List[Key] =>
      val actions = keys.map(k => (k, MergingTestData.randomBlake))
      val emptyMergingHistory = HistoryMergingInstances
        .merging[Task](HistoryMergingInstances.emptyRootHash, inMemHistoryStore)
      val emptySimplisticHistory: HistoryWithFind[Task] =
        SimplisticHistory.noMerging[Task](HistoryMergingInstances.emptyRootHash, inMemHistoryStore)

      val inserts                  = actions.map { case (k, v) => InsertAction(k, v) }
      val postInsertMergingHistory = runEffect(emptyMergingHistory.process(inserts))
      val postInsertNonMergingHistory =
        runEffect(emptySimplisticHistory.process(inserts))

      postInsertMergingHistory.root shouldBe postInsertNonMergingHistory.root

      val deletions = actions.map { case (k, _) => DeleteAction(k) }

      val postDeletionMergingHistory =
        runEffect(postInsertMergingHistory.process(deletions))
      val postDeletionNonMergingHistory =
        runEffect(postInsertNonMergingHistory.process(deletions))

      postDeletionMergingHistory.root shouldBe HistoryMergingInstances.emptyRootHash
      postDeletionNonMergingHistory.root shouldBe HistoryMergingInstances.emptyRootHash
  }

  val arbitraryRandomThreeBytes: Arbitrary[Key] =
    Arbitrary(
      Gen
        .listOfN(3, Arbitrary.arbitrary[Int])
        .map(ints => (List.fill(29)(0) ++ ints).map(_.toByte))
    )

  def insertAndVerify(
      history: HistoryWithFind[Task],
      toBeProcessed: List[Data],
      pastData: List[Data]
  ): (HistoryWithFind[Task], List[Data]) = {
    val inserts      = toBeProcessed.map(v => InsertAction(v._1, v._2))
    val insertResult = runEffect(history.process(inserts))
    insertResult.root should not be history.root

    val deletions      = toBeProcessed.map(v => DeleteAction(v._1))
    val deletionResult = runEffect(insertResult.process(deletions))
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

  def fetchData(h: HistoryWithFind[Task], data: Data): (TriePointer, Vector[Trie]) =
    fetchData(h, data._1)

  def fetchData(h: HistoryWithFind[Task], k: Key): (TriePointer, Vector[Trie]) =
    runEffect(h.find(k))
}
