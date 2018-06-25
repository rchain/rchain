package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import coop.rchain.rspace.test._
import org.lmdbjava.{Env, Txn}
import org.scalacheck.Arbitrary
import org.scalatest.{BeforeAndAfterAll, Suite}
import scodec.Codec
import scodec.codecs._
import scodec.bits.ByteVector

abstract class HistoryActionsTests[T] extends HistoryTestsBase[T, TestKey4, ByteVector] {

  implicit val codecV: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)
  implicit val codecK: Codec[TestKey4]   = TestKey4.codecTestKey

  import TestData._

  "Insert, lookup" should "work" in withTestTrieStore {
    implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key1, val1)
      val actual = lookup(branch, key1)
      actual.value shouldBe val1
  }

  "Two inserts, two lookups" should "work" in withTestTrieStore {
    implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key1, val1)
      insert(branch, key2, val2)
      val actual1 = lookup(branch, key1)
      val actual2 = lookup(branch, key2)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
  }

  "Duplicate inserts, lookup" should "work" in withTestTrieStore {
    implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key1, val1)
      val root1 = getRoot(store, branch)
      insert(branch, key1, val1)
      val root2 = getRoot(store, branch)
      root2 shouldBe root1
      lookup(branch, key1).value shouldBe val1
  }

  "Two inserts at same key with different values, lookup" should "work" in withTestTrieStore {
    implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key1, val1)
      val root1 = getRoot(store, branch)
      insert(branch, key1, val2)
      val root2 = getRoot(store, branch)
      root2 should not be root1
      lookup(branch, key1).value shouldBe val2
  }

  "Three inserts, three lookups" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key1, val1)
      insert(branch, key2, val2)
      insert(branch, key3, val3)
      val actual1 = lookup(branch, key1)
      val actual2 = lookup(branch, key2)
      val actual3 = lookup(branch, key3)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
      actual3.value shouldBe val3
    }

  "Three inserts, three lookups (alt)" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key1, val1)
      insert(branch, key2, val2)
      insert(branch, key6, val6)
      val actual1 = lookup(branch, key1)
      val actual2 = lookup(branch, key2)
      val actual3 = lookup(branch, key6)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
      actual3.value shouldBe val6
    }

  "Four inserts, four lookups" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key1, val1)
      insert(branch, key2, val2)
      insert(branch, key3, val3)
      insert(branch, key4, val4)
      val actual1 = lookup(branch, key1)
      val actual2 = lookup(branch, key2)
      val actual3 = lookup(branch, key3)
      val actual4 = lookup(branch, key4)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
      actual3.value shouldBe val3
      actual4.value shouldBe val4
    }

  "Five inserts, five lookups" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key1, val1)
      insert(branch, key2, val2)
      insert(branch, key3, val3)
      insert(branch, key4, val4)
      insert(branch, key5, val5)
      val actual1 = lookup(branch, key1)
      val actual2 = lookup(branch, key2)
      val actual3 = lookup(branch, key3)
      val actual4 = lookup(branch, key4)
      val actual5 = lookup(branch, key5)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
      actual3.value shouldBe val3
      actual4.value shouldBe val4
      actual5.value shouldBe val5
    }

  "Insert, lookup, delete, lookup" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key1, val1)
      lookup(branch, key1).value shouldBe val1

      delete(branch, key1, val1) shouldBe true
      lookup(branch, key1) shouldBe None
    }

  "Delete, lookup" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      delete(branch, key1, val1) shouldBe false
      lookup(branch, key1) shouldBe None
    }

  "Delete" should "return false for a missing (key, value)" in {
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      insert(branch, key2, val2)
      delete(branch, key2, val2) shouldBe true
      delete(branch, key2, val2) shouldBe false
    }
  }

  "Insert, insert, delete, delete" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      val root0 = getRoot(store, branch)

      insert(branch, key1, val1)
      val root1 = getRoot(store, branch)
      root1 should not be root0

      insert(branch, key2, val2)
      val root2 = getRoot(store, branch)
      root2 should not be root1

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2).value shouldBe val2

      delete(branch, key2, val2) shouldBe true
      val root3 = getRoot(store, branch)
      root3 shouldBe root1

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2) shouldBe None

      delete(branch, key1, val1) shouldBe true
      val root4 = getRoot(store, branch)
      root4 shouldBe root0

      lookup(branch, key1) shouldBe None
      lookup(branch, key2) shouldBe None
    }

  "Insert, delete, delete again" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      val root0 = getRoot(store, branch)

      insert(branch, key1, val1)
      lookup(branch, key1).value shouldBe val1
      getRoot(store, branch) should not be root0

      delete(branch, key1, val1) shouldBe true
      lookup(branch, key1) shouldBe None
      getRoot(store, branch) shouldBe root0

      delete(branch, key1, val1) shouldBe false
      lookup(branch, key1) shouldBe None
      getRoot(store, branch) shouldBe root0
    }

  "Insert, Insert, Delete, Delete, Rollback, Delete, Delete" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      val root0 = getRoot(store, branch)

      insert(branch, key1, val1)
      val root1 = getRoot(store, branch)
      root1 should not be root0

      insert(branch, key2, val2)
      val root2 = getRoot(store, branch)
      root2 should not be root1

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2).value shouldBe val2

      delete(branch, key2, val2) shouldBe true
      getRoot(store, branch) shouldBe root1

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2) shouldBe None

      delete(branch, key1, val1) shouldBe true
      getRoot(store, branch) shouldBe root0

      lookup(branch, key1) shouldBe None
      lookup(branch, key2) shouldBe None

      // Aaannnd rollback...
      root2.foreach(setRoot(store, branch, _))

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2).value shouldBe val2

      delete(branch, key2, val2) shouldBe true
      getRoot(store, branch) shouldBe root1

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2) shouldBe None

      delete(branch, key1, val1) shouldBe true
      getRoot(store, branch) shouldBe root0

      lookup(branch, key1) shouldBe None
      lookup(branch, key2) shouldBe None
    }

  "Rollback twice" should "work" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      val root0 = getRoot(store, branch)

      insert(branch, key1, val1)
      val root1 = getRoot(store, branch)
      root1 should not be root0

      insert(branch, key2, val2)
      val root2 = getRoot(store, branch)
      root2 should not be root1

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2).value shouldBe val2

      delete(branch, key2, val2) shouldBe true
      getRoot(store, branch) shouldBe root1

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2) shouldBe None

      delete(branch, key1, val1) shouldBe true
      getRoot(store, branch) shouldBe root0

      lookup(branch, key1) shouldBe None
      lookup(branch, key2) shouldBe None

      // Aaannnd rollback...
      root2.foreach(setRoot(store, branch, _))

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2).value shouldBe val2
      lookup(branch, key3) shouldBe None

      insert(branch, key3, val3)
      val root3 = getRoot(store, branch)
      root3 should not be root2

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2).value shouldBe val2
      lookup(branch, key3).value shouldBe val3

      insert(branch, key4, val4)
      val root4 = getRoot(store, branch)
      root4 should not be root3

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2).value shouldBe val2
      lookup(branch, key3).value shouldBe val3
      lookup(branch, key4).value shouldBe val4

      // rollback again
      root3.foreach(setRoot(store, branch, _))

      lookup(branch, key1).value shouldBe val1
      lookup(branch, key2).value shouldBe val2
      lookup(branch, key3).value shouldBe val3
      lookup(branch, key4) shouldBe None

      insert(branch, key4, val4)
      val root5 = getRoot(store, branch)
      root5 shouldBe root4
    }

  "getLeaves on an empty store" should "return an empty sequence" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      val leaves = getRoot(store, branch).map(getLeaves(store, _))
      leaves.value shouldBe empty
    }

  "insert 6 things and getLeaves" should "return all of the leaves" in
    withTestTrieStore { implicit store: ITrieStore[T, TestKey4, ByteVector] => branch: Branch =>
      val expected: Vector[Leaf[TestKey4, ByteVector]] = Vector(
        Leaf(key1, val1),
        Leaf(key2, val2),
        Leaf(key3, val3),
        Leaf(key4, val4),
        Leaf(key5, val5),
        Leaf(key6, val6)
      )

      insert(branch, key1, val1)
      insert(branch, key2, val2)
      insert(branch, key3, val3)
      insert(branch, key4, val4)
      insert(branch, key5, val5)
      insert(branch, key6, val6)

      val leaves = getRoot(store, branch).map(getLeaves(store, _))

      leaves.value should contain theSameElementsAs expected
    }
}

trait GenerativeHistoryActionsTests[T, K]
    extends HistoryTestsBase[T, K, ByteVector]
    with WithTestStore[T, K, ByteVector] {

  implicit def arbitraryMap: Arbitrary[Map[K, ByteVector]]

  "Round trip rollback" should "work" in {
    forAll { (kvs: Map[K, ByteVector]) =>
      withTestTrieStore { implicit store => branch =>
        val pairs = kvs.toList

        val (first, second) = pairs.splitAt(pairs.length / 2)

        val root0 = getRoot(store, branch)

        HistoryActionsTests.insertAll(branch, first)
        val root1 = getRoot(store, branch)
        val ret1  = HistoryActionsTests.lookupAll(branch, first)

        ret1 shouldBe first

        HistoryActionsTests.insertAll(branch, second)
        val root2 = getRoot(store, branch)
        val ret2f = HistoryActionsTests.lookupAll(branch, first)
        val ret2s = HistoryActionsTests.lookupAll(branch, second)

        root2 should not be root1
        ret2f shouldBe first
        ret2s shouldBe second

        HistoryActionsTests.deleteAll(branch, second)
        val root3 = getRoot(store, branch)
        val ret3f = HistoryActionsTests.lookupAll(branch, first)
        val ret3s = HistoryActionsTests.lookupAll(branch, second)

        root3 shouldBe root1
        ret3f shouldBe first
        ret3s shouldBe empty

        HistoryActionsTests.deleteAll(branch, first)
        val root4 = getRoot(store, branch)
        val ret4f = HistoryActionsTests.lookupAll(branch, first)
        val ret4s = HistoryActionsTests.lookupAll(branch, second)

        root4 shouldBe root0
        ret4f shouldBe empty
        ret4s shouldBe empty

        root2.foreach(setRoot(store, branch, _))

        HistoryActionsTests.deleteAll(branch, second)
        val root5 = getRoot(store, branch)
        val ret5f = HistoryActionsTests.lookupAll(branch, first)
        val ret5s = HistoryActionsTests.lookupAll(branch, second)

        root5 shouldBe root1
        ret5f shouldBe first
        ret5s shouldBe empty

        HistoryActionsTests.deleteAll(branch, first)
        val root6 = getRoot(store, branch)
        val ret6f = HistoryActionsTests.lookupAll(branch, first)
        val ret6s = HistoryActionsTests.lookupAll(branch, second)

        root6 shouldBe root0
        ret6f shouldBe empty
        ret6s shouldBe empty
      }
    }
  }

  "insert a bunch of things and getLeaves" should "return all of the leaves" in
    forAll { (kvs: Map[K, ByteVector]) =>
      withTestTrieStore { implicit store => branch =>
        val expected = kvs.map { case (k, v) => Leaf(k, v) }
        kvs.foreach { case (k, v) => insert(branch, k, v) }
        val leaves = getRoot(store, branch).map(getLeaves(store, _))
        leaves.value should contain theSameElementsAs expected
      }
    }
}

object HistoryActionsTests {

  def insertAll[T, K, V](branch: Branch, pairs: Seq[(K, V)])(implicit
                                                             store: ITrieStore[T, K, V],
                                                             codecK: Codec[K],
                                                             codecV: Codec[V]): Unit =
    pairs.foreach { case (k, v) => insert(branch, k, v) }

  def deleteAll[T, K, V](branch: Branch, pairs: Seq[(K, V)])(implicit
                                                             store: ITrieStore[T, K, V],
                                                             codecK: Codec[K],
                                                             codecV: Codec[V]): Unit =
    pairs.foreach { case (k, v) => delete(branch, k, v) }

  def lookupAll[T, K, V](branch: Branch, pairs: Seq[(K, V)])(implicit store: ITrieStore[T, K, V],
                                                             codecK: Codec[K]): Seq[(K, V)] =
    pairs.flatMap { case (k, _) => lookup(branch, k).map((v: V) => (k, v)).toList }
}

trait LMDBWithTestTrieStore[K]
    extends BeforeAndAfterAll
    with WithTestStore[Txn[ByteBuffer], K, ByteVector] { this: Suite =>
  val dbDir: Path   = Files.createTempDirectory("rchain-storage-history-test-")
  val mapSize: Long = 1024L * 1024L * 1024L

  override def withTestTrieStore[R](
      f: (ITrieStore[Txn[ByteBuffer], K, ByteVector], Branch) => R): R =
    withTestTrieStore(f.curried)

  override def withTestTrieStore[R](
      f: ITrieStore[Txn[ByteBuffer], K, ByteVector] => Branch => R): R = {
    val env: Env[ByteBuffer] =
      Env
        .create()
        .setMapSize(mapSize)
        .setMaxDbs(2)
        .setMaxReaders(126)
        .open(dbDir.toFile)
    implicit val testStore = LMDBTrieStore.create[K, ByteVector](env)
    val testBranch         = Branch("test")
    testStore.withTxn(testStore.createTxnWrite())(txn => testStore.clear(txn))
    try {
      initialize(testBranch)
      f(testStore)(testBranch)
    } finally {
      testStore.close()
      env.close()
    }
  }

  override def afterAll(): Unit =
    recursivelyDeletePath(dbDir)
}

class LMDBHistoryActionsTests
    extends HistoryActionsTests[Txn[ByteBuffer]]
    with GenerativeHistoryActionsTests[Txn[ByteBuffer], TestKey4]
    with LMDBWithTestTrieStore[TestKey4] {
  implicit val arbitraryMap = ArbitraryInstances.arbitraryNonEmptyMapTestKeyByteVector
}

class LMDBHistoryActionsTestsKey32
    extends GenerativeHistoryActionsTests[Txn[ByteBuffer], TestKey32]
    with LMDBWithTestTrieStore[TestKey32] {
  implicit val codecV: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)
  implicit val codecK: Codec[TestKey32]  = TestKey32.codecTestKey
  implicit val arbitraryMap              = ArbitraryInstances.arbitraryNonEmptyMapTestKey32ByteVector
}
