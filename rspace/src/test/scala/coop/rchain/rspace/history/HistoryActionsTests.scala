package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.test._
import org.lmdbjava.{Env, Txn}
import org.scalatest.{BeforeAndAfterAll, Suite}
import scodec.Codec
import scodec.codecs._
import scodec.bits.ByteVector

abstract class HistoryActionsTests[T] extends HistoryTestsBase[T, TestKey, ByteVector] {

  implicit val codecByteVector: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)

  import TestData._

  "Insert, lookup" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      val actual = lookup(store, key1)
      actual.value shouldBe val1
    }

  "Two inserts, two lookups" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      insert(store, key2, val2)
      val actual1 = lookup(store, key1)
      val actual2 = lookup(store, key2)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
    }

  "Duplicate inserts, lookup" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      val root1 = getRoot(store)
      insert(store, key1, val1)
      val root2 = getRoot(store)
      root2 shouldBe root1
      lookup(store, key1).value shouldBe val1
    }

  "Two inserts at same key with different values, lookup" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      val root1 = getRoot(store)
      insert(store, key1, val2)
      val root2 = getRoot(store)
      root2 should not be root1
      lookup(store, key1).value shouldBe val2
    }

  "Three inserts, three lookups" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      insert(store, key2, val2)
      insert(store, key3, val3)
      val actual1 = lookup(store, key1)
      val actual2 = lookup(store, key2)
      val actual3 = lookup(store, key3)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
      actual3.value shouldBe val3
    }

  "Three inserts, three lookups (alt)" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      insert(store, key2, val2)
      insert(store, key6, val6)
      val actual1 = lookup(store, key1)
      val actual2 = lookup(store, key2)
      val actual3 = lookup(store, key6)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
      actual3.value shouldBe val6
    }

  "Four inserts, four lookups" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      insert(store, key2, val2)
      insert(store, key3, val3)
      insert(store, key4, val4)
      val actual1 = lookup(store, key1)
      val actual2 = lookup(store, key2)
      val actual3 = lookup(store, key3)
      val actual4 = lookup(store, key4)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
      actual3.value shouldBe val3
      actual4.value shouldBe val4
    }

  "Five inserts, five lookups" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      insert(store, key2, val2)
      insert(store, key3, val3)
      insert(store, key4, val4)
      insert(store, key5, val5)
      val actual1 = lookup(store, key1)
      val actual2 = lookup(store, key2)
      val actual3 = lookup(store, key3)
      val actual4 = lookup(store, key4)
      val actual5 = lookup(store, key5)
      actual1.value shouldBe val1
      actual2.value shouldBe val2
      actual3.value shouldBe val3
      actual4.value shouldBe val4
      actual5.value shouldBe val5
    }

  "Insert, lookup, delete, lookup" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      lookup(store, key1).value shouldBe val1

      delete(store, key1, val1) shouldBe true
      lookup(store, key1) shouldBe None
    }

  "Delete, lookup" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      delete(store, key1, val1) shouldBe false
      lookup(store, key1) shouldBe None
    }

  "Delete" should "return false for a missing (key, value)" in {
    withTestTrieStore { implicit store =>
      insert(store, key2, val2)
      delete(store, key2, val2) shouldBe true
      delete(store, key2, val2) shouldBe false
    }
  }

  "Insert, insert, delete, delete" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      val root0 = getRoot(store)

      insert(store, key1, val1)
      val root1 = getRoot(store)
      root1 should not be root0

      insert(store, key2, val2)
      val root2 = getRoot(store)
      root2 should not be root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2

      delete(store, key2, val2) shouldBe true
      val root3 = getRoot(store)
      root3 shouldBe root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2) shouldBe None

      delete(store, key1, val1) shouldBe true
      val root4 = getRoot(store)
      root4 shouldBe root0

      lookup(store, key1) shouldBe None
      lookup(store, key2) shouldBe None
    }

  "Insert, delete, delete again" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      val root0 = getRoot(store)

      insert(store, key1, val1)
      lookup(store, key1).value shouldBe val1
      getRoot(store) should not be root0

      delete(store, key1, val1) shouldBe true
      lookup(store, key1) shouldBe None
      getRoot(store) shouldBe root0

      delete(store, key1, val1) shouldBe false
      lookup(store, key1) shouldBe None
      getRoot(store) shouldBe root0
    }

  "Insert, Insert, Delete, Delete, Rollback, Delete, Delete" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      val root0 = getRoot(store)

      insert(store, key1, val1)
      val root1 = getRoot(store)
      root1 should not be root0

      insert(store, key2, val2)
      val root2 = getRoot(store)
      root2 should not be root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2

      delete(store, key2, val2) shouldBe true
      getRoot(store) shouldBe root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2) shouldBe None

      delete(store, key1, val1) shouldBe true
      getRoot(store) shouldBe root0

      lookup(store, key1) shouldBe None
      lookup(store, key2) shouldBe None

      // Aaannnd rollback...
      root2.foreach(setRoot(store, _))

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2

      delete(store, key2, val2) shouldBe true
      getRoot(store) shouldBe root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2) shouldBe None

      delete(store, key1, val1) shouldBe true
      getRoot(store) shouldBe root0

      lookup(store, key1) shouldBe None
      lookup(store, key2) shouldBe None
    }

  "Rollback twice" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      val root0 = getRoot(store)

      insert(store, key1, val1)
      val root1 = getRoot(store)
      root1 should not be root0

      insert(store, key2, val2)
      val root2 = getRoot(store)
      root2 should not be root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2

      delete(store, key2, val2) shouldBe true
      getRoot(store) shouldBe root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2) shouldBe None

      delete(store, key1, val1) shouldBe true
      getRoot(store) shouldBe root0

      lookup(store, key1) shouldBe None
      lookup(store, key2) shouldBe None

      // Aaannnd rollback...
      root2.foreach(setRoot(store, _))

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2
      lookup(store, key3) shouldBe None

      insert(store, key3, val3)
      val root3 = getRoot(store)
      root3 should not be root2

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2
      lookup(store, key3).value shouldBe val3

      insert(store, key4, val4)
      val root4 = getRoot(store)
      root4 should not be root3

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2
      lookup(store, key3).value shouldBe val3
      lookup(store, key4).value shouldBe val4

      // rollback again
      root3.foreach(setRoot(store, _))

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2
      lookup(store, key3).value shouldBe val3
      lookup(store, key4) shouldBe None

      insert(store, key4, val4)
      val root5 = getRoot(store)
      root5 shouldBe root4
    }

  "Round trip rollback" should "work" in {
    forAll { (kvs: Map[TestKey, ByteVector]) =>
      withTestTrieStore { store =>
        val pairs = kvs.toList

        val (first, second) = pairs.splitAt(pairs.length / 2)

        val root0 = getRoot(store)

        HistoryActionsTests.insertAll(store, first)
        val root1 = getRoot(store)
        val ret1  = HistoryActionsTests.lookupAll(store, first)

        ret1 shouldBe first

        HistoryActionsTests.insertAll(store, second)
        val root2 = getRoot(store)
        val ret2f = HistoryActionsTests.lookupAll(store, first)
        val ret2s = HistoryActionsTests.lookupAll(store, second)

        root2 should not be root1
        ret2f shouldBe first
        ret2s shouldBe second

        HistoryActionsTests.deleteAll(store, second)
        val root3 = getRoot(store)
        val ret3f = HistoryActionsTests.lookupAll(store, first)
        val ret3s = HistoryActionsTests.lookupAll(store, second)

        root3 shouldBe root1
        ret3f shouldBe first
        ret3s shouldBe empty

        HistoryActionsTests.deleteAll(store, first)
        val root4 = getRoot(store)
        val ret4f = HistoryActionsTests.lookupAll(store, first)
        val ret4s = HistoryActionsTests.lookupAll(store, second)

        root4 shouldBe root0
        ret4f shouldBe empty
        ret4s shouldBe empty

        root2.foreach(setRoot(store, _))

        HistoryActionsTests.deleteAll(store, second)
        val root5 = getRoot(store)
        val ret5f = HistoryActionsTests.lookupAll(store, first)
        val ret5s = HistoryActionsTests.lookupAll(store, second)

        root5 shouldBe root1
        ret5f shouldBe first
        ret5s shouldBe empty

        HistoryActionsTests.deleteAll(store, first)
        val root6 = getRoot(store)
        val ret6f = HistoryActionsTests.lookupAll(store, first)
        val ret6s = HistoryActionsTests.lookupAll(store, second)

        root6 shouldBe root0
        ret6f shouldBe empty
        ret6s shouldBe empty
      }
    }
  }

  "getLeaves on an empty store" should "return an empty sequence" in
    withTestTrieStore { store =>
      val leaves = getRoot(store).map(getLeaves(store, _))
      leaves.value shouldBe empty
    }

  "insert 6 things and getLeaves" should "return all of the leaves" in
    withTestTrieStore { store =>
      val expected: Vector[Leaf[TestKey, ByteVector]] = Vector(
        Leaf(key1, val1),
        Leaf(key2, val2),
        Leaf(key3, val3),
        Leaf(key4, val4),
        Leaf(key5, val5),
        Leaf(key6, val6)
      )

      insert(store, key1, val1)
      insert(store, key2, val2)
      insert(store, key3, val3)
      insert(store, key4, val4)
      insert(store, key5, val5)
      insert(store, key6, val6)

      val leaves = getRoot(store).map(getLeaves(store, _))

      leaves.value should contain theSameElementsAs expected
    }

  "insert a bunch of things and getLeaves" should "return all of the leaves" in
    forAll { (kvs: Map[TestKey, ByteVector]) =>
      withTestTrieStore { store =>
        val expected = kvs.map { case (k, v) => Leaf(k, v) }
        kvs.foreach { case (k, v) => insert(store, k, v) }
        val leaves = getRoot(store).map(getLeaves(store, _))
        leaves.value should contain theSameElementsAs expected
      }
    }
}

object HistoryActionsTests {

  def insertAll[T, K, V](store: ITrieStore[T, K, V], pairs: Seq[(K, V)])(implicit
                                                                         codecK: Codec[K],
                                                                         codecV: Codec[V]): Unit =
    pairs.foreach { case (k, v) => insert(store, k, v) }

  def deleteAll[T, K, V](store: ITrieStore[T, K, V], pairs: Seq[(K, V)])(implicit
                                                                         codecK: Codec[K],
                                                                         codecV: Codec[V]): Unit =
    pairs.foreach { case (k, v) => delete(store, k, v) }

  def lookupAll[T, K, V](store: ITrieStore[T, K, V], pairs: Seq[(K, V)])(
      implicit codecK: Codec[K]): Seq[(K, V)] =
    pairs.flatMap { case (k, _) => lookup(store, k).map((v: V) => (k, v)).toList }
}

trait LMDBTrieStoreFixtures extends BeforeAndAfterAll { this: Suite =>
  val dbDir: Path   = Files.createTempDirectory("rchain-storage-history-test-")
  val mapSize: Long = 1024L * 1024L * 1024L

  def withTestTrieStore[R](f: ITrieStore[Txn[ByteBuffer], TestKey, ByteVector] => R): R = {
    // @todo deliver better
    implicit val codecByteVector: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)
    val env: Env[ByteBuffer] =
      Env
        .create()
        .setMapSize(mapSize)
        .setMaxDbs(2)
        .setMaxReaders(126)
        .open(dbDir.toFile)
    val testStore = LMDBTrieStore.create[TestKey, ByteVector](env)
    testStore.withTxn(testStore.createTxnWrite())(txn => testStore.clear(txn))
    try {
      f(testStore)
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
    with LMDBTrieStoreFixtures {}
