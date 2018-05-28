package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.test._
import org.lmdbjava.{Env, Txn}
import org.scalatest.BeforeAndAfterAll
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

abstract class HistoryActionsTests[T] extends HistoryTestsBase[T, TestKey, ByteVector] {

  implicit val codecByteVector: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)

  object TestData {
    val key1 = TestKey.create(Seq(1, 0, 0, 0))
    val val1 = ByteVector("value1".getBytes)
    val key2 = TestKey.create(Seq(1, 0, 0, 1))
    val val2 = ByteVector("value2".getBytes)
    val key3 = TestKey.create(Seq(1, 0, 1, 0))
    val val3 = ByteVector("value3".getBytes)
    val key4 = TestKey.create(Seq(1, 0, 1, 1))
    val val4 = ByteVector("value4".getBytes)
    val key5 = TestKey.create(Seq(1, 0, 2, 1))
    val val5 = ByteVector("value5".getBytes)
    val key6 = TestKey.create(Seq(1, 0, 0, 2))
    val val6 = ByteVector("value6".getBytes)
  }

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
      val root1 = store.workingRootHash.get
      insert(store, key1, val1)
      val root2 = store.workingRootHash.get
      root2 shouldBe root1
      lookup(store, key1).value shouldBe val1
    }

  "Two inserts at same key with different values, lookup" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      val root1 = store.workingRootHash.get
      insert(store, key1, val2)
      val root2 = store.workingRootHash.get
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

  "Insert, insert, delete, delete" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      val root0 = store.workingRootHash.get

      insert(store, key1, val1)
      val root1 = store.workingRootHash.get
      root1 should not be root0

      insert(store, key2, val2)
      val root2 = store.workingRootHash.get
      root2 should not be root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2

      delete(store, key2, val2) shouldBe true
      val root3 = store.workingRootHash.get
      root3 shouldBe root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2) shouldBe None

      delete(store, key1, val1) shouldBe true
      val root4 = store.workingRootHash.get
      root4 shouldBe root0

      lookup(store, key1) shouldBe None
      lookup(store, key2) shouldBe None
    }

  "Insert, delete, delete again" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      val root0 = store.workingRootHash.get

      insert(store, key1, val1)
      lookup(store, key1).value shouldBe val1
      store.workingRootHash.get should not be root0

      delete(store, key1, val1) shouldBe true
      lookup(store, key1) shouldBe None
      store.workingRootHash.get shouldBe root0

      delete(store, key1, val1) shouldBe false
      lookup(store, key1) shouldBe None
      store.workingRootHash.get shouldBe root0
    }

  "Insert, Insert, Delete, Delete, Rollback, Delete, Delete" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      val root0 = store.workingRootHash.get

      insert(store, key1, val1)
      val root1 = store.workingRootHash.get
      root1 should not be root0

      insert(store, key2, val2)
      val root2 = store.workingRootHash.get
      root2 should not be root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2

      delete(store, key2, val2) shouldBe true
      store.workingRootHash.get shouldBe root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2) shouldBe None

      delete(store, key1, val1) shouldBe true
      store.workingRootHash.get shouldBe root0

      lookup(store, key1) shouldBe None
      lookup(store, key2) shouldBe None

      // Aaannnd rollback...
      store.reset(root2)

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2

      delete(store, key2, val2) shouldBe true
      store.workingRootHash.get shouldBe root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2) shouldBe None

      delete(store, key1, val1) shouldBe true
      store.workingRootHash.get shouldBe root0

      lookup(store, key1) shouldBe None
      lookup(store, key2) shouldBe None
    }

  "Rollback twice" should "work" in
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      val root0 = store.workingRootHash.get

      insert(store, key1, val1)
      val root1 = store.workingRootHash.get
      root1 should not be root0

      insert(store, key2, val2)
      val root2 = store.workingRootHash.get
      root2 should not be root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2

      delete(store, key2, val2) shouldBe true
      store.workingRootHash.get shouldBe root1

      lookup(store, key1).value shouldBe val1
      lookup(store, key2) shouldBe None

      delete(store, key1, val1) shouldBe true
      store.workingRootHash.get shouldBe root0

      lookup(store, key1) shouldBe None
      lookup(store, key2) shouldBe None

      // Aaannnd rollback...
      store.reset(root2)

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2
      lookup(store, key3) shouldBe None

      insert(store, key3, val3)
      val root3 = store.workingRootHash.get
      root3 should not be root2

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2
      lookup(store, key3).value shouldBe val3

      insert(store, key4, val4)
      val root4 = store.workingRootHash.get
      root4 should not be root3

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2
      lookup(store, key3).value shouldBe val3
      lookup(store, key4).value shouldBe val4

      // rollback again
      store.reset(root3)

      lookup(store, key1).value shouldBe val1
      lookup(store, key2).value shouldBe val2
      lookup(store, key3).value shouldBe val3
      lookup(store, key4) shouldBe None

      insert(store, key4, val4)
      val root5 = store.workingRootHash.get
      root5 shouldBe root4
    }

  "Round trip rollback" should "work" in {
    forAll { (kvs: Map[TestKey, ByteVector]) =>
      withTestTrieStore { store =>
        val pairs = kvs.toList

        val (first, second) = pairs.splitAt(pairs.length / 2)

        val root0 = store.workingRootHash.get

        HistoryActionsTests.insertAll(store, first)
        val root1 = store.workingRootHash.get
        val ret1  = HistoryActionsTests.lookupAll(store, first)

        ret1 shouldBe first

        HistoryActionsTests.insertAll(store, second)
        val root2 = store.workingRootHash.get
        val ret2f = HistoryActionsTests.lookupAll(store, first)
        val ret2s = HistoryActionsTests.lookupAll(store, second)

        root2 should not be root1
        ret2f shouldBe first
        ret2s shouldBe second

        HistoryActionsTests.deleteAll(store, second)
        val root3 = store.workingRootHash.get
        val ret3f = HistoryActionsTests.lookupAll(store, first)
        val ret3s = HistoryActionsTests.lookupAll(store, second)

        root3 shouldBe root1
        ret3f shouldBe first
        ret3s shouldBe empty

        HistoryActionsTests.deleteAll(store, first)
        val root4 = store.workingRootHash.get
        val ret4f = HistoryActionsTests.lookupAll(store, first)
        val ret4s = HistoryActionsTests.lookupAll(store, second)

        root4 shouldBe root0
        ret4f shouldBe empty
        ret4s shouldBe empty

        store.reset(root2)

        HistoryActionsTests.deleteAll(store, second)
        val root5 = store.workingRootHash.get
        val ret5f = HistoryActionsTests.lookupAll(store, first)
        val ret5s = HistoryActionsTests.lookupAll(store, second)

        root5 shouldBe root1
        ret5f shouldBe first
        ret5s shouldBe empty

        HistoryActionsTests.deleteAll(store, first)
        val root6 = store.workingRootHash.get
        val ret6f = HistoryActionsTests.lookupAll(store, first)
        val ret6s = HistoryActionsTests.lookupAll(store, second)

        root6 shouldBe root0
        ret6f shouldBe empty
        ret6s shouldBe empty
      }
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

class LMDBHistoryActionsTests extends HistoryActionsTests[Txn[ByteBuffer]] with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-storage-history-test-")
  val mapSize: Long = 1024L * 1024L * 1024L

  def withTestTrieStore[R](f: ITrieStore[Txn[ByteBuffer], TestKey, ByteVector] => R): R = {
    val env: Env[ByteBuffer] =
      Env
        .create()
        .setMapSize(mapSize)
        .setMaxDbs(1)
        .setMaxReaders(126)
        .open(dbDir.toFile)
    val testStore = LMDBTrieStore.create[TestKey, ByteVector](env)
    try {
      initialize(testStore)
      f(testStore)
    } finally {
      testStore.close()
      env.close()
    }

  }
  override def afterAll(): Unit =
    recursivelyDeletePath(dbDir)
}
