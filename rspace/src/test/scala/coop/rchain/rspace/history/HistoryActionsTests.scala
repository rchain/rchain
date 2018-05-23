package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

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

  // TODO(ht): enable this test
  "Two inserts at same key with different values, lookup" should "work" ignore
    withTestTrieStore { (store: ITrieStore[T, TestKey, ByteVector]) =>
      insert(store, key1, val1)
      val root1 = store.workingRootHash.get
      insert(store, key1, val2)
      val root2 = store.workingRootHash.get
      root2 shouldBe root1
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
}

class LMDBHistoryActionsTests extends HistoryActionsTests[Txn[ByteBuffer]] with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-storage-history-test-")
  val mapSize: Long = 1024L * 1024L * 1024L

  def withTestTrieStore(f: ITrieStore[Txn[ByteBuffer], TestKey, ByteVector] => Unit): Unit = {
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
