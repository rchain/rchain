package coop.rchain.storage

import java.nio.file.{Files, Path}

import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.storage.test._
import coop.rchain.storage.test.implicits._
import coop.rchain.storage.util.{ignore => ign}
import org.scalatest._

import scala.collection.mutable
import scala.util.Try

abstract class StorageActionsTests extends FlatSpec with Matchers with OptionValues {

  val logger: Logger = Logger[StorageActionsTests]

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestStore(f: IStore[String, Pattern, String, List[String] => Unit] => Unit): Unit

  /** A utility function to be used as a continuation in tests.
    *
    * It captures the data it consumes in a given [[scala.collection.mutable.ListBuffer]]
    */
  def capture(res: mutable.ListBuffer[List[String]]): List[String] => Unit =
    (as: List[String]) => ign(res += as)

  /** Runs a given test continuation with given data as its arguments.
    */
  def runK(t: Option[(List[String] => Unit, List[String])]): Unit =
    t.foreach { case (k, data) => k(data) }

  /* Tests */

  "produce" should
    "persist a piece of data in the store" in withTestStore { store =>
    val key     = List("ch1")
    val keyHash = store.hashCs(key)

    val r = produce(store, key.head, "datum")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List("datum")
      store.getPsK(txn, key) shouldBe Nil
    }

    r shouldBe None
  }

  "producing twice on the same channel" should
    "persist two pieces of data in the store" in withTestStore { store =>
    val key     = List("ch1")
    val keyHash = store.hashCs(key)

    val r1 = produce(store, key.head, "datum1")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List("datum1")
      store.getPsK(txn, key) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = produce(store, key.head, "datum2")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) should contain theSameElementsAs List("datum1", "datum2")
      store.getPsK(txn, key) shouldBe Nil
    }

    r2 shouldBe None
  }

  "consuming on one channel" should
    "persist a continuation in the store" in withTestStore { store =>
    val key      = List("ch1")
    val patterns = List(Wildcard)
    val keyHash  = store.hashCs(key)
    val results  = mutable.ListBuffer.empty[List[String]]

    val r = consume(store, key, patterns, capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe List("ch1")
      store.getPs(txn, key) shouldBe List(patterns)
      store.getAs(txn, key) shouldBe Nil
      store.getPsK(txn, key) should not be empty
    }

    r shouldBe None
  }

  "consuming with a list of patterns that is a different length than the list of channels" should
    "throw" in withTestStore { store =>
    val results = mutable.ListBuffer.empty[List[String]]

    an[IllegalArgumentException] shouldBe thrownBy(
      consume(store, List("ch1", "ch2"), List(Wildcard), capture(results)))
  }

  "consuming on three channels" should
    "persist a continuation in the store" in withTestStore { store =>
    val key      = List("ch1", "ch2", "ch3")
    val patterns = List(Wildcard, Wildcard, Wildcard)
    val keyHash  = store.hashCs(key)
    val results  = mutable.ListBuffer.empty[List[String]]

    val r = consume(store, key, patterns, capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe List(patterns)
      store.getAs(txn, key) shouldBe Nil
      store.getPsK(txn, key) should not be empty
    }

    r shouldBe None
  }

  "producing and then consuming on the same channel" should
    "return the continuation and data" in withTestStore { store =>
    val key     = List("ch1")
    val keyHash = store.hashCs(key)
    val results = mutable.ListBuffer.empty[List[String]]

    val r1 = produce(store, key.head, "datum")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List("datum")
      store.getPsK(txn, key) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = consume(store, key, List(Wildcard), capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe Nil
      store.getPsK(txn, key) shouldBe Nil
    }

    r2 shouldBe defined

    runK(r2)

    results should contain theSameElementsAs List(List("datum"))
  }

  "producing on channel, consuming on that channel and another, and then producing on the other channel" should
    "return a continuation and all the data" in withTestStore { store =>
    val produceKey1     = List("ch1")
    val produceKey1Hash = store.hashCs(produceKey1)

    val r1 = produce(store, produceKey1.head, "datum1")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey1Hash) shouldBe produceKey1
      store.getPs(txn, produceKey1) shouldBe Nil
      store.getAs(txn, produceKey1) shouldBe List("datum1")
      store.getPsK(txn, produceKey1) shouldBe Nil
    }

    r1 shouldBe None

    val consumeKey     = List("ch1", "ch2")
    val consumeKeyHash = store.hashCs(consumeKey)
    val consumePattern = List(Wildcard, Wildcard)
    val results        = mutable.ListBuffer.empty[List[String]]

    val r2 = consume(store, consumeKey, consumePattern, capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey1Hash) shouldBe produceKey1
      store.getPs(txn, produceKey1) shouldBe Nil
      store.getAs(txn, produceKey1) shouldBe List("datum1")
      store.getPsK(txn, produceKey1) shouldBe Nil
      store.getKey(txn, consumeKeyHash) shouldBe consumeKey
      store.getPs(txn, consumeKey) shouldBe List(consumePattern)
      store.getAs(txn, consumeKey) shouldBe Nil
      store.getPsK(txn, consumeKey) should not be empty
    }

    r2 shouldBe None

    val produceKey2     = List("ch2")
    val produceKey2Hash = store.hashCs(produceKey2)

    val r3 = produce(store, produceKey2.head, "datum2")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey1Hash) shouldBe produceKey1
      store.getPs(txn, produceKey1) shouldBe Nil
      store.getAs(txn, produceKey1) shouldBe Nil
      store.getPsK(txn, produceKey1) shouldBe Nil
      store.getKey(txn, consumeKeyHash) shouldBe consumeKey
      store.getPs(txn, consumeKey) shouldBe Nil
      store.getAs(txn, consumeKey) shouldBe Nil
      store.getPsK(txn, consumeKey) shouldBe Nil
      store.getKey(txn, produceKey2Hash) shouldBe produceKey2
      store.getPs(txn, produceKey2) shouldBe Nil
      store.getAs(txn, produceKey2) shouldBe Nil
      store.getPsK(txn, produceKey2) shouldBe Nil
    }

    r3 shouldBe defined

    runK(r3)

    results should contain theSameElementsAs List(List("datum1", "datum2"))
  }

  "producing on three different channels and then consuming once on all three" should
    "return the continuation and all the data" in withTestStore { store =>
    val produceKey1     = List("ch1")
    val produceKey2     = List("ch2")
    val produceKey3     = List("ch3")
    val consumeKey      = List("ch1", "ch2", "ch3")
    val patterns        = List(Wildcard, Wildcard, Wildcard)
    val produceKey1Hash = store.hashCs(produceKey1)
    val produceKey2Hash = store.hashCs(produceKey2)
    val produceKey3Hash = store.hashCs(produceKey3)
    val consumeKeyHash  = store.hashCs(consumeKey)
    val results         = mutable.ListBuffer.empty[List[String]]

    val r1 = produce(store, produceKey1.head, "datum1")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey1Hash) shouldBe produceKey1
      store.getPs(txn, produceKey1) shouldBe Nil
      store.getAs(txn, produceKey1) shouldBe List("datum1")
      store.getPsK(txn, produceKey1) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = produce(store, produceKey2.head, "datum2")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey2Hash) shouldBe produceKey2
      store.getPs(txn, produceKey2) shouldBe Nil
      store.getAs(txn, produceKey2) shouldBe List("datum2")
      store.getPsK(txn, produceKey2) shouldBe Nil
    }

    r2 shouldBe None

    val r3 = produce(store, produceKey3.head, "datum3")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey3Hash) shouldBe produceKey3
      store.getPs(txn, produceKey3) shouldBe Nil
      store.getAs(txn, produceKey3) shouldBe List("datum3")
      store.getPsK(txn, produceKey3) shouldBe Nil
    }

    r3 shouldBe None

    val r4 = consume(store, List("ch1", "ch2", "ch3"), patterns, capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, consumeKeyHash) shouldBe Nil
      store.getPs(txn, consumeKey) shouldBe Nil
      store.getAs(txn, consumeKey) shouldBe Nil
      store.getPsK(txn, consumeKey) shouldBe Nil
    }

    r4 shouldBe defined

    runK(r4)

    results should contain theSameElementsAs List(List("datum1", "datum2", "datum3"))
  }

  "producing three times on the same channel then consuming three times on the same channel" should
    "return three pairs of continuations and data" in withTestStore { store =>
    val results: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

    val key = List("ch1")

    val r1 = produce(store, key.head, "datum1")
    val r2 = produce(store, key.head, "datum2")
    val r3 = produce(store, key.head, "datum3")

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None

    val r4 = consume(store, key, List(Wildcard), capture(results))
    val r5 = consume(store, key, List(Wildcard), capture(results))
    val r6 = consume(store, key, List(Wildcard), capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, store.hashCs(key)) shouldBe key
      store.getAs(txn, key) shouldBe Nil
      store.getPs(txn, key) shouldBe Nil
      store.getPsK(txn, key) shouldBe Nil
    }

    val continuations = List(r4, r5, r6)

    continuations.forall(_.isDefined) shouldBe true

    continuations.foreach(runK)

    results should contain theSameElementsAs List(List("datum3"), List("datum2"), List("datum1"))
  }

  "consuming three times on the same channel, then producing three times on that channel" should
    "return three continuations, each paired with distinct pieces of data" in withTestStore {
    store =>
      val results1: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]
      val results2: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]
      val results3: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

      consume(store, List("ch1"), List(Wildcard), capture(results1))
      consume(store, List("ch1"), List(Wildcard), capture(results2))
      consume(store, List("ch1"), List(Wildcard), capture(results3))
      val r1 = produce(store, "ch1", "datum1")
      val r2 = produce(store, "ch1", "datum2")
      val r3 = produce(store, "ch1", "datum3")

      r1 shouldBe defined
      r2 shouldBe defined
      r3 shouldBe defined

      List(r1, r2, r3).foreach(runK)

      results1 should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
      results2 should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
      results3 should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

      results1 shouldNot contain theSameElementsAs results2
      results1 shouldNot contain theSameElementsAs results3
      results2 shouldNot contain theSameElementsAs results3
  }

  "consuming three times on the same channel with non-trivial matches, then producing three times on that channel" should
    "return three continuations, each paired with matching data" in withTestStore { store =>
    val results1: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]
    val results2: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]
    val results3: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

    consume(store, List("ch1"), List(StringMatch("datum1")), capture(results1))
    consume(store, List("ch1"), List(StringMatch("datum2")), capture(results2))
    consume(store, List("ch1"), List(StringMatch("datum3")), capture(results3))
    val r1 = produce(store, "ch1", "datum1")
    val r2 = produce(store, "ch1", "datum2")
    val r3 = produce(store, "ch1", "datum3")

    r1 shouldBe defined
    r2 shouldBe defined
    r3 shouldBe defined

    List(r1, r2, r3).foreach(runK)

    results1 shouldBe List(List("datum1"))
    results2 shouldBe List(List("datum2"))
    results3 shouldBe List(List("datum3"))
  }

  "consuming on two channels, producing on one, then producing on the other" should
    "return a continuation with both pieces of data" in withTestStore { store =>
    val key     = List("ch1", "ch2")
    val pattern = List(Wildcard, Wildcard)
    val results = mutable.ListBuffer.empty[List[String]]

    val r1 = consume(store, key, pattern, capture(results))
    val r2 = produce(store, "ch1", "datum1")
    val r3 = produce(store, "ch2", "datum2")

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe defined

    runK(r3)

    results should contain theSameElementsAs List(List("datum1", "datum2"))
  }

  "consuming twice on the same channels with different patterns, and then producing on those channels" should
    "return continuations with the expected data" in withTestStore { store =>
    val channels = List("ch1", "ch2")
    val pattern1 = List(StringMatch("datum1"), StringMatch("datum2"))
    val pattern2 = List(StringMatch("datum3"), StringMatch("datum4"))
    val results1 = mutable.ListBuffer.empty[List[String]]
    val results2 = mutable.ListBuffer.empty[List[String]]

    val r1 = consume(store, channels, pattern1, capture(results1))
    val r2 = consume(store, channels, pattern2, capture(results2))
    val r3 = produce(store, "ch1", "datum3")
    val r4 = produce(store, "ch2", "datum4")
    val r5 = produce(store, "ch1", "datum1")
    val r6 = produce(store, "ch2", "datum2")

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None
    r4 shouldBe defined
    r5 shouldBe None
    r6 shouldBe defined

    List(r4, r6).foreach(runK)

    results1 should contain theSameElementsAs List(List("datum1", "datum2"))
    results2 should contain theSameElementsAs List(List("datum3", "datum4"))
  }

  "consuming and producing with non-trivial matches" should
    "work" in withTestStore { store =>
    val results: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

    val r1 = consume(
      store,
      List("ch1", "ch2"),
      List(Wildcard, StringMatch("datum1")),
      capture(results)
    )
    val r2 = produce(store, "ch1", "datum1")

    List(r1, r2).foreach(runK)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1", "ch2")) shouldBe Nil
      store.getAs(txn, List("ch1")) shouldBe List("datum1")
    }

    results shouldBe empty
  }

  "consuming twice and producing twice with non-trivial matches" should
    "work" in withTestStore { store =>
    val results1: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]
    val results2: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

    val r1 = consume(store, List("ch1"), List(StringMatch("datum1")), capture(results1))
    val r2 = consume(store, List("ch2"), List(StringMatch("datum2")), capture(results2))
    val r3 = produce(store, "ch1", "datum1")
    val r4 = produce(store, "ch2", "datum2")

    List(r1, r2, r3, r4).foreach(runK)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1")) shouldBe Nil
      store.getAs(txn, List("ch2")) shouldBe Nil
    }

    results1 should contain theSameElementsAs List(List("datum1"))
    results2 should contain theSameElementsAs List(List("datum2"))
  }

  "consuming on two channels, consuming on one of those channels, and then producing on both of those channels separately" should
    "return a continuation paired with one piece of data" in
    withTestStore { store =>
      val results1 = mutable.ListBuffer.empty[List[String]]
      val results2 = mutable.ListBuffer.empty[List[String]]

      consume(store, List("ch1", "ch2"), List(Wildcard, Wildcard), capture(results1))
      consume(store, List("ch1"), List(Wildcard), capture(results2))

      val r3 = produce(store, "ch1", "datum1")
      val r4 = produce(store, "ch2", "datum2")

      store.withTxn(store.createTxnRead()) { txn =>
        store.getPsK(txn, List("ch1", "ch2")) should not be empty
        store.getPsK(txn, List("ch1")) shouldBe Nil
        store.getPsK(txn, List("ch2")) shouldBe Nil
        store.getAs(txn, List("ch1")) shouldBe Nil
        store.getAs(txn, List("ch2")) shouldBe List("datum2")
      }

      r3 shouldBe defined
      r4 shouldBe None

      runK(r3)

      results1 shouldBe empty
      results2 should contain theSameElementsAs List(List("datum1"))
    }

  "the hello world example" should "work" in withTestStore { store =>
    val results = mutable.ListBuffer.empty[List[String]]

    def testConsumer(k: List[String] => Unit)(channels: List[String]): Unit =
      runK(consume(store, channels, List(Wildcard), k))

    def test(k: List[String] => Unit): Unit = {
      runK(consume(store, List("helloworld"), List(Wildcard), k))
      runK(produce(store, "helloworld", "world"))
      runK(produce(store, "world", "Hello World"))
    }

    test(testConsumer(capture(results)))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("helloworld")) shouldBe Nil
    }

    results should contain theSameElementsAs List(List("Hello World"))
  }
}

class InMemoryStoreStorageActionsTests extends StorageActionsTests {
  override def withTestStore(
      f: IStore[String, Pattern, String, List[String] => Unit] => Unit): Unit = {
    val testStore = InMemoryStore.create[String, Pattern, String, List[String] => Unit]
    try {
      f(testStore)
    } finally {
      testStore.close()
    }
  }
}

class LMDBStoreStorageActionsTests extends StorageActionsTests with BeforeAndAfterAll {
  private[this] val dbDir = Files.createTempDirectory("rchain-storage-test-")

  override def withTestStore(
      f: IStore[String, Pattern, String, List[String] => Unit] => Unit): Unit = {
    val testStore =
      LMDBStore.create[String, Pattern, String, List[String] => Unit](dbDir, 1024 * 1024 * 1024)
    testStore.clear()
    try {
      f(testStore)
    } finally {
      testStore.close()
    }
  }

  override def afterAll(): Unit =
    recursivelyDeletePath(dbDir)
}
