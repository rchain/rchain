package coop.rchain.storage

import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.storage.test._
import coop.rchain.storage.test.implicits._
import coop.rchain.storage.util.{ignore => ign}
import org.scalatest._

import scala.collection.mutable

class StorageActionsTests(createStore: () => IStore[String, Pattern, String, List[String] => Unit])
    extends FlatSpec
    with Matchers
    with OptionValues {

  val logger: Logger = Logger[StorageActionsTests]

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestStore(f: IStore[String, Pattern, String, List[String] => Unit] => Unit): Unit = {
    val store: IStore[String, Pattern, String, List[String] => Unit] = createStore()
    try {
      f(store)
    } finally {
      store.close()
    }
  }

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
    val keyHash = store.hashC(key)

    val r = produce(store, key.head, "hello")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List("hello")
      store.getK(txn, key) shouldBe None
    }

    r shouldBe None
  }

  "producing twice on the same channel" should
    "persist two pieces of data in the store" in withTestStore { store =>
    val key     = List("ch1")
    val keyHash = store.hashC(key)

    val r1 = produce(store, key.head, "hello")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List("hello")
      store.getK(txn, key) shouldBe None
    }

    r1 shouldBe None

    val r2 = produce(store, key.head, "goodbye")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) should contain theSameElementsAs List("goodbye", "hello")
      store.getK(txn, key) shouldBe None
    }

    r2 shouldBe None
  }

  "consuming on one channel" should
    "persist a continuation in the store" in withTestStore { store =>
    val key      = List("ch1")
    val patterns = List(Wildcard)
    val keyHash  = store.hashC(key)
    val results  = mutable.ListBuffer.empty[List[String]]

    val r = consume(store, key, patterns, capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe List("ch1")
      store.getPs(txn, key) shouldBe patterns
      store.getAs(txn, key) shouldBe Nil
      store.getK(txn, key) shouldBe defined
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
    val keyHash  = store.hashC(key)
    val results  = mutable.ListBuffer.empty[List[String]]

    val r = consume(store, key, patterns, capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe patterns
      store.getAs(txn, key) shouldBe Nil
      store.getK(txn, key) shouldBe defined
    }

    r shouldBe None
  }

  "producing and then consuming on the same channel" should
    "return the continuation and data" in withTestStore { store =>
    val key     = List("ch1")
    val keyHash = store.hashC(key)
    val results = mutable.ListBuffer.empty[List[String]]

    val r1 = produce(store, key.head, "world")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List("world")
      store.getK(txn, key) shouldBe None
    }

    r1 shouldBe None

    val r2 = consume(store, key, List(Wildcard), capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe Nil
      store.getK(txn, key) shouldBe None
    }

    r2 shouldBe defined

    runK(r2)

    results should contain theSameElementsAs List(List("world"))
  }

  "producing on three different channels and then consuming once on all three" should
    "return the continuation and all the data" in withTestStore { store =>
    val produceKey1     = List("ch1")
    val produceKey2     = List("ch2")
    val produceKey3     = List("ch3")
    val consumeKey      = List("ch1", "ch2", "ch3")
    val patterns        = List(Wildcard, Wildcard, Wildcard)
    val produceKey1Hash = store.hashC(produceKey1)
    val produceKey2Hash = store.hashC(produceKey2)
    val produceKey3Hash = store.hashC(produceKey3)
    val consumeKeyHash  = store.hashC(consumeKey)
    val results         = mutable.ListBuffer.empty[List[String]]

    val r1 = produce(store, produceKey1.head, "world")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey1Hash) shouldBe produceKey1
      store.getPs(txn, produceKey1) shouldBe Nil
      store.getAs(txn, produceKey1) shouldBe List("world")
      store.getK(txn, produceKey1) shouldBe None
    }

    r1 shouldBe None

    val r2 = produce(store, produceKey2.head, "hello")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey2Hash) shouldBe produceKey2
      store.getPs(txn, produceKey2) shouldBe Nil
      store.getAs(txn, produceKey2) shouldBe List("hello")
      store.getK(txn, produceKey2) shouldBe None
    }

    r2 shouldBe None

    val r3 = produce(store, produceKey3.head, "goodbye")

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey3Hash) shouldBe produceKey3
      store.getPs(txn, produceKey3) shouldBe Nil
      store.getAs(txn, produceKey3) shouldBe List("goodbye")
      store.getK(txn, produceKey3) shouldBe None
    }

    r3 shouldBe None

    val r4 = consume(store, List("ch1", "ch2", "ch3"), patterns, capture(results))

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, consumeKeyHash) shouldBe Nil
      store.getPs(txn, consumeKey) shouldBe Nil
      store.getAs(txn, consumeKey) shouldBe Nil
      store.getK(txn, consumeKey) shouldBe None
    }

    r4 shouldBe defined

    runK(r4)

    results should contain theSameElementsAs List(List("world", "hello", "goodbye"))
  }

  "producing three times on the same channel then consuming three times on the same channel" should
    "return " in withTestStore { store =>
    val results: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

    val key = List("ch1")

    val wk1 = produce(store, key.head, "world")
    val wk2 = produce(store, key.head, "hello")
    val wk3 = produce(store, key.head, "goodbye")
    val wk4 = consume(store, key, List(Wildcard), capture(results))
    val wk5 = consume(store, key, List(Wildcard), capture(results))
    val wk6 = consume(store, key, List(Wildcard), capture(results))

    val test = List(wk1, wk2, wk3, wk4, wk5, wk6)

    test.foreach(runK)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, store.hashC(key)) shouldBe key
      store.getAs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe Nil
      store.getPs(txn, key) shouldBe Nil
      store.getPs(txn, key) shouldBe Nil
      store.getPs(txn, key) shouldBe Nil
      store.getK(txn, key) shouldBe None
      store.getK(txn, key) shouldBe None
      store.getK(txn, key) shouldBe None
    }

    results should contain theSameElementsAs List(List("goodbye"), List("hello"), List("world"))
  }

  "consuming three times on the same channel, then producing three times on that channel" should
    "return the last continuation consumed along with a piece of data" in withTestStore { store =>
    val results: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

    consume(store, List("ch1"), List(Wildcard), capture(results))
    consume(store, List("ch1"), List(Wildcard), capture(results))
    consume(store, List("ch1"), List(Wildcard), capture(results))
    val r1 = produce(store, "ch1", "world")
    val r2 = produce(store, "ch1", "hello")
    val r3 = produce(store, "ch1", "goodbye")

    List(r1, r2, r3).foreach(runK)

    results should contain theSameElementsAs List(List("world"))
  }

  "consuming on two channels, producing on one, then producing on the other" should
    "return two separate continuations, each paired with separate pieces of data" in withTestStore {
    store =>
      val key     = List("ch1", "ch2")
      val pattern = List(Wildcard, Wildcard)
      val results = mutable.ListBuffer.empty[List[String]]

      val r1 = consume(store, key, pattern, capture(results))

      r1 shouldBe None

      val r2 = produce(store, "ch1", "This is some data")

      r2 shouldBe defined

      val r3 = produce(store, "ch2", "This is some other data")

      r3 shouldBe defined

      List(r2, r3).foreach(runK)

      results should contain theSameElementsAs List(List("This is some data"),
                                                    List("This is some other data"))
  }

  "consuming and producing with non-trivial matches" should
    "work" in withTestStore { store =>
    val results: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

    val wk1 = consume(store,
                      List("hello", "world"),
                      List(Wildcard, StringMatch("This is some data")),
                      capture(results))
    val wk2  = produce(store, "hello", "This is some data")
    val test = List(wk1, wk2)

    test.foreach(runK)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("hello", "world")) shouldBe Nil
      store.getAs(txn, List("hello")) shouldBe Nil
    }

    results should contain theSameElementsAs List(List("This is some data"))
  }

  "consuming twice and producing twice with non-trivial matches" should
    "work" in withTestStore { store =>
    val results1: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]
    val results2: mutable.ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

    val wk1 =
      consume(store, List("hello"), List(StringMatch("This is some data")), capture(results1))
    val wk2 =
      consume(store, List("world"), List(StringMatch("This is some other data")), capture(results2))
    val wk3  = produce(store, "hello", "This is some data")
    val wk4  = produce(store, "world", "This is some other data")
    val test = List(wk1, wk2, wk3, wk4)

    test.foreach(runK)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("hello")) shouldBe Nil
      store.getAs(txn, List("world")) shouldBe Nil
    }

    results1 should contain theSameElementsAs List(List("This is some data"))
    results2 should contain theSameElementsAs List(List("This is some other data"))
  }

  "consuming on two channels, consuming on one of those channels, and then producing on both of those channels" should
    "return a continuations paired with one piece of data along with another continuation paired with the other piece of data" in
    withTestStore { store =>
      val consumeKey1 = List("hello", "goodbye")
      val consumeKey2 = List("hello")

      val results1 = mutable.ListBuffer.empty[List[String]]
      val results2 = mutable.ListBuffer.empty[List[String]]

      consume(store, consumeKey1, List(Wildcard, Wildcard), capture(results1))
      consume(store, consumeKey2, List(Wildcard), capture(results2))

      val r3 = produce(store, "goodbye", "This is some data")

      val r4 = produce(store, "hello", "This is some other data")

      List(r3, r4).foreach(runK)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getAs(txn, List("hello")) shouldBe Nil
        store.getAs(txn, List("goodbye")) shouldBe Nil
      }

      results1 should contain theSameElementsAs List(List("This is some data"))
      results2 should contain theSameElementsAs List(List("This is some other data"))
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

class InMemoryStoreStorageActionsTests
    extends StorageActionsTests(
      () => InMemoryStore.create[String, Pattern, String, List[String] => Unit])
