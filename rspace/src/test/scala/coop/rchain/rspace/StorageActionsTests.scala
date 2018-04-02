package coop.rchain.rspace

import java.nio.file.Files

import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.extended._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test._
import org.scalatest._
import scodec.bits.ByteVector

abstract class StorageActionsBase extends FlatSpec with Matchers with OptionValues {

  type TestStore =
    IStore[String, Pattern, String, StringsCaptor] with ITestableStore[String, Pattern]

  val logger: Logger = Logger[StorageActionsTests]

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestStore(f: TestStore => Unit): Unit
}

trait StorageActionsTests extends StorageActionsBase {

  /* Tests */

  "produce" should
    "persist a piece of data in the store" in withTestStore { store =>
    val key     = List("ch1")
    val keyHash = store.hashCs(key)

    val r = produce(store, key.head, "datum", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List(Datum("datum", persist = false))
      store.getPsK(txn, key) shouldBe Nil
    }

    r shouldBe None
    //store is not empty - we have 'A' stored
    store.isEmpty shouldBe false
  }

  "producing twice on the same channel" should
    "persist two pieces of data in the store" in withTestStore { store =>
    val key     = List("ch1")
    val keyHash = store.hashCs(key)

    val r1 = produce(store, key.head, "datum1", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List(Datum("datum1", persist = false))
      store.getPsK(txn, key) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = produce(store, key.head, "datum2", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) should contain theSameElementsAs List(Datum("datum1", persist = false),
                                                                  Datum("datum2", persist = false))
      store.getPsK(txn, key) shouldBe Nil
    }

    r2 shouldBe None
    //store is not empty - we have 2 As stored
    store.isEmpty shouldBe false
  }

  "consuming on one channel" should
    "persist a continuation in the store" in withTestStore { store =>
    val key      = List("ch1")
    val patterns = List(Wildcard)
    val keyHash  = store.hashCs(key)

    val r = consume(store, key, patterns, new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe List("ch1")
      store.getPs(txn, key) shouldBe List(patterns)
      store.getAs(txn, key) shouldBe Nil
      store.getPsK(txn, key) should not be empty
    }

    r shouldBe None
    //there is a continuation stored in the storage
    store.isEmpty shouldBe false
  }

  "consuming with a list of patterns that is a different length than the list of channels" should
    "throw" in withTestStore { store =>
    an[IllegalArgumentException] shouldBe thrownBy(
      consume(store, List("ch1", "ch2"), List(Wildcard), new StringsCaptor, persist = false))

    store.isEmpty shouldBe true
  }

  "consuming on three channels" should
    "persist a continuation in the store" in withTestStore { store =>
    val key      = List("ch1", "ch2", "ch3")
    val patterns = List(Wildcard, Wildcard, Wildcard)
    val keyHash  = store.hashCs(key)

    val r = consume(store, key, patterns, new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe List(patterns)
      store.getAs(txn, key) shouldBe Nil
      store.getPsK(txn, key) should not be empty
    }

    r shouldBe None
    //continuation is left in the storage
    store.isEmpty shouldBe false
  }

  "producing and then consuming on the same channel" should
    "return the continuation and data" in withTestStore { store =>
    val key     = List("ch1")
    val keyHash = store.hashCs(key)

    val r1 = produce(store, key.head, "datum", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List(Datum("datum", persist = false))
      store.getPsK(txn, key) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = consume(store, key, List(Wildcard), new StringsCaptor, persist = false)

    store.isEmpty shouldBe true

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe Nil
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe Nil
      store.getPsK(txn, key) shouldBe Nil
    }

    r2 shouldBe defined

    runK(r2)

    getK(r2).results should contain theSameElementsAs List(List("datum"))

    store.isEmpty shouldBe true
  }

  "producing three times then doing consuming three times" should "work" in withTestStore { store =>
    val r1 = produce(store, "ch1", "datum1", persist = false)
    val r2 = produce(store, "ch1", "datum2", persist = false)
    val r3 = produce(store, "ch1", "datum3", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None

    val r4 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    runK(r4)

    getK(r4).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    val r5 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    runK(r5)

    getK(r5).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    val r6 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    runK(r6)

    getK(r6).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    store.isEmpty shouldBe true
  }

  "producing on channel, consuming on that channel and another, and then producing on the other channel" should
    "return a continuation and all the data" in withTestStore { store =>
    val produceKey1     = List("ch1")
    val produceKey1Hash = store.hashCs(produceKey1)

    val r1 = produce(store, produceKey1.head, "datum1", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey1Hash) shouldBe produceKey1
      store.getPs(txn, produceKey1) shouldBe Nil
      store.getAs(txn, produceKey1) shouldBe List(Datum("datum1", persist = false))
      store.getPsK(txn, produceKey1) shouldBe Nil
    }

    r1 shouldBe None

    val consumeKey     = List("ch1", "ch2")
    val consumeKeyHash = store.hashCs(consumeKey)
    val consumePattern = List(Wildcard, Wildcard)

    val r2 = consume(store, consumeKey, consumePattern, new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey1Hash) shouldBe produceKey1
      store.getPs(txn, produceKey1) shouldBe Nil
      store.getAs(txn, produceKey1) shouldBe List(Datum("datum1", persist = false))
      store.getPsK(txn, produceKey1) shouldBe Nil
      store.getKey(txn, consumeKeyHash) shouldBe consumeKey
      store.getPs(txn, consumeKey) shouldBe List(consumePattern)
      store.getAs(txn, consumeKey) shouldBe Nil
      store.getPsK(txn, consumeKey) should not be empty
    }

    r2 shouldBe None

    val produceKey2     = List("ch2")
    val produceKey2Hash = store.hashCs(produceKey2)

    val r3 = produce(store, produceKey2.head, "datum2", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey1Hash) shouldBe Nil
      store.getPs(txn, produceKey1) shouldBe Nil
      store.getAs(txn, produceKey1) shouldBe Nil
      store.getPsK(txn, produceKey1) shouldBe Nil
      store.getKey(txn, consumeKeyHash) shouldBe Nil
      store.getPs(txn, consumeKey) shouldBe Nil
      store.getAs(txn, consumeKey) shouldBe Nil
      store.getPsK(txn, consumeKey) shouldBe Nil
      store.getKey(txn, produceKey2Hash) shouldBe Nil
      store.getPs(txn, produceKey2) shouldBe Nil
      store.getAs(txn, produceKey2) shouldBe Nil
      store.getPsK(txn, produceKey2) shouldBe Nil
    }

    r3 shouldBe defined

    runK(r3)

    getK(r3).results should contain theSameElementsAs List(List("datum1", "datum2"))

    store.isEmpty shouldBe true
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

    val r1 = produce(store, produceKey1.head, "datum1", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey1Hash) shouldBe produceKey1
      store.getPs(txn, produceKey1) shouldBe Nil
      store.getAs(txn, produceKey1) shouldBe List(Datum("datum1", persist = false))
      store.getPsK(txn, produceKey1) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = produce(store, produceKey2.head, "datum2", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey2Hash) shouldBe produceKey2
      store.getPs(txn, produceKey2) shouldBe Nil
      store.getAs(txn, produceKey2) shouldBe List(Datum("datum2", persist = false))
      store.getPsK(txn, produceKey2) shouldBe Nil
    }

    r2 shouldBe None

    val r3 = produce(store, produceKey3.head, "datum3", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, produceKey3Hash) shouldBe produceKey3
      store.getPs(txn, produceKey3) shouldBe Nil
      store.getAs(txn, produceKey3) shouldBe List(Datum("datum3", persist = false))
      store.getPsK(txn, produceKey3) shouldBe Nil
    }

    r3 shouldBe None

    val r4 = consume(store, List("ch1", "ch2", "ch3"), patterns, new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, consumeKeyHash) shouldBe Nil
      store.getPs(txn, consumeKey) shouldBe Nil
      store.getAs(txn, consumeKey) shouldBe Nil
      store.getPsK(txn, consumeKey) shouldBe Nil
    }

    r4 shouldBe defined

    runK(r4)

    getK(r4).results should contain theSameElementsAs List(List("datum1", "datum2", "datum3"))

    store.isEmpty shouldBe true
  }

  "producing three times on the same channel then consuming three times on the same channel" should
    "return three pairs of continuations and data" in withTestStore { store =>
    val captor = new StringsCaptor

    val key = List("ch1")

    val r1 = produce(store, key.head, "datum1", persist = false)
    val r2 = produce(store, key.head, "datum2", persist = false)
    val r3 = produce(store, key.head, "datum3", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None

    val r4 = consume(store, key, List(Wildcard), captor, persist = false)
    val r5 = consume(store, key, List(Wildcard), captor, persist = false)
    val r6 = consume(store, key, List(Wildcard), captor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, store.hashCs(key)) shouldBe Nil
      store.getAs(txn, key) shouldBe Nil
      store.getPs(txn, key) shouldBe Nil
      store.getPsK(txn, key) shouldBe Nil
    }

    val continuations = List(r4, r5, r6)

    continuations.forall(_.isDefined) shouldBe true

    continuations.foreach(runK)

    captor.results should contain theSameElementsAs List(List("datum3"),
                                                         List("datum2"),
                                                         List("datum1"))

    store.isEmpty shouldBe true
  }

  "consuming three times on the same channel, then producing three times on that channel" should
    "return three continuations, each paired with distinct pieces of data" in withTestStore {
    store =>
      consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      val r1 = produce(store, "ch1", "datum1", persist = false)
      val r2 = produce(store, "ch1", "datum2", persist = false)
      val r3 = produce(store, "ch1", "datum3", persist = false)

      r1 shouldBe defined
      r2 shouldBe defined
      r3 shouldBe defined

      List(r1, r2, r3).foreach(runK)

      getK(r1).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
      getK(r2).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
      getK(r3).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

      getK(r1).results shouldNot contain theSameElementsAs getK(r2).results
      getK(r1).results shouldNot contain theSameElementsAs getK(r3).results
      getK(r2).results shouldNot contain theSameElementsAs getK(r3).results

      store.isEmpty shouldBe true
  }

  "consuming three times on the same channel with non-trivial matches, then producing three times on that channel" should
    "return three continuations, each paired with matching data" in withTestStore { store =>
    consume(store, List("ch1"), List(StringMatch("datum1")), new StringsCaptor, persist = false)
    consume(store, List("ch1"), List(StringMatch("datum2")), new StringsCaptor, persist = false)
    consume(store, List("ch1"), List(StringMatch("datum3")), new StringsCaptor, persist = false)

    val r1 = produce(store, "ch1", "datum1", persist = false)
    val r2 = produce(store, "ch1", "datum2", persist = false)
    val r3 = produce(store, "ch1", "datum3", persist = false)

    r1 shouldBe defined
    r2 shouldBe defined
    r3 shouldBe defined

    List(r1, r2, r3).foreach(runK)

    getK(r1).results shouldBe List(List("datum1"))
    getK(r2).results shouldBe List(List("datum2"))
    getK(r3).results shouldBe List(List("datum3"))

    store.isEmpty shouldBe true
  }

  "consuming on two channels, producing on one, then producing on the other" should
    "return a continuation with both pieces of data" in withTestStore { store =>
    val r1 = consume(store,
                     List("ch1", "ch2"),
                     List(Wildcard, Wildcard),
                     new StringsCaptor,
                     persist = false)
    val r2 = produce(store, "ch1", "datum1", persist = false)
    val r3 = produce(store, "ch2", "datum2", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe defined

    runK(r3)

    getK(r3).results should contain theSameElementsAs List(List("datum1", "datum2"))

    store.isEmpty shouldBe true
  }

  "consuming twice on the same channels with different patterns, and then producing on those channels" should
    "return continuations with the expected data" in withTestStore { store =>
    val channels = List("ch1", "ch2")

    val r1 = consume(store,
                     channels,
                     List(StringMatch("datum1"), StringMatch("datum2")),
                     new StringsCaptor,
                     persist = false)
    val r2 = consume(store,
                     channels,
                     List(StringMatch("datum3"), StringMatch("datum4")),
                     new StringsCaptor,
                     persist = false)

    val r3 = produce(store, "ch1", "datum3", persist = false)
    val r4 = produce(store, "ch2", "datum4", persist = false)
    val r5 = produce(store, "ch1", "datum1", persist = false)
    val r6 = produce(store, "ch2", "datum2", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None
    r4 shouldBe defined
    r5 shouldBe None
    r6 shouldBe defined

    List(r4, r6).foreach(runK)

    getK(r4).results should contain theSameElementsAs List(List("datum3", "datum4"))
    getK(r6).results should contain theSameElementsAs List(List("datum1", "datum2"))

    store.isEmpty shouldBe true
  }

  "consuming and producing with non-trivial matches" should
    "work" in withTestStore { store =>
    val r1 = consume(
      store,
      List("ch1", "ch2"),
      List(Wildcard, StringMatch("datum1")),
      new StringsCaptor,
      persist = false
    )

    val r2 = produce(store, "ch1", "datum1", persist = false)

    r1 shouldBe None
    r2 shouldBe None

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1", "ch2")) shouldBe Nil
      store.getAs(txn, List("ch1")) shouldBe List(Datum("datum1", persist = false))
    }

    store.withTxn(store.createTxnRead()) { txn =>
      store.getPsK(txn, List("ch1", "ch2")) should not be empty
      store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
      store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
    }

    store.isEmpty shouldBe false
  }

  "consuming twice and producing twice with non-trivial matches" should
    "work" in withTestStore { store =>
    val r1 =
      consume(store, List("ch1"), List(StringMatch("datum1")), new StringsCaptor, persist = false)
    val r2 =
      consume(store, List("ch2"), List(StringMatch("datum2")), new StringsCaptor, persist = false)
    val r3 = produce(store, "ch1", "datum1", persist = false)
    val r4 = produce(store, "ch2", "datum2", persist = false)

    List(r1, r2, r3, r4).foreach(runK)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1")) shouldBe Nil
      store.getAs(txn, List("ch2")) shouldBe Nil
    }

    getK(r3).results should contain theSameElementsAs List(List("datum1"))
    getK(r4).results should contain theSameElementsAs List(List("datum2"))

    store.isEmpty shouldBe true
  }

  "consuming on two channels, consuming on one of those channels, and then producing on both of those channels separately" should
    "return a continuation paired with one piece of data" in
    withTestStore { store =>
      consume(store,
              List("ch1", "ch2"),
              List(Wildcard, Wildcard),
              new StringsCaptor,
              persist = false)
      consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      val r3 = produce(store, "ch1", "datum1", persist = false)
      val r4 = produce(store, "ch2", "datum2", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getPsK(txn, List("ch1", "ch2")) should not be empty
        store.getPsK(txn, List("ch1")) shouldBe Nil
        store.getPsK(txn, List("ch2")) shouldBe Nil
        store.getAs(txn, List("ch1")) shouldBe Nil
        store.getAs(txn, List("ch2")) shouldBe List(Datum("datum2", persist = false))
      }

      r3 shouldBe defined
      r4 shouldBe None

      runK(r3)

      getK(r3).results should contain theSameElementsAs List(List("datum1"))
      //ensure that joins are cleaned-up after all
      store.withTxn(store.createTxnRead()) { txn =>
        store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
        store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
      }

      store.isEmpty shouldBe false
    }

  /* Persist tests */

  "producing and then doing a persistent consume on the same channel" should
    "return the continuation and data" in withTestStore { store =>
    val key     = List("ch1")
    val keyHash = store.hashCs(key)

    val r1 = produce(store, key.head, "datum", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe key
      store.getPs(txn, key) shouldBe Nil
      store.getAs(txn, key) shouldBe List(Datum("datum", persist = false))
      store.getPsK(txn, key) shouldBe Nil
    }

    r1 shouldBe None

    // Data exists so the write will not "stick"
    val r2 = consume(store, key, List(Wildcard), new StringsCaptor, persist = true)

    store.isEmpty shouldBe true

    r2 shouldBe defined

    runK(r2)

    getK(r2).results should contain theSameElementsAs List(List("datum"))

    // the data has been consumed, so the write will "stick"
    val r3 = consume(store, key, List(Wildcard), new StringsCaptor, persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getKey(txn, keyHash) shouldBe List("ch1")
      store.getPs(txn, key) shouldBe List(List(Wildcard))
      store.getAs(txn, key) shouldBe Nil
      store.getPsK(txn, key) should not be empty
    }

    r3 shouldBe None
  }

  "producing, doing a persistent consume, and producing again on the same channel" should
    "return the continuation for the first produce, and then the second produce" in withTestStore {
    store =>
      val key     = List("ch1")
      val keyHash = store.hashCs(key)

      val r1 = produce(store, key.head, "datum1", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getKey(txn, keyHash) shouldBe key
        store.getPs(txn, key) shouldBe Nil
        store.getAs(txn, key) shouldBe List(Datum("datum1", persist = false))
        store.getPsK(txn, key) shouldBe Nil
      }

      r1 shouldBe None

      // Matching data exists so the write will not "stick"
      val r2 = consume(store, key, List(Wildcard), new StringsCaptor, persist = true)

      store.isEmpty shouldBe true

      r2 shouldBe defined

      runK(r2)

      getK(r2).results should contain theSameElementsAs List(List("datum1"))

      // All matching data has been consumed, so the write will "stick"
      val r3 = consume(store, key, List(Wildcard), new StringsCaptor, persist = true)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getKey(txn, keyHash) shouldBe List("ch1")
        store.getPs(txn, key) shouldBe List(List(Wildcard))
        store.getAs(txn, key) shouldBe Nil
        store.getPsK(txn, key) should not be empty
      }

      r3 shouldBe None

      val r4 = produce(store, key.head, "datum2", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getKey(txn, keyHash) shouldBe List("ch1")
        store.getPs(txn, key) shouldBe List(List(Wildcard))
        store.getAs(txn, key) shouldBe Nil
        store.getPsK(txn, key) should not be empty
      }

      r4 shouldBe defined

      runK(r4)

      getK(r4).results should contain theSameElementsAs List(List("datum2"))
  }

  "doing a persistent consume and producing multiple times" should "work" in withTestStore {
    store =>
      val r1 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getAs(txn, List("ch1")) shouldBe Nil
        store.getPsK(txn, List("ch1")) should not be empty
      }

      r1 shouldBe None

      val r2 = produce(store, "ch1", "datum1", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getAs(txn, List("ch1")) shouldBe Nil
        store.getPsK(txn, List("ch1")) should not be empty
      }

      r2 shouldBe defined

      runK(r2)

      getK(r2).results should contain theSameElementsAs List(List("datum1"))

      val r3 = produce(store, "ch1", "datum2", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getAs(txn, List("ch1")) shouldBe Nil
        store.getPsK(txn, List("ch1")) should not be empty
      }

      r3 shouldBe defined

      runK(r3)

      getK(r3).results should contain theSameElementsAs List(List("datum2"))
  }

  "consuming and doing a persistient produce" should "work" in withTestStore { store =>
    val r1 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    r1 shouldBe None

    // A matching continuation exists so the write will not "stick"
    val r2 = produce(store, "ch1", "datum1", persist = true)

    store.isEmpty shouldBe true

    r2 shouldBe defined

    runK(r2)

    getK(r2).results should contain theSameElementsAs List(List("datum1"))

    // All matching continuations have been produced, so the write will "stick"
    val r3 = produce(store, "ch1", "datum1", persist = true)

    r3 shouldBe None

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
      store.getPsK(txn, List("ch1")) shouldBe Nil
    }
  }

  "consuming, doing a persistient produce, and consuming again" should "work" in withTestStore {
    store =>
      val r1 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      r1 shouldBe None

      // A matching continuation exists so the write will not "stick"
      val r2 = produce(store, "ch1", "datum1", persist = true)

      store.isEmpty shouldBe true

      r2 shouldBe defined

      runK(r2)

      getK(r2).results should contain theSameElementsAs List(List("datum1"))

      // All matching continuations have been produced, so the write will "stick"
      val r3 = produce(store, "ch1", "datum1", persist = true)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getAs(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
        store.getPsK(txn, List("ch1")) shouldBe Nil
      }

      r3 shouldBe None

      val r4 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getAs(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
        store.getPsK(txn, List("ch1")) shouldBe Nil
      }

      r4 shouldBe defined

      runK(r4)

      getK(r4).results should contain theSameElementsAs List(List("datum1"))

  }

  "doing a persistent produce and consuming twice" should "work" in withTestStore { store =>
    val r1 = produce(store, "ch1", "datum1", persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
      store.getPsK(txn, List("ch1")) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
      store.getPsK(txn, List("ch1")) shouldBe Nil
    }

    r2 shouldBe defined

    runK(r2)

    getK(r2).results should contain theSameElementsAs List(List("datum1"))

    val r3 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
      store.getPsK(txn, List("ch1")) shouldBe Nil
    }

    r3 shouldBe defined

    runK(r3)

    getK(r3).results should contain theSameElementsAs List(List("datum1"))
  }

  "producing three times and doing a persistent consume" should "work" in withTestStore { store =>
    val r1 = produce(store, "ch1", "datum1", persist = false)
    val r2 = produce(store, "ch1", "datum2", persist = false)
    val r3 = produce(store, "ch1", "datum3", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None

    // Matching data exists so the write will not "stick"
    val r4 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1")) should contain atLeastOneOf (
        Datum("datum1", persist = false),
        Datum("datum2", persist = false),
        Datum("datum3", persist = false)
      )
      store.getPsK(txn, List("ch1")) shouldBe Nil
    }

    r4 shouldBe defined

    runK(r4)

    getK(r4).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    // Matching data exists so the write will not "stick"
    val r5 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1")) should contain oneOf (
        Datum("datum1", persist = false),
        Datum("datum2", persist = false),
        Datum("datum3", persist = false)
      )
      store.getPsK(txn, List("ch1")) shouldBe Nil
    }

    r5 shouldBe defined

    runK(r5)

    getK(r5).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    // Matching data exists so the write will not "stick"
    val r6 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

    store.isEmpty shouldBe true

    r6 shouldBe defined

    runK(r6)

    getK(r6).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    // All matching data has been consumed, so the write will "stick"
    val r7 = consume(store, List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getAs(txn, List("ch1")) shouldBe Nil
      store.getPsK(txn, List("ch1")) should not be empty
    }

    r7 shouldBe None
  }
}

class InMemoryStoreStorageActionsTests extends StorageActionsTests with JoinOperationsTests {

  override def withTestStore(f: TestStore => Unit): Unit = {
    val testStore = InMemoryStore.create[String, Pattern, String, StringsCaptor]
    try {
      f(testStore)
    } finally {
      testStore.close()
    }
  }
}

class LMDBStoreStorageActionsTests
    extends StorageActionsTests
    with JoinOperationsTests
    with BeforeAndAfterAll {

  private[this] val dbDir = Files.createTempDirectory("rchain-storage-test-")

  override def withTestStore(f: TestStore => Unit): Unit = {
    val testStore =
      LMDBStore.create[String, Pattern, String, StringsCaptor](dbDir, 1024 * 1024 * 1024)
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

class LMDBStoreScodecActionsTests
    extends StorageActionsTests
    with JoinOperationsTests
    with BeforeAndAfterAll {

  private[this] val dbDir = Files.createTempDirectory("rchain-storage-test-")

  override def withTestStore(f: TestStore => Unit): Unit = {
    val testStore =
      LMDBStoreScodec.create[String, Pattern, String, StringsCaptor](dbDir, 1024 * 1024 * 1024)
    testStore.clear()
    try {
      f(testStore)
    } finally {
      testStore.close()
    }
  }

  override def afterAll(): Unit =
    recursivelyDeletePath(dbDir)

  import coop.rchain.rspace.scodecmodels._
  import coop.rchain.rspace.scodecmodels.rscodecs._

  "asBytesList" should "round-trip a list" in {
    val bll =
      AsBytesList(
        List(AsBytes(ByteVector(Array[Byte](1, 2, 3)), false),
             AsBytes(ByteVector(Array[Byte](4, 5, 6)), true)))

    val arr = toBitVector(bll, rscodecs.asBytesListCodec)
    val res = fromBitVector(arr, rscodecs.asBytesListCodec)

    res shouldBe bll
  }

  "asData" should "round-trip" in {
    val asd = AsBytes(ByteVector(Array[Byte](4, 5, 6)), true)

    val arr = toBitVector(asd, rscodecs.asBytesCodec)
    val res = fromBitVector(arr, rscodecs.asBytesCodec)

    res shouldBe asd
  }

  "PsKs" should "round-trip" in {
    val psks = PsKsBytesList(
      List(
        PsKsBytes(BytesList(List(ByteVector(Array[Byte](1, 2, 3)))),
                  ByteVector(Array[Byte](4, 5, 6)),
                  true)))

    val arr = toBitVector(psks, rscodecs.psKsBytesListCodec)
    val res = fromBitVector(arr, rscodecs.psKsBytesListCodec)

    res shouldBe psks
  }
}
