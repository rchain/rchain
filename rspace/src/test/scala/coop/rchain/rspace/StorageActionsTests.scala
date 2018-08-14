package coop.rchain.rspace

import java.lang.{Byte => JByte}

import com.google.common.collect.HashMultiset
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.{Leaf, LeafPointer, Node, NodePointer, PointerBlock, Skip, Trie}
import coop.rchain.rspace.util._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import org.scalacheck.Prop
import org.scalatest._
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import scodec.Codec

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

import coop.rchain.rspace.test.ArbitraryInstances._

trait StorageActionsTests
    extends StorageTestsBase[String, Pattern, String, StringsCaptor]
    with GeneratorDrivenPropertyChecks
    with Checkers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 5, sizeRange = 30)

  implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
  implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
  implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

  type TestProduceMap = Map[String, Datum[String]]

  type TestConsumeMap = Map[List[String], WaitingContinuation[Pattern, StringsCaptor]]

  type TestGNAT = GNAT[String, Pattern, String, StringsCaptor]

  case class State(
      checkpoint: Blake2b256Hash,
      contents: Map[Seq[String], Row[Pattern, String, StringsCaptor]],
      joins: Map[Blake2b256Hash, Seq[Seq[String]]]
  )

  "produce" should
    "persist a piece of data in the store" in withTestSpace { space =>
    val store   = space.store
    val key     = List("ch1")
    val keyHash = store.hashChannels(key)

    val r = space.produce(key.head, "datum", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe key
      store.getPatterns(txn, key) shouldBe Nil
      store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum", persist = false))
      store.getWaitingContinuation(txn, key) shouldBe Nil
    }

    r shouldBe None
    //store is not empty - we have 'A' stored
    store.isEmpty shouldBe false
  }

  "producing twice on the same channel" should
    "persist two pieces of data in the store" in withTestSpace { space =>
    val store   = space.store
    val key     = List("ch1")
    val keyHash = store.hashChannels(key)

    val r1 = space.produce(key.head, "datum1", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe key
      store.getPatterns(txn, key) shouldBe Nil
      store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum1", false))
      store.getWaitingContinuation(txn, key) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = space.produce(key.head, "datum2", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe key
      store.getPatterns(txn, key) shouldBe Nil
      store.getData(txn, key) should contain theSameElementsAs List(
        Datum.create(key.head, "datum1", false),
        Datum.create(key.head, "datum2", false))
      store.getWaitingContinuation(txn, key) shouldBe Nil
    }

    r2 shouldBe None
    //store is not empty - we have 2 As stored
    store.isEmpty shouldBe false
  }

  "consuming on one channel" should
    "persist a continuation in the store" in withTestSpace { space =>
    val store    = space.store
    val key      = List("ch1")
    val patterns = List(Wildcard)
    val keyHash  = store.hashChannels(key)

    val r = space.consume(key, patterns, new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe List("ch1")
      store.getPatterns(txn, key) shouldBe List(patterns)
      store.getData(txn, key) shouldBe Nil
      store.getWaitingContinuation(txn, key) should not be empty
    }

    r shouldBe None
    //there is a continuation stored in the storage
    store.isEmpty shouldBe false
  }

  "consuming with a list of patterns that is a different length than the list of channels" should
    "throw" in withTestSpace { space =>
    an[IllegalArgumentException] shouldBe thrownBy(
      space.consume(List("ch1", "ch2"), List(Wildcard), new StringsCaptor, persist = false))

    val store = space.store

    store.isEmpty shouldBe true
  }

  "consuming on three channels" should
    "persist a continuation in the store" in withTestSpace { space =>
    val store    = space.store
    val key      = List("ch1", "ch2", "ch3")
    val patterns = List(Wildcard, Wildcard, Wildcard)
    val keyHash  = store.hashChannels(key)

    val r = space.consume(key, patterns, new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe key
      store.getPatterns(txn, key) shouldBe List(patterns)
      store.getData(txn, key) shouldBe Nil
      store.getWaitingContinuation(txn, key) should not be empty
    }

    r shouldBe None
    //continuation is left in the storage
    store.isEmpty shouldBe false
  }

  "producing and then consuming on the same channel" should
    "return the continuation and data" in withTestSpace { space =>
    val store   = space.store
    val key     = List("ch1")
    val keyHash = store.hashChannels(key)

    val r1 = space.produce(key.head, "datum", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe key
      store.getPatterns(txn, key) shouldBe Nil
      store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum", false))
      store.getWaitingContinuation(txn, key) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = space.consume(key, List(Wildcard), new StringsCaptor, persist = false)

    store.isEmpty shouldBe true

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe Nil
      store.getPatterns(txn, key) shouldBe Nil
      store.getData(txn, key) shouldBe Nil
      store.getWaitingContinuation(txn, key) shouldBe Nil
    }

    r2 shouldBe defined

    runK(r2)

    getK(r2).results should contain theSameElementsAs List(List("datum"))

    store.isEmpty shouldBe true
  }

  "producing three times then doing consuming three times" should "work" in withTestSpace { space =>
    val store = space.store
    val r1    = space.produce("ch1", "datum1", persist = false)
    val r2    = space.produce("ch1", "datum2", persist = false)
    val r3    = space.produce("ch1", "datum3", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None

    val r4 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    runK(r4)

    getK(r4).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    val r5 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    runK(r5)

    getK(r5).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    val r6 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    runK(r6)

    getK(r6).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    store.isEmpty shouldBe true
  }

  "producing on channel, consuming on that channel and another, and then producing on the other channel" should
    "return a continuation and all the data" in withTestSpace { space =>
    val store           = space.store
    val produceKey1     = List("ch1")
    val produceKey1Hash = store.hashChannels(produceKey1)

    val r1 = space.produce(produceKey1.head, "datum1", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, produceKey1Hash) shouldBe produceKey1
      store.getPatterns(txn, produceKey1) shouldBe Nil
      store.getData(txn, produceKey1) shouldBe List(
        Datum.create(produceKey1.head, "datum1", persist = false))
      store.getWaitingContinuation(txn, produceKey1) shouldBe Nil
    }

    r1 shouldBe None

    val consumeKey     = List("ch1", "ch2")
    val consumeKeyHash = store.hashChannels(consumeKey)
    val consumePattern = List(Wildcard, Wildcard)

    val r2 = space.consume(consumeKey, consumePattern, new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, produceKey1Hash) shouldBe produceKey1
      store.getPatterns(txn, produceKey1) shouldBe Nil
      store.getData(txn, produceKey1) shouldBe List(
        Datum.create(produceKey1.head, "datum1", persist = false))
      store.getWaitingContinuation(txn, produceKey1) shouldBe Nil
      store.getChannels(txn, consumeKeyHash) shouldBe consumeKey
      store.getPatterns(txn, consumeKey) shouldBe List(consumePattern)
      store.getData(txn, consumeKey) shouldBe Nil
      store.getWaitingContinuation(txn, consumeKey) should not be empty
    }

    r2 shouldBe None

    val produceKey2     = List("ch2")
    val produceKey2Hash = store.hashChannels(produceKey2)

    val r3 = space.produce(produceKey2.head, "datum2", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, produceKey1Hash) shouldBe Nil
      store.getPatterns(txn, produceKey1) shouldBe Nil
      store.getData(txn, produceKey1) shouldBe Nil
      store.getWaitingContinuation(txn, produceKey1) shouldBe Nil
      store.getChannels(txn, consumeKeyHash) shouldBe Nil
      store.getPatterns(txn, consumeKey) shouldBe Nil
      store.getData(txn, consumeKey) shouldBe Nil
      store.getWaitingContinuation(txn, consumeKey) shouldBe Nil
      store.getChannels(txn, produceKey2Hash) shouldBe Nil
      store.getPatterns(txn, produceKey2) shouldBe Nil
      store.getData(txn, produceKey2) shouldBe Nil
      store.getWaitingContinuation(txn, produceKey2) shouldBe Nil
    }

    r3 shouldBe defined

    runK(r3)

    getK(r3).results should contain theSameElementsAs List(List("datum1", "datum2"))

    store.isEmpty shouldBe true
  }

  "producing on three different channels and then consuming once on all three" should
    "return the continuation and all the data" in withTestSpace { space =>
    val store           = space.store
    val produceKey1     = List("ch1")
    val produceKey2     = List("ch2")
    val produceKey3     = List("ch3")
    val consumeKey      = List("ch1", "ch2", "ch3")
    val patterns        = List(Wildcard, Wildcard, Wildcard)
    val produceKey1Hash = store.hashChannels(produceKey1)
    val produceKey2Hash = store.hashChannels(produceKey2)
    val produceKey3Hash = store.hashChannels(produceKey3)
    val consumeKeyHash  = store.hashChannels(consumeKey)

    val r1 = space.produce(produceKey1.head, "datum1", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, produceKey1Hash) shouldBe produceKey1
      store.getPatterns(txn, produceKey1) shouldBe Nil
      store.getData(txn, produceKey1) shouldBe List(Datum.create(produceKey1.head, "datum1", false))
      store.getWaitingContinuation(txn, produceKey1) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = space.produce(produceKey2.head, "datum2", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, produceKey2Hash) shouldBe produceKey2
      store.getPatterns(txn, produceKey2) shouldBe Nil
      store.getData(txn, produceKey2) shouldBe List(Datum.create(produceKey2.head, "datum2", false))
      store.getWaitingContinuation(txn, produceKey2) shouldBe Nil
    }

    r2 shouldBe None

    val r3 = space.produce(produceKey3.head, "datum3", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, produceKey3Hash) shouldBe produceKey3
      store.getPatterns(txn, produceKey3) shouldBe Nil
      store.getData(txn, produceKey3) shouldBe List(Datum.create(produceKey3.head, "datum3", false))
      store.getWaitingContinuation(txn, produceKey3) shouldBe Nil
    }

    r3 shouldBe None

    val r4 = space.consume(List("ch1", "ch2", "ch3"), patterns, new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, consumeKeyHash) shouldBe Nil
      store.getPatterns(txn, consumeKey) shouldBe Nil
      store.getData(txn, consumeKey) shouldBe Nil
      store.getWaitingContinuation(txn, consumeKey) shouldBe Nil
    }

    r4 shouldBe defined

    runK(r4)

    getK(r4).results should contain theSameElementsAs List(List("datum1", "datum2", "datum3"))

    store.isEmpty shouldBe true
  }

  "producing three times on the same channel then consuming three times on the same channel" should
    "return three pairs of continuations and data" in withTestSpace { space =>
    val store  = space.store
    val captor = new StringsCaptor

    val key = List("ch1")

    val r1 = space.produce(key.head, "datum1", persist = false)
    val r2 = space.produce(key.head, "datum2", persist = false)
    val r3 = space.produce(key.head, "datum3", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None

    val r4 = space.consume(key, List(Wildcard), captor, persist = false)
    val r5 = space.consume(key, List(Wildcard), captor, persist = false)
    val r6 = space.consume(key, List(Wildcard), captor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, store.hashChannels(key)) shouldBe Nil
      store.getData(txn, key) shouldBe Nil
      store.getPatterns(txn, key) shouldBe Nil
      store.getWaitingContinuation(txn, key) shouldBe Nil
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
    "return three continuations, each paired with distinct pieces of data" in withTestSpace {
    space =>
      val store = space.store

      space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      val r1 = space.produce("ch1", "datum1", persist = false)
      val r2 = space.produce("ch1", "datum2", persist = false)
      val r3 = space.produce("ch1", "datum3", persist = false)

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
    "return three continuations, each paired with matching data" in withTestSpace { space =>
    val store = space.store

    space.consume(List("ch1"), List(StringMatch("datum1")), new StringsCaptor, persist = false)
    space.consume(List("ch1"), List(StringMatch("datum2")), new StringsCaptor, persist = false)
    space.consume(List("ch1"), List(StringMatch("datum3")), new StringsCaptor, persist = false)

    val r1 = space.produce("ch1", "datum1", persist = false)
    val r2 = space.produce("ch1", "datum2", persist = false)
    val r3 = space.produce("ch1", "datum3", persist = false)

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
    "return a continuation with both pieces of data" in withTestSpace { space =>
    val store = space.store

    val r1 = space.consume(List("ch1", "ch2"),
                           List(Wildcard, Wildcard),
                           new StringsCaptor,
                           persist = false)
    val r2 = space.produce("ch1", "datum1", persist = false)
    val r3 = space.produce("ch2", "datum2", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe defined

    runK(r3)

    getK(r3).results should contain theSameElementsAs List(List("datum1", "datum2"))

    store.isEmpty shouldBe true

  }

  "A joined consume with the same channel given twice followed by a produce" should
    "not raises any errors (CORE-365)" in withTestSpace { space =>
    val store = space.store

    val channels = List("ch1", "ch1")

    val r1 = space.consume(channels,
                           List(StringMatch("datum1"), StringMatch("datum1")),
                           new StringsCaptor,
                           persist = false)

    val r2 = space.produce("ch1", "datum1", persist = false)
    val r3 = space.produce("ch1", "datum1", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe defined

    runK(r3)
    getK(r3).results shouldBe List(List("datum1", "datum1"))

    store.isEmpty shouldBe true

  }

  "consuming twice on the same channels with different patterns, and then producing on those channels" should
    "return continuations with the expected data" in withTestSpace { space =>
    val store = space.store

    val channels = List("ch1", "ch2")

    val r1 = space.consume(channels,
                           List(StringMatch("datum1"), StringMatch("datum2")),
                           new StringsCaptor,
                           persist = false)
    val r2 = space.consume(channels,
                           List(StringMatch("datum3"), StringMatch("datum4")),
                           new StringsCaptor,
                           persist = false)

    val r3 = space.produce("ch1", "datum3", persist = false)
    val r4 = space.produce("ch2", "datum4", persist = false)
    val r5 = space.produce("ch1", "datum1", persist = false)
    val r6 = space.produce("ch2", "datum2", persist = false)

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
    "work" in withTestSpace { space =>
    val store = space.store

    val r1 = space.consume(
      List("ch1", "ch2"),
      List(Wildcard, StringMatch("datum1")),
      new StringsCaptor,
      persist = false
    )

    val r2 = space.produce("ch1", "datum1", persist = false)

    r1 shouldBe None
    r2 shouldBe None

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1", "ch2")) shouldBe Nil
      store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", false))
    }

    store.withTxn(store.createTxnRead()) { txn =>
      store.getWaitingContinuation(txn, List("ch1", "ch2")) should not be empty
      store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
      store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
    }

    store.isEmpty shouldBe false
  }

  "consuming twice and producing twice with non-trivial matches" should
    "work" in withTestSpace { space =>
    val store = space.store

    val r1 =
      space.consume(List("ch1"), List(StringMatch("datum1")), new StringsCaptor, persist = false)
    val r2 =
      space.consume(List("ch2"), List(StringMatch("datum2")), new StringsCaptor, persist = false)
    val r3 = space.produce("ch1", "datum1", persist = false)
    val r4 = space.produce("ch2", "datum2", persist = false)

    List(r1, r2, r3, r4).foreach(runK)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) shouldBe Nil
      store.getData(txn, List("ch2")) shouldBe Nil
    }

    getK(r3).results should contain theSameElementsAs List(List("datum1"))
    getK(r4).results should contain theSameElementsAs List(List("datum2"))

    store.isEmpty shouldBe true

  }

  "consuming on two channels, consuming on one of those channels, and then producing on both of those channels separately" should
    "return a continuation paired with one piece of data" in
    withTestSpace { space =>
      val store = space.store

      space.consume(List("ch1", "ch2"),
                    List(Wildcard, Wildcard),
                    new StringsCaptor,
                    persist = false)
      space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      val r3 = space.produce("ch1", "datum1", persist = false)
      val r4 = space.produce("ch2", "datum2", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getWaitingContinuation(txn, List("ch1", "ch2")) should not be empty
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
        store.getWaitingContinuation(txn, List("ch2")) shouldBe Nil
        store.getData(txn, List("ch1")) shouldBe Nil
        store.getData(txn, List("ch2")) shouldBe List(Datum.create("ch2", "datum2", false))
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
    "return the continuation and data" in withTestSpace { space =>
    val store   = space.store
    val key     = List("ch1")
    val keyHash = store.hashChannels(key)

    val r1 = space.produce(key.head, "datum", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe key
      store.getPatterns(txn, key) shouldBe Nil
      store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum", false))
      store.getWaitingContinuation(txn, key) shouldBe Nil
    }

    r1 shouldBe None

    // Data exists so the write will not "stick"
    val r2 = space.consume(key, List(Wildcard), new StringsCaptor, persist = true)

    store.isEmpty shouldBe true

    r2 shouldBe defined

    runK(r2)

    getK(r2).results should contain theSameElementsAs List(List("datum"))

    // the data has been consumed, so the write will "stick"
    val r3 = space.consume(key, List(Wildcard), new StringsCaptor, persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe List("ch1")
      store.getPatterns(txn, key) shouldBe List(List(Wildcard))
      store.getData(txn, key) shouldBe Nil
      store.getWaitingContinuation(txn, key) should not be empty
    }

    r3 shouldBe None
  }

  "producing, doing a persistent consume, and producing again on the same channel" should
    "return the continuation for the first produce, and then the second produce" in withTestSpace {
    space =>
      val store   = space.store
      val key     = List("ch1")
      val keyHash = store.hashChannels(key)

      val r1 = space.produce(key.head, "datum1", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe key
        store.getPatterns(txn, key) shouldBe Nil
        store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum1", false))
        store.getWaitingContinuation(txn, key) shouldBe Nil
      }

      r1 shouldBe None

      // Matching data exists so the write will not "stick"
      val r2 = space.consume(key, List(Wildcard), new StringsCaptor, persist = true)

      store.isEmpty shouldBe true

      r2 shouldBe defined

      runK(r2)

      getK(r2).results should contain theSameElementsAs List(List("datum1"))

      // All matching data has been consumed, so the write will "stick"
      val r3 = space.consume(key, List(Wildcard), new StringsCaptor, persist = true)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe List("ch1")
        store.getPatterns(txn, key) shouldBe List(List(Wildcard))
        store.getData(txn, key) shouldBe Nil
        store.getWaitingContinuation(txn, key) should not be empty
      }

      r3 shouldBe None

      val r4 = space.produce(key.head, "datum2", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe List("ch1")
        store.getPatterns(txn, key) shouldBe List(List(Wildcard))
        store.getData(txn, key) shouldBe Nil
        store.getWaitingContinuation(txn, key) should not be empty
      }

      r4 shouldBe defined

      runK(r4)

      getK(r4).results should contain theSameElementsAs List(List("datum2"))
  }

  "doing a persistent consume and producing multiple times" should "work" in withTestSpace {
    space =>
      val store = space.store

      val r1 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe Nil
        store.getWaitingContinuation(txn, List("ch1")) should not be empty
      }

      r1 shouldBe None

      val r2 = space.produce("ch1", "datum1", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe Nil
        store.getWaitingContinuation(txn, List("ch1")) should not be empty
      }

      r2 shouldBe defined

      runK(r2)

      getK(r2).results should contain theSameElementsAs List(List("datum1"))

      val r3 = space.produce("ch1", "datum2", persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe Nil
        store.getWaitingContinuation(txn, List("ch1")) should not be empty
      }

      r3 shouldBe defined

      runK(r3)

      getK(r3).results should contain theSameElementsAs List(List("datum2"))
  }

  "consuming and doing a persistient produce" should "work" in withTestSpace { space =>
    val store = space.store

    val r1 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    r1 shouldBe None

    // A matching continuation exists so the write will not "stick"
    val r2 = space.produce("ch1", "datum1", persist = true)

    store.isEmpty shouldBe true

    r2 shouldBe defined

    runK(r2)

    getK(r2).results should contain theSameElementsAs List(List("datum1"))

    // All matching continuations have been produced, so the write will "stick"
    val r3 = space.produce("ch1", "datum1", persist = true)

    r3 shouldBe None

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }
  }

  "consuming, doing a persistient produce, and consuming again" should "work" in withTestSpace {
    space =>
      val store = space.store

      val r1 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      r1 shouldBe None

      // A matching continuation exists so the write will not "stick"
      val r2 = space.produce("ch1", "datum1", persist = true)

      store.isEmpty shouldBe true

      r2 shouldBe defined

      runK(r2)

      getK(r2).results should contain theSameElementsAs List(List("datum1"))

      // All matching continuations have been produced, so the write will "stick"
      val r3 = space.produce("ch1", "datum1", persist = true)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      }

      r3 shouldBe None

      val r4 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      }

      r4 shouldBe defined

      runK(r4)

      getK(r4).results should contain theSameElementsAs List(List("datum1"))
  }

  "doing a persistent produce and consuming twice" should "work" in withTestSpace { space =>
    val store = space.store

    val r1 = space.produce("ch1", "datum1", persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }

    r2 shouldBe defined

    runK(r2)

    getK(r2).results should contain theSameElementsAs List(List("datum1"))

    val r3 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }

    r3 shouldBe defined

    runK(r3)

    getK(r3).results should contain theSameElementsAs List(List("datum1"))
  }

  "producing three times and doing a persistent consume" should "work" in withTestSpace { space =>
    val store = space.store

    val r1 = space.produce("ch1", "datum1", persist = false)
    val r2 = space.produce("ch1", "datum2", persist = false)
    val r3 = space.produce("ch1", "datum3", persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None

    // Matching data exists so the write will not "stick"
    val r4 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) should contain atLeastOneOf (
        Datum.create("ch1", "datum1", false),
        Datum.create("ch1", "datum2", false),
        Datum.create("ch1", "datum3", false)
      )
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }

    r4 shouldBe defined

    runK(r4)

    getK(r4).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    // Matching data exists so the write will not "stick"
    val r5 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) should contain oneOf (
        Datum.create("ch1", "datum1", false),
        Datum.create("ch1", "datum2", false),
        Datum.create("ch1", "datum3", false)
      )
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }

    r5 shouldBe defined

    runK(r5)

    getK(r5).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    // Matching data exists so the write will not "stick"
    val r6 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

    store.isEmpty shouldBe true

    r6 shouldBe defined

    runK(r6)

    getK(r6).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

    // All matching data has been consumed, so the write will "stick"
    val r7 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) shouldBe Nil
      store.getWaitingContinuation(txn, List("ch1")) should not be empty
    }

    r7 shouldBe None

  }

  "A persistent produce" should "be available for multiple matches (CORE-633)" in withTestSpace {
    space =>
      val channel = "chan"

      val r1 = space.produce(channel, data = "datum", persist = true)

      r1 shouldBe None

      val r2 = space.consume(
        List(channel, channel),
        List(Wildcard, Wildcard),
        new StringsCaptor,
        persist = false
      )

      r2 shouldBe defined

      runK(r2)

      getK(r2).results should contain(List("datum", "datum"))
  }

  "reset" should "change the state of the store, and reset the trie updates log" in withTestSpace {
    space =>
      val checkpoint0 = space.createCheckpoint()

      val store    = space.store
      val key      = List("ch1")
      val patterns = List(Wildcard)

      val r = space.consume(key, patterns, new StringsCaptor, persist = false)

      r shouldBe None
      store.isEmpty shouldBe false
      store.getTrieUpdates.length shouldBe 1
      store.getTrieUpdateCount shouldBe 1

      space.reset(checkpoint0.root)

      store.isEmpty shouldBe true
      store.getTrieUpdates.length shouldBe 0
      store.getTrieUpdateCount shouldBe 0

      val checkpoint1 = space.createCheckpoint()
      checkpoint1.log shouldBe empty
  }

  "clear" should "empty the store, reset the event log, and reset the trie updates log" in withTestSpace {
    space =>
      val store    = space.store
      val key      = List("ch1")
      val patterns = List(Wildcard)
      val keyHash  = store.hashChannels(key)

      val r = space.consume(key, patterns, new StringsCaptor, persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe List("ch1")
        store.getPatterns(txn, key) shouldBe List(patterns)
        store.getData(txn, key) shouldBe Nil
        store.getWaitingContinuation(txn, key) should not be empty
      }

      r shouldBe None
      store.isEmpty shouldBe false
      store.getTrieUpdates.length shouldBe 1
      store.getTrieUpdateCount shouldBe 1

      val checkpoint0 = space.createCheckpoint()
      checkpoint0.log should not be empty

      space.clear()
      store.isEmpty shouldBe true
      store.getTrieUpdates.length shouldBe 0
      store.getTrieUpdateCount shouldBe 0

      val checkpoint1 = space.createCheckpoint()
      checkpoint1.log shouldBe empty
  }

  def validateIndexedStates(space: ISpace[String, Pattern, String, String, StringsCaptor],
                            indexedStates: Seq[(State, Int)]): Boolean = {
    val tests: Seq[Any] = indexedStates
      .map {
        case (State(checkpoint, expectedContents, expectedJoins), chunkNo) =>
          space.reset(checkpoint)
          val num = "%02d".format(chunkNo)

          val contentsTest = space.store.toMap == expectedContents

          if (contentsTest) {
            logger.debug(s"$num: store had expected contents")
          } else {
            logger.error(s"$num: store had unexpected contents")
          }

          val actualJoins = space.store.joinMap

          val joinsTest =
            expectedJoins.forall {
              case (hash: Blake2b256Hash, expecteds: Seq[Seq[String]]) =>
                val expected = HashMultiset.create[Seq[String]](expecteds.asJava)
                val actual   = HashMultiset.create[Seq[String]](actualJoins(hash).asJava)
                expected.equals(actual)
            }

          if (joinsTest) {
            logger.debug(s"$num: store had expected joins")
          } else {
            logger.error(s"$num: store had unexpected joins")
          }

          contentsTest && joinsTest
      }
    !tests.contains(false)
  }

  "createCheckpoint on an empty store" should "return the expected hash" in withTestSpace { space =>
    space.createCheckpoint().root shouldBe Blake2b256Hash.fromHex(
      "ff3c5e70a028b7956791a6b3d8db9cd11f469e0088db22dd3afbc86997fe86a3")
  }

  "consume then createCheckpoint" should "return the expected hash and the TrieStore should contain the expected value" in
    withTestSpace { space =>
      val channels = List("ch1")
      val gnat = GNAT(
        channels,
        List.empty[Datum[String]],
        List(
          WaitingContinuation.create(channels, List[Pattern](Wildcard), new StringsCaptor, false)))

      val channelsHash: Blake2b256Hash = space.store.hashChannels(gnat.channels)

      val leafPointer = LeafPointer(Trie.hash[Blake2b256Hash, TestGNAT](Leaf(channelsHash, gnat)))
      val skip        = Skip(channelsHash.bytes.drop(1), leafPointer)
      val skipHash    = Trie.hash(skip)(codecK, Codec[String])

      val nodeHash = Trie.hash[Blake2b256Hash, TestGNAT](
        Node(
          PointerBlock
            .create()
            .updated(List((JByte.toUnsignedInt(channelsHash.bytes.head), NodePointer(skipHash))))))

      space.consume(gnat.channels,
                    gnat.wks.head.patterns,
                    gnat.wks.head.continuation,
                    gnat.wks.head.persist)

      history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe None

      space.createCheckpoint().root shouldBe nodeHash

      history
        .lookup(space.store.trieStore, space.store.trieBranch, channelsHash)
        .value shouldBe gnat
    }

  "consume twice then createCheckpoint" should "persist the expected values in the TrieStore" in
    withTestSpace { space =>
      val gnat1 = {
        val channels = List("ch1")
        GNAT(channels,
             List.empty[Datum[String]],
             List(
               WaitingContinuation
                 .create(channels, List[Pattern](Wildcard), new StringsCaptor, false)))
      }

      val channelsHash1: Blake2b256Hash = space.store.hashChannels(gnat1.channels)

      space.consume(gnat1.channels,
                    gnat1.wks.head.patterns,
                    gnat1.wks.head.continuation,
                    gnat1.wks.head.persist)

      val gnat2 = {
        val channels = List("ch2")
        GNAT(channels,
             List.empty[Datum[String]],
             List(
               WaitingContinuation
                 .create(channels, List[Pattern](Wildcard), new StringsCaptor, false)))
      }

      val channelsHash2: Blake2b256Hash = space.store.hashChannels(gnat2.channels)

      space.consume(gnat2.channels,
                    gnat2.wks.head.patterns,
                    gnat2.wks.head.continuation,
                    gnat2.wks.head.persist)

      history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash1) shouldBe None

      history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash2) shouldBe None

      val _ = space.createCheckpoint()

      history
        .lookup(space.store.trieStore, space.store.trieBranch, channelsHash1)
        .value shouldBe gnat1

      history
        .lookup(space.store.trieStore, space.store.trieBranch, channelsHash2)
        .value shouldBe gnat2
    }

  "produce a bunch and then createCheckpoint" should "persist the expected values in the TrieStore" in withTestSpace {
    space =>
      forAll { (data: TestProduceMap) =>
        val gnats: Seq[TestGNAT] =
          data.map {
            case (channel, datum) =>
              GNAT(List(channel),
                   List(datum),
                   List.empty[WaitingContinuation[Pattern, StringsCaptor]])
          }.toList

        gnats.foreach {
          case GNAT(List(channel), List(datum), _) =>
            space.produce(channel, datum.a, datum.persist)
        }

        val channelHashes = gnats.map(gnat => space.store.hashChannels(gnat.channels))

        history.lookup(space.store.trieStore, space.store.trieBranch, channelHashes) shouldBe None

        val _ = space.createCheckpoint()

        history
          .lookup(space.store.trieStore, space.store.trieBranch, channelHashes)
          .value should contain theSameElementsAs gnats
      }
  }

  "consume a bunch and then createCheckpoint" should "persist the expected values in the TrieStore" in
    withTestSpace { space =>
      forAll { (data: TestConsumeMap) =>
        val gnats: Seq[TestGNAT] =
          data.map {
            case (channels, wk) =>
              GNAT(channels, List.empty[Datum[String]], List(wk))
          }.toList

        gnats.foreach {
          case GNAT(channels, _, List(wk)) =>
            space.consume(channels, wk.patterns, wk.continuation, wk.persist)
        }

        val channelHashes = gnats.map(gnat => space.store.hashChannels(gnat.channels))

        history.lookup(space.store.trieStore, space.store.trieBranch, channelHashes) shouldBe None

        val _ = space.createCheckpoint()

        history
          .lookup(space.store.trieStore, space.store.trieBranch, channelHashes)
          .value should contain theSameElementsAs gnats
      }
    }

  "consume and produce a match and then createCheckpoint " should "result in an empty TrieStore" in
    withTestSpace { space =>
      val channels     = List("ch1")
      val channelsHash = space.store.hashChannels(channels)

      val r1 = space.consume(channels, List(Wildcard), new StringsCaptor, persist = false)

      r1 shouldBe None

      val r2 = space.produce(channels.head, "datum", persist = false)

      r2 shouldBe defined

      history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe None

      space.createCheckpoint().root shouldBe Blake2b256Hash.fromHex(
        "ff3c5e70a028b7956791a6b3d8db9cd11f469e0088db22dd3afbc86997fe86a3")

      history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe None
    }

  "createCheckpoint, consume, reset" should "result in an empty store" in
    withTestSpace { space =>
      val root0 = space.createCheckpoint().root

      val gnat1 = {
        val channels = List("ch1")
        GNAT(channels,
             List.empty[Datum[String]],
             List(
               WaitingContinuation
                 .create(channels, List[Pattern](Wildcard), new StringsCaptor, false)))
      }

      space.consume(gnat1.channels,
                    gnat1.wks.head.patterns,
                    gnat1.wks.head.continuation,
                    gnat1.wks.head.persist)

      space.store.isEmpty shouldBe false

      space.reset(root0)

      space.store.isEmpty shouldBe true
    }

  "reset to an unknown checkpoint" should "result in an exception" in
    withTestSpace { space =>
      val unknownHash =
        Blake2b256Hash.fromHex("ff3c5e70a028b7956791a6b3d8db00000f469e0088db22dd3afbc86997fe86a0")
      the[Exception] thrownBy {
        space.reset(unknownHash)
      } should have message "Unknown root."
    }

  "createCheckpoint, consume, createCheckpoint, reset to first checkpoint, reset to second checkpoint" should
    "result in a store that contains the consume and appropriate join map" in withTestSpace {
    space =>
      val root0 = space.createCheckpoint().root

      val gnat1 = {
        val channels = List("ch1", "ch2")
        GNAT(channels,
             List.empty[Datum[String]],
             List(
               WaitingContinuation
                 .create(channels, List[Pattern](Wildcard, Wildcard), new StringsCaptor, false)))
      }

      space.consume(gnat1.channels,
                    gnat1.wks.head.patterns,
                    gnat1.wks.head.continuation,
                    gnat1.wks.head.persist)

      val root1 = space.createCheckpoint().root

      val contents1: Map[Seq[String], Row[Pattern, String, StringsCaptor]] = space.store.toMap

      space.store.isEmpty shouldBe false

      space.store.withTxn(space.store.createTxnRead()) { txn =>
        space.store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
        space.store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
      }

      // Rollback to first checkpoint

      space.reset(root0)

      space.store.isEmpty shouldBe true

      space.store.withTxn(space.store.createTxnRead()) { txn =>
        space.store.getJoin(txn, "ch1") shouldBe Nil
        space.store.getJoin(txn, "ch2") shouldBe Nil
      }

      // Rollback to second checkpoint

      space.reset(root1)

      space.store.isEmpty shouldBe false

      space.store.withTxn(space.store.createTxnRead()) { txn =>
        space.store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
        space.store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
      }

      space.store.toMap shouldBe contents1
  }

  "when resetting to a bunch of checkpoints made with produces, the store" should
    "have the expected contents" in {
    val prop = Prop.forAllNoShrink { (data: Seq[TestProduceMap]) =>
      withTestSpace { space =>
        logger.debug(s"Test: ${data.length} stages")

        val states = data.zipWithIndex.map {
          case (produces, chunkNo) =>
            produces.foreach {
              case (channel, datum) =>
                space.produce(channel, datum.a, datum.persist)
            }
            val num  = "%02d".format(chunkNo)
            val size = "%02d".format(produces.size)
            logger.debug(s"$num: checkpointing $size produces")
            (State(space.createCheckpoint().root, space.store.toMap, space.store.joinMap), chunkNo)
        }

        validateIndexedStates(space, states)
      }
    }
    check(prop)
  }

  "when resetting to a bunch of checkpoints made with consumes, the store" should
    "have the expected contents" in {
    val prop = Prop.forAllNoShrink { (data: Seq[TestConsumeMap]) =>
      withTestSpace { space =>
        logger.debug(s"Test: ${data.length} stages")

        val states = data.zipWithIndex.map {
          case (consumes, chunkNo) =>
            consumes.foreach {
              case (channels, wk) =>
                space.consume(channels, wk.patterns, wk.continuation, wk.persist)
            }
            val num  = "%02d".format(chunkNo)
            val size = "%02d".format(consumes.size)
            logger.debug(s"$num: checkpointing $size consumes")
            (State(space.createCheckpoint().root, space.store.toMap, space.store.joinMap), chunkNo)
        }

        validateIndexedStates(space, states)
      }
    }
    check(prop)
  }

  "when resetting to a bunch of checkpoints made with consumes and produces, the store" should
    "have the expected contents" in {
    val prop = Prop.forAllNoShrink { (data: Seq[(TestConsumeMap, TestProduceMap)]) =>
      withTestSpace { space =>
        logger.debug(s"Test: ${data.length} stages")

        val states = data.zipWithIndex.map {
          case ((consumes, produces), chunkNo) =>
            consumes.foreach {
              case (channels, wk) =>
                space.consume(channels, wk.patterns, wk.continuation, wk.persist)
            }
            produces.foreach {
              case (channel, datum) =>
                space.produce(channel, datum.a, datum.persist)
            }
            val num          = "%02d".format(chunkNo)
            val consumesSize = "%02d".format(consumes.size)
            val producesSize = "%02d".format(produces.size)
            logger.debug(s"$num: checkpointing $consumesSize consumes and $producesSize produces")
            (State(space.createCheckpoint().root, space.store.toMap, space.store.joinMap), chunkNo)
        }

        validateIndexedStates(space, states)
      }
    }
    check(prop)
  }

  "produce and consume" should "store channel hashes" in withTestSpace { space =>
    val channels = List("ch1", "ch2")
    val patterns = List[Pattern](Wildcard, Wildcard)
    val k        = new StringsCaptor
    val data     = List("datum1", "datum2")

    space.consume(channels, patterns, k, false)

    space.produce(channels(0), data(0), false)

    space.produce(channels(1), data(1), false)

    val expectedConsume = Consume.create(channels, patterns, k, false)

    expectedConsume.channelsHash shouldBe StableHashProvider.hash(channels)

    val expectedProduce1 = Produce.create(channels(0), data(0), false)

    expectedProduce1.channelsHash shouldBe StableHashProvider.hash(Seq(channels(0)))

    val expectedProduce2 = Produce.create(channels(1), data(1), false)

    expectedProduce2.channelsHash shouldBe StableHashProvider.hash(Seq(channels(1)))

    val commEvent = COMM(expectedConsume, Seq(expectedProduce1, expectedProduce2))

    val Checkpoint(_, log) = space.createCheckpoint()

    log should contain theSameElementsInOrderAs Seq(commEvent,
                                                    expectedProduce2,
                                                    expectedProduce1,
                                                    expectedConsume)

    log match {
      case COMM(chkCommConsume1: Consume,
                (chkCommProduce1: Produce) :: (chkCommProduce2: Produce) :: Nil)
            :: (chkProduce2: Produce) :: (chkProduce1: Produce) :: (chkConsume: Consume) :: Nil =>
        chkCommConsume1.channelsHash shouldBe expectedConsume.channelsHash
        chkCommProduce1.channelsHash shouldBe expectedProduce1.channelsHash
        chkCommProduce2.channelsHash shouldBe expectedProduce2.channelsHash

        chkProduce2.channelsHash shouldBe expectedProduce2.channelsHash
        chkProduce1.channelsHash shouldBe expectedProduce1.channelsHash
        chkConsume.channelsHash shouldBe expectedConsume.channelsHash

      case _ => fail("unexpected trace log")
    }
  }

  "consume, produce, produce" should "result in the expected trace log" in withTestSpace { space =>
    val channels = List("ch1", "ch2")
    val patterns = List[Pattern](Wildcard, Wildcard)
    val k        = new StringsCaptor
    val data     = List("datum1", "datum2")

    space.consume(channels, patterns, k, false)

    space.produce(channels(0), data(0), false)

    space.produce(channels(1), data(1), false)

    val expectedConsume = Consume.create(channels, patterns, k, false)

    val expectedProduce1 = Produce.create(channels(0), data(0), false)

    val expectedProduce2 = Produce.create(channels(1), data(1), false)

    val commEvent = COMM(expectedConsume, Seq(expectedProduce1, expectedProduce2))

    val Checkpoint(_, log) = space.createCheckpoint()

    log should contain theSameElementsInOrderAs Seq(commEvent,
                                                    expectedProduce2,
                                                    expectedProduce1,
                                                    expectedConsume)
  }

  "an install" should "not allow installing after a produce operation" in withTestSpace { space =>
    val channel  = "ch1"
    val datum    = "datum1"
    val key      = List(channel)
    val patterns = List(Wildcard)

    space.produce(channel, datum, persist = false)
    val ex = the[RuntimeException] thrownBy {
      space.install(key, patterns, new StringsCaptor)
    }
    ex.getMessage shouldBe "Installing can be done only on startup"
  }
}

class InMemoryStoreStorageActionsTests
    extends InMemoryStoreTestsBase
    with StorageActionsTests
    with JoinOperationsTests

class LMDBStoreActionsTests
    extends LMDBStoreTestsBase
    with StorageActionsTests
    with JoinOperationsTests
    with BeforeAndAfterAll {}
