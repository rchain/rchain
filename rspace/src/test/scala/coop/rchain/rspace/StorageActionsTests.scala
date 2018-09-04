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
import scala.collection.immutable.{Seq, Set}
import coop.rchain.rspace.test.ArbitraryInstances._

import scala.util.Random

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

  "clear" should "reset to the same hash on multiple runs" in withTestSpace { space =>
    val store           = space.store
    val key             = List("ch1")
    val patterns        = List(Wildcard)
    val emptyCheckpoint = space.createCheckpoint()

    //put some data so the checkpoint is != empty
    space.consume(key, patterns, new StringsCaptor, persist = false)
    val checkpoint0 = space.createCheckpoint()
    checkpoint0.log should not be empty

    space.createCheckpoint()
    space.clear()

    //force clearing of trie store state
    store.createCheckpoint()
    space.clear()

    //the checkpointing mechanism should not interfere with the empty root
    val checkpoint2 = space.createCheckpoint()
    checkpoint2.log shouldBe empty
    checkpoint2.root shouldBe emptyCheckpoint.root
  }

  def validateIndexedStates(space: ISpace[String, Pattern, String, String, StringsCaptor],
                            indexedStates: Seq[(State, Int)],
                            reportName: String,
                            differenceReport: Boolean = false): Boolean = {
    final case class SetRow[P, A, K](data: Set[Datum[A]], wks: Set[WaitingContinuation[P, K]])

    def convertMap(m: Map[Seq[String], Row[Pattern, String, StringsCaptor]])
      : Map[Seq[String], SetRow[Pattern, String, StringsCaptor]] =
      m.map { case (channels, row) => channels -> SetRow(row.data.toSet, row.wks.toSet) }

    val tests: Seq[Any] = indexedStates
      .map {
        case (State(checkpoint, rawExpectedContents, expectedJoins), chunkNo) =>
          space.reset(checkpoint)
          val num = "%02d".format(chunkNo)

          val expectedContents = convertMap(rawExpectedContents)
          val actualContents   = convertMap(space.store.toMap)

          val contentsTest = expectedContents == actualContents

          val actualJoins = space.store.joinMap

          val joinsTest =
            expectedJoins.forall {
              case (hash: Blake2b256Hash, expecteds: Seq[Seq[String]]) =>
                val expected = HashMultiset.create[Seq[String]](expecteds.asJava)
                val actual   = HashMultiset.create[Seq[String]](actualJoins(hash).asJava)
                expected.equals(actual)
            }

          val result = contentsTest && joinsTest
          if (!result) {
            if (!contentsTest) {
              logger.error(s"$num: store had unexpected contents ($reportName)")
            }

            if (!joinsTest) {
              logger.error(s"$num: store had unexpected joins ($reportName)")
            }

            if (differenceReport) {
              logger.error(s"difference report ($reportName)")
              for ((expectedChannels, expectedRow) <- expectedContents) {
                val actualRow = actualContents.get(expectedChannels)

                actualRow match {
                  case Some(row) =>
                    if (row != expectedRow) {
                      logger.error(
                        s"key [$expectedChannels] invalid actual value: $row !== $expectedRow")
                    }
                  case None => logger.error(s"key [$expectedChannels] not found in actual records")
                }
              }

              for ((actualChannels, actualRow) <- actualContents) {
                val expectedRow = expectedContents.get(actualChannels)

                expectedRow match {
                  case Some(row) =>
                    if (row != actualRow) {
                      logger.error(
                        s"key[$actualChannels] invalid actual value: $actualRow !== $row")
                    }
                  case None => logger.error(s"key [$actualChannels] not found in expected records")
                }
              }
            }
          }
          result
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

  "produce a bunch and then createCheckpoint" should "persist the expected values in the TrieStore" in
    forAll { (data: TestProduceMap) =>
      if (data.nonEmpty) {
        withTestSpace { space =>
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
    }

  "consume a bunch and then createCheckpoint" should "persist the expected values in the TrieStore" in
    forAll { (data: TestConsumeMap) =>
      val gnats: Seq[TestGNAT] =
        data
          .filter(_._1.nonEmpty) //channels == Seq.empty will faill in consume
          .map {
            case (channels, wk) =>
              GNAT(channels, List.empty[Datum[String]], List(wk))
          }
          .toList

      withTestSpace { space =>
        if (gnats.nonEmpty) {
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

  def getData(): (Seq[TestProduceMap], Seq[TestProduceMap]) = {
    val vec = Vector(
      Map("襥Ⱕ녌莊阧喨䕫⮠䌱" -> ("衐촑쥌胎䴮픚", false), "돥냽㜳昗" -> ("㏡瀄ヿ넙肳諙骫⾹빏ᷲ", false)),
      Map(
        ""           -> ("䚞㾤⫞ֱ霷", false),
        "ㆯ֍鱥ꏊ䷆겝♋␰죐创" -> ("", false),
        "뮣㸰ﬔ眴泪䄙ḁ"     -> ("絩", false),
        "꺄巭蕝㰹䓻すฃ顲抢"   -> ("遝", false),
        "ᑄ텍䘰ꓹ㙵Ӎ﷈娲읇"   -> ("칵䣀䲌", true),
        "倈๺꼸庅᷹ᩂ"     -> ("ꄘ㒨퉴턖銰먗๣ꃕ", true),
        "૲堙넓䭘恾嵬"    -> ("""틁钔獉멢뤌껺겯）✸""", true),
        ""           -> ("""┩꜇毀筶媜嘽""", true),
        "⭽Ʈ퀧븚ퟬῡ㨓啘༔"   -> ("랊槗绔", true),
        "핛䦽㼁๦���"    -> ("阴⚦⋠ㅷ", false)
      ),
      Map("욹湡铹洳直ꧩ茜㉴⛬퐣ℑ" -> ("箜", true),
          "搠믕廫词꼬ꇯ"     -> ("⋇塶Ȗ痢꒙梃㭫갍域쁗", true),
          "㈺ᮀ姨᧾逯諊"       -> ("", false)),
      Map(
        "䎴᝸뙛뚉鈫蟾"     -> ("㌔̋", true),
        "赋렾"           -> ("৺㏄淝豗兓귇•", false),
        "隞갴䛂豶훂즉퐇"      -> ("决ˠ宆볷ᔧ萄㘚㻋럨ゥ⢣", true),
        "៩喭ꢆኍ鳰䋬씉ᶔ擇㠊졌쁅" -> ("ൾ", true),
        "砼龦䓖劐"         -> ("솥", false),
        "㤢竞迿拂넄"       -> ("绷꼮媴㯐", true),
        "敾碰첐䕚唐䵓"    -> ("廤ᡢḶ", false),
        "㻀"            -> ("䛤䒞乃韋ꡫ", true),
        "枯輠豐ḣⷦ賳賀㸺"    -> ("㕇醎찤ꔃ", true),
        "삤汔"          -> ("삊㍿욿顕⤏緿ꃣ", true),
        "脑煆븜鷘쮒"       -> ("Ᏺ䪱為瓮Ɛ㪫砰ٓⓂ", false)
      ),
      Map(
        "⨥瞃싵ѩ亥鈩⁶ො슬暿" -> ("ꒊ酙痘䳲", true),
        "妣劚矰ﵑ벀ᗣ宅鋺"   -> ("役䣠ⳍ", false),
        "⣵䪐똎犈"        -> ("㤝︰Ʊ", true),
        "헝"           -> ("귾ｆ笄", true),
        "쨴㽺"          -> ("☉簴㐷ᐋ襴搰䲏", false),
        "듞"           -> ("᏶䰘쩋嘏藵Ṑ㥙ꐚꢌ朣", true),
        "㵻㜙"          -> ("㈟朅", false),
        "읽ꉤ濯フ懲ᱵ"     -> ("䈶", false),
        "憗רּ쒠"        -> ("૽餂紝ꅻ", false),
        "ᛑ쐔醠ӱ觿ꃹ犭䳵ꨀ"   -> ("⪢", true),
        "褫颠橰厞"        -> ("⯊↵", false),
        "㥴穄綻橽ࡏ춣"     -> ("㳽ꘙ͢", true)
      ),
      Map(
        "껠䀃陽哧⤚⻽鄺슑ᛖ" -> ("艥ొⱘ볷ૼﳃɤ鍌画", false),
        "鏃멁醫懠އ龅ꑻ⨮⹅"  -> ("澿㸆乾춨닸깔핌獀팈춙", false),
        "ꁴ遣樂確"      -> ("れ罜鎝凜埼烶⍎襫", true),
        "笋剛銗ﺘⵏ헯繊Ӻට" -> ("풫梾Ｕ懢᩷쳕", true),
        "긯"          -> ("䤕뉼ꝲ봽", true),
        "苛⅒꥖跢"    -> ("㎳缦", false),
        "璔棝㛧뾾︘䇲囌"    -> ("ࡋ饡섊橿뚙㛼搩禟틾ℷ", false),
        "핣⋄趬窼䬫銐"     -> ("护緒尟⿼풰", false)
      ),
      Map(
        "趻ᰄᮆ騑묇"       -> ("ヽ뇟䆄䖻䫉骋펴ㇹ徤", false),
        "☳ℿ㱴或㊈鬣ꅔ薓ᓶ" -> ("鐏렪֙馳駖ሂꂚ", false),
        "翎Ო歬𥉉絩툜"      -> ("춗芎枸폳똢摡୨긙渧๣뮏", true),
        "鎮ৈ鴑酘���靾"    -> ("䵷앤᳠", false),
        "鍚늅섳퇑羖烩⋚≋旒"   -> ("墘", false),
        "寋畀흌풠浗恈㛪ﴍ健"   -> ("럿퉧㰾ힲ낈橱", false),
        "ﲠ濅ᙃ襊冦洱鑙"     -> ("㎹섢隵ᖛ夷᯶蕴", false),
        "梔驸㫀뼓䎧୶℔˳皑"  -> ("㘐툠夸", false),
        "꼉﹞儴鑝♊遯譣ઔϳ"  -> ("ﰖ", false),
        "煜랞"         -> ("朣꒔뾑묲쨃", true),
        "痤㙳㶙늽襩⳪"      -> ("륉땔▓", true),
        "顄鍐뎚䩻坭觹"      -> ("꫘㝾덃", false)
      ),
      Map(
        "렟혹"           -> ("ᯁ⣰㨥㣬⡘捰ᕻ", true),
        "㒮ߨ"           -> ("ጇ뺽鯅֜꾞椩㗆넖", true),
        "鹑酳謽ᜪ儿됬"       -> ("漡츌둚Ⱈⴊ饩琧෦뻀ケ", true),
        "攷㨑ଅꧪ႖氚푑鸺"    -> ("肆ᵧ䋗黒잏苑뗮", false),
        "듞"            -> ("笌ꎟ夰", true),
        "⟩睌㎶侮뒛﷯늗"     -> ("ఘ", false),
        "㊛鸸"           -> ("Ū晀晪銑踘ᓝ", false),
        "䂟럠鯱쩏ኻ빟㖀"     -> ("벻谵숫㸷䔀睰㠔䆂瓿惴㴇", true),
        "⋑橺䄘簇䇼絧៊ꆕ儵洩" -> ("荼쓮椺몍伈谥", false),
        "嚼鳢⮸ꠧ"        -> ("෴粷뿉酩↉", true),
        "Ⴡ⋑ض㳍嬞쁇탙十"   -> ("嫱锥ᇠꞇꨗ籰닐邆", false)
      ),
      Map(
        "踞슑᮵披뾴"     -> ("썯", false),
        "ﵳ僦공ヿ镥"      -> ("蔛Ὰ竜ฃ▄ᑚ䕼䐠", true),
        " 놳侐ꢍﱹ픩ꆩ"    -> ("價忣뿲ꬤꤩꐣ塞東ᶳ곱輪㜊", true),
        "㈃뉰บ꿶쩜鸻㿏"    -> ("짝袆᧟➋絫麤퓙⎾嶧韅", true),
        "ٲ螘낮샎禩뾻뭔"    -> ("", false),
        "㵱툫"         -> ("ꯔ洁", false),
        "힑ᔭ㦣焽涼誅玭났ᆥ"  -> ("门霓೧麱寃䳿櫝᷁枳ﻏ", false),
        "㲊뇬抇ௗ"       -> ("০塎튵턻뮴暯᩼", false),
        "杏⤈胅奖ꭦ뜒꣆铱Ԗ" -> ("嵏祋ﲏ", true),
        "뜊㫸♱믑퇑杧"     -> ("蘐奵鑭洉졚", true),
        "ꃕ뙣尗ﳔ揪"     -> ("民흁ｯ챣藹", true)
      ),
      Map(
        "౛봍屐읽휩䋜⥎釐ᕑ谬읗꒕" -> ("毜꿻ꈼ먆뤌ᒫ酔Ỏ", false),
        "冘炛풝갑젯⸼ᯎ䩢뚌拢킑" -> ("ꦏ㬄渴輋✟호뙪卒", false),
        "瘊Ꟗ齥"         -> ("ꐓ㍮矝", true),
        "㍭⢈㉯翙ᤉὨ絳쳸荲"  -> ("薼節", true),
        "麯햱蒿⻶後㻴ꮷ쒺ዝ劫﹉⣌" -> ("襙", true),
        "맭൝킂"          -> ("ᶅധ瘾୿ᲁ", true),
        "让劭ᦕ뫤乭ᇴ簆䚓"   -> ("䍺苮똿઴頟⢉蕬䔳궩北ᴅ", true),
        "칿萐鳌"          -> ("旌຿㘳Ḁ嵽콁鼫涬", false),
        "楜濗䜹ꮆ镠㼉"     -> ("溟᧱ੇꩲ柯烈嬝넗ᆒᴩ", false)
      ),
      Map("⹸㬤梊"     -> ("꫻ꏺ", false),
          "껃ퟒ硥뵕煍⦳똊"  -> ("푾㉠쥛＜墝긆ܓ➖졕踾", true),
          "먈肂㎏᳗疪㯦⣻" -> ("譥즰⃥軕丹彜㒒", false),
          "२墴㕔"      -> ("컆ꈷ쎒簹涛琱利앍컽㞕˟", false)),
      Map(
        "羦㋏쉪帊믶⯔쑜粎﹢"   -> ("謓軞惶ᨫ", false),
        "꿷멑輿銡⬀"      -> ("ﵔ卶", true),
        "喰鷂簍姈潄Ι諩멗ꧧ썺" -> ("♵ⷚ妣겧홥馳ᜭ큻䛯", false),
        "물䀓ⷈʨ㌆℣뎙"   -> ("", true),
        "ी地䬘♞ꭅ셓穄学䘃"   -> ("ᝪ겓⸭ߦꟓ됄", true),
        "瞲挩"        -> ("쎢ץ煵栧", false)
      )
    )

    def fixStr(s: String): String = s.map(_ => (0x31 + Random.nextInt(0x7e - 0x31)).toChar)

    val resRand = vec.map(_.map {
      case (channel, datumArgs) =>
        channel -> Datum.create(channel, datumArgs._1, datumArgs._2)
    })

    val resAscii = vec.map(_.map {
      case (channel, datumArgs) => {
        val asciiChannel = fixStr(channel)
        val asciiData    = fixStr(datumArgs._1)
        asciiChannel -> Datum.create(asciiChannel, asciiData, datumArgs._2)
      }
    })
    (resRand, resAscii)
  }

  "BUG(ascii_chars) when resetting to a bunch of checkpoints made with produces, the store" should
    "have the expected contents" in {

    val data = getData()._2
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

      validateIndexedStates(space, states, "ascii_chars", true)
    }
  }

  "BUG(random_chars) when resetting to a bunch of checkpoints made with produces, the store" should
    "have the expected contents" in {
    val data = getData()._1

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

      validateIndexedStates(space, states, "random_chars", true)
    }
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

        validateIndexedStates(space, states, "gen_random_1")
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

        validateIndexedStates(space, states, "gen_random_2")
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

        validateIndexedStates(space, states, "gen_random_3")
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

  "after close space" should "throw RSpaceClosedException on all store operations" in withTestSpace {
    val channel  = "ch1"
    val key      = List(channel)
    val patterns = List(Wildcard)

    space =>
      space.close()
      //using some nulls here to ensure that exception is thrown even before args check
      an[RSpaceClosedException] shouldBe thrownBy(
        space.install(key, patterns, null)
      )

      an[RSpaceClosedException] shouldBe thrownBy(
        space.consume(key, patterns, null, false)
      )

      an[RSpaceClosedException] shouldBe thrownBy(
        space.produce(channel, null, false)
      )
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
    with BeforeAndAfterAll

class MixedStoreActionsTests
    extends MixedStoreTestsBase
    with StorageActionsTests
    with JoinOperationsTests
    with BeforeAndAfterAll
