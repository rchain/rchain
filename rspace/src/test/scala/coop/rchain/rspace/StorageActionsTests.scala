package coop.rchain.rspace

import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.extended._
import coop.rchain.rspace.internal._
import org.scalatest._

trait StorageActionsTests extends StorageTestsBase[String, Pattern, String, StringsCaptor] {

  "produce" should
    "persist a piece of data in the store" in withTestSpace { space =>
    val store   = space.store
    val key     = List("ch1")
    val keyHash = store.hashChannels(key)

    val r = space.produce(key.head, "datum", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe key
      store.getPatterns(txn, key) shouldBe Nil
      store.getData(txn, key) shouldBe List(Datum("datum", persist = false))
      store.getWaitingContinuation(txn, key) shouldBe Nil
    }

    r shouldBe None
    //store is not empty - we have 'A' stored
    store.isEmpty shouldBe false

    store.eventsCounter.getProducesCount shouldBe 1
    store.eventsCounter.getConsumesCount shouldBe 0
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
      store.getData(txn, key) shouldBe List(Datum("datum1", persist = false))
      store.getWaitingContinuation(txn, key) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = space.produce(key.head, "datum2", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, keyHash) shouldBe key
      store.getPatterns(txn, key) shouldBe Nil
      store.getData(txn, key) should contain theSameElementsAs List(
        Datum("datum1", persist = false),
        Datum("datum2", persist = false))
      store.getWaitingContinuation(txn, key) shouldBe Nil
    }

    r2 shouldBe None
    //store is not empty - we have 2 As stored
    store.isEmpty shouldBe false

    store.eventsCounter.getProducesCount shouldBe 2
    store.eventsCounter.getConsumesCount shouldBe 0
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

    store.eventsCounter.getProducesCount shouldBe 0
    store.eventsCounter.getConsumesCount shouldBe 1
  }

  "consuming with a list of patterns that is a different length than the list of channels" should
    "throw" in withTestSpace { space =>
    an[IllegalArgumentException] shouldBe thrownBy(
      space.consume(List("ch1", "ch2"), List(Wildcard), new StringsCaptor, persist = false))

    val store = space.store

    store.isEmpty shouldBe true

    store.eventsCounter.getProducesCount shouldBe 0
    store.eventsCounter.getConsumesCount shouldBe 1
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

    store.eventsCounter.getProducesCount shouldBe 0
    store.eventsCounter.getConsumesCount shouldBe 1
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
      store.getData(txn, key) shouldBe List(Datum("datum", persist = false))
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

    store.eventsCounter.getProducesCount shouldBe 1
    store.eventsCounter.getConsumesCount shouldBe 1
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

    store.eventsCounter.getProducesCount shouldBe 3
    store.eventsCounter.getConsumesCount shouldBe 3
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
      store.getData(txn, produceKey1) shouldBe List(Datum("datum1", persist = false))
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
      store.getData(txn, produceKey1) shouldBe List(Datum("datum1", persist = false))
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

    store.eventsCounter.getProducesCount shouldBe 2
    store.eventsCounter.getConsumesCount shouldBe 1
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
      store.getData(txn, produceKey1) shouldBe List(Datum("datum1", persist = false))
      store.getWaitingContinuation(txn, produceKey1) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = space.produce(produceKey2.head, "datum2", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, produceKey2Hash) shouldBe produceKey2
      store.getPatterns(txn, produceKey2) shouldBe Nil
      store.getData(txn, produceKey2) shouldBe List(Datum("datum2", persist = false))
      store.getWaitingContinuation(txn, produceKey2) shouldBe Nil
    }

    r2 shouldBe None

    val r3 = space.produce(produceKey3.head, "datum3", persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getChannels(txn, produceKey3Hash) shouldBe produceKey3
      store.getPatterns(txn, produceKey3) shouldBe Nil
      store.getData(txn, produceKey3) shouldBe List(Datum("datum3", persist = false))
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

    store.eventsCounter.getProducesCount shouldBe 3
    store.eventsCounter.getConsumesCount shouldBe 1
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

    store.eventsCounter.getProducesCount shouldBe 3
    store.eventsCounter.getConsumesCount shouldBe 3
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

      store.eventsCounter.getProducesCount shouldBe 3
      store.eventsCounter.getConsumesCount shouldBe 3
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

    store.eventsCounter.getProducesCount shouldBe 3
    store.eventsCounter.getConsumesCount shouldBe 3
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

    store.eventsCounter.getProducesCount shouldBe 2
    store.eventsCounter.getConsumesCount shouldBe 1
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

    store.eventsCounter.getProducesCount shouldBe 2
    store.eventsCounter.getConsumesCount shouldBe 1
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

    store.eventsCounter.getProducesCount shouldBe 4
    store.eventsCounter.getConsumesCount shouldBe 2
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
      store.getData(txn, List("ch1")) shouldBe List(Datum("datum1", persist = false))
    }

    store.withTxn(store.createTxnRead()) { txn =>
      store.getWaitingContinuation(txn, List("ch1", "ch2")) should not be empty
      store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
      store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
    }

    store.isEmpty shouldBe false

    store.eventsCounter.getProducesCount shouldBe 1
    store.eventsCounter.getConsumesCount shouldBe 1
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

    store.eventsCounter.getProducesCount shouldBe 2
    store.eventsCounter.getConsumesCount shouldBe 2
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
        store.getData(txn, List("ch2")) shouldBe List(Datum("datum2", persist = false))
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

      store.eventsCounter.getProducesCount shouldBe 2
      store.eventsCounter.getConsumesCount shouldBe 2
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
      store.getData(txn, key) shouldBe List(Datum("datum", persist = false))
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

    store.eventsCounter.getProducesCount shouldBe 1
    store.eventsCounter.getConsumesCount shouldBe 2
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
        store.getData(txn, key) shouldBe List(Datum("datum1", persist = false))
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

      store.eventsCounter.getProducesCount shouldBe 2
      store.eventsCounter.getConsumesCount shouldBe 2
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

      store.eventsCounter.getProducesCount shouldBe 2
      store.eventsCounter.getConsumesCount shouldBe 1
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
      store.getData(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }

    store.eventsCounter.getProducesCount shouldBe 2
    store.eventsCounter.getConsumesCount shouldBe 1
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
        store.getData(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      }

      r3 shouldBe None

      val r4 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      }

      r4 shouldBe defined

      runK(r4)

      getK(r4).results should contain theSameElementsAs List(List("datum1"))

      store.eventsCounter.getProducesCount shouldBe 2
      store.eventsCounter.getConsumesCount shouldBe 2
  }

  "doing a persistent produce and consuming twice" should "work" in withTestSpace { space =>
    val store = space.store

    val r1 = space.produce("ch1", "datum1", persist = true)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }

    r1 shouldBe None

    val r2 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }

    r2 shouldBe defined

    runK(r2)

    getK(r2).results should contain theSameElementsAs List(List("datum1"))

    val r3 = space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

    store.withTxn(store.createTxnRead()) { txn =>
      store.getData(txn, List("ch1")) shouldBe List(Datum("datum1", persist = true))
      store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
    }

    r3 shouldBe defined

    runK(r3)

    getK(r3).results should contain theSameElementsAs List(List("datum1"))

    store.eventsCounter.getProducesCount shouldBe 1
    store.eventsCounter.getConsumesCount shouldBe 2
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
        Datum("datum1", persist = false),
        Datum("datum2", persist = false),
        Datum("datum3", persist = false)
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
        Datum("datum1", persist = false),
        Datum("datum2", persist = false),
        Datum("datum3", persist = false)
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

    store.eventsCounter.getProducesCount shouldBe 3
    store.eventsCounter.getConsumesCount shouldBe 4
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
