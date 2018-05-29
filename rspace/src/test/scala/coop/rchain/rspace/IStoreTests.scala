package coop.rchain.rspace

import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.internal._
import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait IStoreTests
    extends StorageTestsBase[String, Pattern, String, StringsCaptor]
    with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(10))

  "putDatum" should "put datum in a new channel" in withTestStore { store =>
    forAll("channel", "datum") { (channel: String, datumValue: String) =>
      val key   = List(channel)
      val datum = Datum(datumValue, persist = false)

      store.withTxn(store.createTxnWrite()) { txn =>
        store.putDatum(txn, key, datum)
        store.getData(txn, key) should contain theSameElementsAs (Seq(datum))
      }
      store.clear()
    }
  }

  it should "append datum if channel already exists" in withTestStore { store =>
    forAll("channel", "datum") { (channel: String, datumValue: String) =>
      val key    = List(channel)
      val datum1 = Datum(datumValue, persist = false)
      val datum2 = Datum(datumValue + "2", persist = false)

      store.withTxn(store.createTxnWrite()) { txn =>
        store.putDatum(txn, key, datum1)
        store.putDatum(txn, key, datum2)
        store.getData(txn, key) should contain theSameElementsAs (Seq(datum1, datum2))
      }
      store.clear()
    }
  }

  private[this] val validIndices =
    for (n <- Gen.choose(1, 10)) yield n

  "removeDatum" should s"remove datum at index" in withTestStore { store =>
    val size = 11
    forAll("channel", "datum", validIndices, minSuccessful(10)) {
      (channel: String, datumValue: String, index: Int) =>
        val key = List(channel)
        val data = List.tabulate(size) { i =>
          Datum(datumValue + i, persist = false)
        }

        store.withTxn(store.createTxnWrite()) { txn =>
          data.foreach { d =>
            store.putDatum(txn, key, d)
          }
          store.removeDatum(txn, key(0), index - 1)
          store.getData(txn, key) should contain theSameElementsAs (data.filterNot(
            _.a == datumValue + (size - index)))
        }
        store.clear()
    }
  }

  "collectGarbage" should "not remove used channels" in withTestStore { store =>
    forAll("channel", "datum") { (channel: String, datum: String) =>
      val key  = List(channel)
      val hash = store.hashChannels(key)

      store.withTxn(store.createTxnWrite()) { txn =>
        store.putDatum(txn, key, Datum(datum, persist = false))

        store.collectGarbage(txn, store.hashChannels(key))
        store.getChannels(txn, hash) should contain theSameElementsAs (key)
      }
      store.clear()
    }
  }

  it should "remove obsolete channels" in withTestStore { store =>
    forAll("channel", "datum") { (channel: String, datum: String) =>
      val key  = List(channel)
      val hash = store.hashChannels(key)
      store.withTxn(store.createTxnWrite()) { txn =>
        store.putDatum(txn, key, Datum(datum, persist = false))
        // collectGarbage is called in removeDatum:
        store.removeDatum(txn, key, 0)
        store.getChannels(txn, hash) shouldBe empty
      }
      store.clear()
    }
  }

  "putWaitingContinuation" should "put waiting continuation in a new channel" in withTestStore {
    store =>
      forAll("channel", "continuation") { (channel: String, pattern: String) =>
        val key          = List(channel)
        val patterns     = List(StringMatch(pattern))
        val continuation = new StringsCaptor
        val wc: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation(patterns, continuation, false)

        store.withTxn(store.createTxnWrite()) { txn =>
          store.putWaitingContinuation(txn, key, wc)
          store.getWaitingContinuation(txn, key) shouldBe List(wc)
        }
        store.clear()
      }
  }

  it should "append continuation if channel already exists" in withTestStore { store =>
    forAll("channel", "continuation") { (channel: String, pattern: String) =>
      val key          = List(channel)
      val patterns     = List(StringMatch(pattern))
      val continuation = new StringsCaptor
      val wc1: WaitingContinuation[Pattern, StringsCaptor] =
        WaitingContinuation(patterns, continuation, false)

      val wc2: WaitingContinuation[Pattern, StringsCaptor] =
        WaitingContinuation(List(StringMatch(pattern + 2)), continuation, false)

      store.withTxn(store.createTxnWrite()) { txn =>
        store.putWaitingContinuation(txn, key, wc1)
        store.putWaitingContinuation(txn, key, wc2)
        store.getWaitingContinuation(txn, key) should contain theSameElementsAs List(wc1, wc2)
      }
      store.clear()
    }
  }

  "removeWaitingContinuation" should "remove waiting continuation from index" in withTestStore {
    store =>
      forAll("channel", "continuation") { (channel: String, pattern: String) =>
        val key          = List(channel)
        val patterns     = List(StringMatch(pattern))
        val continuation = new StringsCaptor
        val wc1: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation(patterns, continuation, false)
        val wc2: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation(List(StringMatch(pattern + 2)), continuation, false)

        store.withTxn(store.createTxnWrite()) { txn =>
          store.putWaitingContinuation(txn, key, wc1)
          store.putWaitingContinuation(txn, key, wc2)
          store.removeWaitingContinuation(txn, key, 0)
          store.getWaitingContinuation(txn, key) shouldBe List(wc1)
        }
        store.clear()
      }
  }

  "addJoin" should "add join for a channel" in withTestStore { store =>
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      store.withTxn(store.createTxnWrite()) { txn =>
        store.addJoin(txn, channel, channels)
        store.getJoin(txn, channel) shouldBe List(channels)
      }
      store.clear()
    }
  }

  "removeJoin" should "remove join for a channel" in withTestStore { store =>
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      store.withTxn(store.createTxnWrite()) { txn =>
        store.addJoin(txn, channel, channels)
        store.removeJoin(txn, channel, channels)
        store.getJoin(txn, channel) shouldBe empty
      }
      store.clear()
    }
  }

  it should "remove only passed in joins for a channel" in withTestStore { store =>
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      store.withTxn(store.createTxnWrite()) { txn =>
        store.addJoin(txn, channel, channels)
        store.addJoin(txn, channel, List("otherChannel"))
        store.removeJoin(txn, channel, channels)
        store.getJoin(txn, channel) shouldBe List(List("otherChannel"))
      }
      store.clear()
    }
  }

  "removeAllJoins" should "remove all joins for a channel" in withTestStore { store =>
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      store.withTxn(store.createTxnWrite()) { txn =>
        store.addJoin(txn, channel, channels)
        store.addJoin(txn, channel, List("otherChannel"))
        store.removeAllJoins(txn, channel)
        store.getJoin(txn, channel) shouldBe List()
      }
      store.clear()
    }
  }
}

class InMemoryStoreTests extends InMemoryStoreTestsBase with IStoreTests
class LMDBStoreTests     extends LMDBStoreTestsBase with IStoreTests
