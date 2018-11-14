package coop.rchain.rspace

import cats._
import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test.ArbitraryInstances._
import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.AppendedClues
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait IStoreTests
    extends StorageTestsBase[Id, String, Pattern, Nothing, String, StringsCaptor]
    with GeneratorDrivenPropertyChecks
    with AppendedClues {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(10))

  "putDatum" should "put datum in a new channel" in forAll("channel", "datum") {
    (channel: String, datumValue: String) =>
      withTestSpace { space =>
        val store: IStore[String, Pattern, String, StringsCaptor] = space.store
        val key                                                   = List(channel)
        val datum                                                 = Datum.create(channel, datumValue, false)

        store
          .withTxn(store.createTxnWrite()) { txn =>
            store.putDatum(txn, key, datum)
            store.getData(txn, key) should contain theSameElementsAs (Seq(datum))
            store.clear(txn)
          }
      }
  }

  it should "append datum if channel already exists" in forAll("channel", "datum") {
    (channel: String, datumValue: String) =>
      withTestSpace { space =>
        val store  = space.store
        val key    = List(channel)
        val datum1 = Datum.create(channel, datumValue, false)
        val datum2 = Datum.create(channel, datumValue + "2", false)

        store.withTxn(store.createTxnWrite()) { txn =>
          store.putDatum(txn, key, datum1)
          store.putDatum(txn, key, datum2)
          store.getData(txn, key) should contain theSameElementsAs (Seq(datum1, datum2))
          store.clear(txn)
        }
      }
  }

  "hashChannels" should "return same hashes for unordered channels" in withTestSpace { space =>
    val store  = space.store
    val hash12 = store.hashChannels(List("ch1", "ch2"))
    val hash21 = store.hashChannels(List("ch2", "ch1"))
    hash12 shouldBe hash21
  }

  private[this] val validIndices =
    for (n <- Gen.choose(1, 10)) yield n
  private[this] val size: Int = 11

  "removeDatum" should s"remove datum at index" in
    forAll("channel", "datum", validIndices, minSuccessful(10)) {
      (channel: String, datumValue: String, index: Int) =>
        withTestSpace { space =>
          val store = space.store
          val key   = List(channel)
          val data = List.tabulate(size) { i =>
            Datum.create(channel, datumValue + i, false)
          }

          store.withTxn(store.createTxnWrite()) { txn =>
            data.foreach { d =>
              store.putDatum(txn, key, d)
            }
            store.removeDatum(txn, key, index - 1)
            store.getData(txn, key) should contain theSameElementsAs (data.filterNot(
              _.a == datumValue + (size - index)
            ))
            store.clear(txn)
          }
        }
    }

  it should "remove obsolete channels" in
    forAll("channel", "datum") { (channel: String, datum: String) =>
      withTestSpace { space =>
        val store = space.store
        val key   = List(channel)
        val hash  = store.hashChannels(key)
        store.withTxn(store.createTxnWrite()) { txn =>
          store.putDatum(txn, key, Datum.create(channel, datum, persist = false))
          // collectGarbage is called in removeDatum:
          store.removeDatum(txn, key, 0)
          store.getChannels(txn, hash) shouldBe empty
          store.clear(txn)
        }
      }
    }

  "putWaitingContinuation" should "put waiting continuation in a new channel" in
    forAll("channel", "continuation") { (channel: String, pattern: String) =>
      withTestSpace { space =>
        val store        = space.store
        val key          = List(channel)
        val patterns     = List(StringMatch(pattern))
        val continuation = new StringsCaptor
        val wc: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(key, patterns, continuation, false)

        store.withTxn(store.createTxnWrite()) { txn =>
          store.putWaitingContinuation(txn, key, wc)
          store.getWaitingContinuation(txn, key) shouldBe List(wc)
          store.clear(txn)
        }
      }
    }

  it should "append continuation if channel already exists" in
    forAll("channel", "continuation") { (channel: String, pattern: String) =>
      withTestSpace { space =>
        val store        = space.store
        val key          = List(channel)
        val patterns     = List(StringMatch(pattern))
        val continuation = new StringsCaptor
        val wc1: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(key, patterns, continuation, false)

        val wc2: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(key, List(StringMatch(pattern + 2)), continuation, false)

        store.withTxn(store.createTxnWrite()) { txn =>
          store.putWaitingContinuation(txn, key, wc1)
          store.putWaitingContinuation(txn, key, wc2)
          store.getWaitingContinuation(txn, key) should contain theSameElementsAs List(wc1, wc2)
          store.clear(txn)
        }
      }
    }

  "removeWaitingContinuation" should "remove waiting continuation from index" in
    forAll("channel", "continuation") { (channel: String, pattern: String) =>
      withTestSpace { space =>
        val store        = space.store
        val key          = List(channel)
        val patterns     = List(StringMatch(pattern))
        val continuation = new StringsCaptor
        val wc1: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(key, patterns, continuation, false)
        val wc2: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(key, List(StringMatch(pattern + 2)), continuation, false)

        store.withTxn(store.createTxnWrite()) { txn =>
          store.putWaitingContinuation(txn, key, wc1)
          store.putWaitingContinuation(txn, key, wc2)
          store.removeWaitingContinuation(txn, key, 0)
          store.getWaitingContinuation(txn, key) shouldBe List(wc1)
          store.clear(txn)
        }
      }
    }

  "addJoin" should "add join for a channel" in
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      withTestSpace { space =>
        val store = space.store
        store.withTxn(store.createTxnWrite()) { txn =>
          store.addJoin(txn, channel, channels)
          store.getJoin(txn, channel) shouldBe List(channels)
          store.clear(txn)
        }
      }
    }

  "removeJoin" should "remove join for a channel" in
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      withTestSpace { space =>
        val store = space.store
        store.withTxn(store.createTxnWrite()) { txn =>
          store.addJoin(txn, channel, channels)
          store.removeJoin(txn, channel, channels)
          store.getJoin(txn, channel) shouldBe empty
          store.clear(txn)
        }
      }
    }

  it should "remove only passed in joins for a channel" in
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      withTestSpace { space =>
        val store = space.store
        store.withTxn(store.createTxnWrite()) { txn =>
          store.addJoin(txn, channel, channels)
          store.addJoin(txn, channel, List("otherChannel"))
          store.removeJoin(txn, channel, channels)
          store.getJoin(txn, channel) shouldBe List(List("otherChannel"))
          store.clear(txn)
        }
      }
    }

  "collapse" should "work on empty history" in withTestSpace { space =>
    space.store.collapse(List.empty) shouldBe List.empty
  }

  it should "return unmodified history when nothing to prune" in
    forAll("gnat") { (gnat: GNAT[String, Pattern, String, StringsCaptor]) =>
      withTestSpace { space =>
        gnat.wks
          .map(_.patterns.size)
          .distinct should contain only gnat.channels.size withClue "#patterns in each continuation should equal #channels"

        val store   = space.store
        val history = List(TrieUpdate(0, Insert, store.hashChannels(gnat.channels), gnat))
        store.collapse(history) shouldBe history
      }
    }

  it should "return unmodified history when nothing to prune in multiple gnats" in
    forAll(distinctListOf[GNAT[String, Pattern, String, StringsCaptor]], SizeRange(3)) {
      withTestSpace { space => (gnats: Seq[GNAT[String, Pattern, String, StringsCaptor]]) =>
        val store = space.store
        val history = gnats
          .flatMap(gnat => List(TrieUpdate(0, Insert, store.hashChannels(gnat.channels), gnat)))
          .toList
        store.collapse(history) should contain theSameElementsAs (history)
      }
    }

  it should "remove all operations from history with the same hash when last operation is delete" in
    forAll("gnat1", "gnat2") {
      withTestSpace {
        space =>
          {
            (
                gnat1: GNAT[String, Pattern, String, StringsCaptor],
                gnat2: GNAT[String, Pattern, String, StringsCaptor]
            ) =>
              val store = space.store
              val gnat1Ops = List(
                TrieUpdate(0, Insert, store.hashChannels(gnat1.channels), gnat1),
                TrieUpdate(1, Delete, store.hashChannels(gnat1.channels), gnat1)
              )
              val gnat2Ops = List(TrieUpdate(2, Insert, store.hashChannels(gnat2.channels), gnat2))
              val history  = gnat1Ops ++ gnat2Ops
              store.collapse(history) shouldBe gnat2Ops
          }
      }
    }

  it should "remove all operations from history with the same hash when last operation is delete - longer case with same hash" in
    forAll("gnat1") { (gnat1: GNAT[String, Pattern, String, StringsCaptor]) =>
      withTestSpace { space =>
        val store = space.store
        val gnatOps = List(
          TrieUpdate(0, Insert, store.hashChannels(gnat1.channels), gnat1),
          TrieUpdate(1, Insert, store.hashChannels(gnat1.channels), gnat1),
          TrieUpdate(2, Insert, store.hashChannels(gnat1.channels), gnat1),
          TrieUpdate(3, Delete, store.hashChannels(gnat1.channels), gnat1)
        )
        store.collapse(gnatOps) shouldBe empty
      }
    }

  it should "remove all but the last operation from history with the same hash when last operation is insert" in
    forAll("gnat") { (gnat: GNAT[String, Pattern, String, StringsCaptor]) =>
      withTestSpace { space =>
        val store      = space.store
        val lastInsert = TrieUpdate(1, Insert, store.hashChannels(gnat.channels), gnat)

        val history =
          List(TrieUpdate(0, Insert, store.hashChannels(gnat.channels), gnat), lastInsert)
        store.collapse(history) shouldBe List(lastInsert)
      }
    }

  it should "remove all but the last operation from history with the same hash when operation with largest count is insert" in
    forAll("gnat") { (gnat: GNAT[String, Pattern, String, StringsCaptor]) =>
      withTestSpace { space =>
        val store      = space.store
        val lastInsert = TrieUpdate(2, Insert, store.hashChannels(gnat.channels), gnat)

        val history = List(
          TrieUpdate(0, Insert, store.hashChannels(gnat.channels), gnat),
          lastInsert,
          TrieUpdate(1, Delete, store.hashChannels(gnat.channels), gnat)
        )
        store.collapse(history) shouldBe List(lastInsert)
      }
    }
}

class InMemoryStoreTests
    extends InMemoryStoreTestsBase[Id]
    with IStoreTests
    with IdTests[String, Pattern, Nothing, String, StringsCaptor]
class LMDBStoreTests
    extends LMDBStoreTestsBase[Id]
    with IStoreTests
    with IdTests[String, Pattern, Nothing, String, StringsCaptor]
class MixedStoreTests
    extends MixedStoreTestsBase[Id]
    with IStoreTests
    with IdTests[String, Pattern, Nothing, String, StringsCaptor]
