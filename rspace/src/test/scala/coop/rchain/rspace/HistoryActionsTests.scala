package coop.rchain.rspace

import java.lang.{Byte => JByte}

import cats.implicits._
import coop.rchain.catscontrib.seq._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringsCaptor, Wildcard}
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal.{Datum, GNAT, Row, WaitingContinuation}
import coop.rchain.rspace.test.ArbitraryInstances._
import org.scalacheck.Prop
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import scodec.Codec

import scala.collection.immutable.Seq

trait HistoryActionsTests
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
      contents: Map[Seq[String], Row[Pattern, String, StringsCaptor]]
  )

  "getCheckpoint on an empty store" should "return the expected hash" in withTestStore { store =>
    getCheckpoint(store) shouldBe Blake2b256Hash.fromHex(
      "c575260cf13e36f179a50b0882bd64fc0466ecd25bdd7bc88766c2cc2e4c0dfe")
  }

  "consume then getCheckpoint" should "return the expected hash and the TrieStore should contain the expected value" in
    withTestStore { store =>
      val gnat = GNAT(List("ch1"),
                      List.empty[Datum[String]],
                      List(WaitingContinuation(List[Pattern](Wildcard), new StringsCaptor, false)))

      val channelsHash: Blake2b256Hash = store.hashChannels(gnat.channels)

      val nodeHash = Trie.hash[Blake2b256Hash, TestGNAT](
        Node(
          PointerBlock
            .create()
            .updated(List((JByte.toUnsignedInt(channelsHash.bytes.head),
                           Some(Trie.hash[Blake2b256Hash, TestGNAT](Leaf(channelsHash, gnat))))))))

      consume(store,
              gnat.channels,
              gnat.wks.head.patterns,
              gnat.wks.head.continuation,
              gnat.wks.head.persist)

      history.lookup(store.trieStore, channelsHash) shouldBe None

      getCheckpoint(store) shouldBe nodeHash

      history.lookup(store.trieStore, channelsHash).value shouldBe gnat
    }

  "consume twice then getCheckpoint" should "persist the expected values in the TrieStore" in
    withTestStore { store =>
      val gnat1 = GNAT(List("ch1"),
                       List.empty[Datum[String]],
                       List(WaitingContinuation(List[Pattern](Wildcard), new StringsCaptor, false)))

      val channelsHash1: Blake2b256Hash = store.hashChannels(gnat1.channels)

      consume(store,
              gnat1.channels,
              gnat1.wks.head.patterns,
              gnat1.wks.head.continuation,
              gnat1.wks.head.persist)

      val gnat2 = GNAT(List("ch2"),
                       List.empty[Datum[String]],
                       List(WaitingContinuation(List[Pattern](Wildcard), new StringsCaptor, false)))

      val channelsHash2: Blake2b256Hash = store.hashChannels(gnat2.channels)

      consume(store,
              gnat2.channels,
              gnat2.wks.head.patterns,
              gnat2.wks.head.continuation,
              gnat2.wks.head.persist)

      history.lookup(store.trieStore, channelsHash1) shouldBe None

      history.lookup(store.trieStore, channelsHash2) shouldBe None

      val _ = getCheckpoint(store)

      history.lookup(store.trieStore, channelsHash1).value shouldBe gnat1

      history.lookup(store.trieStore, channelsHash2).value shouldBe gnat2
    }

  "produce a bunch and then getCheckpoint" should "persist the expected values in the TrieStore" in withTestStore {
    store =>
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
            produce(store, channel, datum.a, datum.persist)
        }

        val channelHashes = gnats.map(gnat => store.hashChannels(gnat.channels))

        history.lookup(store.trieStore, channelHashes) shouldBe None

        val _ = getCheckpoint(store)

        history.lookup(store.trieStore, channelHashes).value should contain theSameElementsAs gnats
      }
  }

  "consume a bunch and then getCheckpoint" should "persist the expected values in the TrieStore" in
    withTestStore { store =>
      forAll { (data: TestConsumeMap) =>
        val gnats: Seq[TestGNAT] =
          data.map {
            case (channels, wk) =>
              GNAT(channels, List.empty[Datum[String]], List(wk))
          }.toList

        gnats.foreach {
          case GNAT(channels, _, List(wk)) =>
            consume(store, channels, wk.patterns, wk.continuation, wk.persist)
        }

        val channelHashes = gnats.map(gnat => store.hashChannels(gnat.channels))

        history.lookup(store.trieStore, channelHashes) shouldBe None

        val _ = getCheckpoint(store)

        history.lookup(store.trieStore, channelHashes).value should contain theSameElementsAs gnats
      }
    }

  "consume and produce a match and then getCheckpoint " should "result in an empty TrieStore" in
    withTestStore { store =>
      val channels     = List("ch1")
      val channelsHash = store.hashChannels(channels)

      val r1 = consume(store, channels, List(Wildcard), new StringsCaptor, persist = false)

      r1 shouldBe None

      val r2 = produce(store, channels.head, "datum", persist = false)

      r2 shouldBe defined

      history.lookup(store.trieStore, channelsHash) shouldBe None

      getCheckpoint(store) shouldBe Blake2b256Hash.fromHex(
        "c575260cf13e36f179a50b0882bd64fc0466ecd25bdd7bc88766c2cc2e4c0dfe")

      history.lookup(store.trieStore, channelsHash) shouldBe None
    }

  "getCheckpoint, consume, reset" should "result in an empty store" in
    withTestStore { store =>
      val checkpoint0 = getCheckpoint(store)

      val gnat1 = GNAT(List("ch1"),
                       List.empty[Datum[String]],
                       List(WaitingContinuation(List(Wildcard), new StringsCaptor, false)))

      // val channelsHash1: Blake2b256Hash = store.hashChannels(gnat1.channels)

      consume(store,
              gnat1.channels,
              gnat1.wks.head.patterns,
              gnat1.wks.head.continuation,
              gnat1.wks.head.persist)

      store.isEmpty shouldBe false

      reset(store, checkpoint0)

      store.isEmpty shouldBe true
    }

  "getCheckpoint, consume, getCheckpoint, reset to first checkpoint, reset to second checkpoint" should
    "result in a store that contains the consume and appropriate join map" in withTestStore {
    store =>
      val checkpoint0 = getCheckpoint(store)

      val gnat1 =
        GNAT(List("ch1", "ch2"),
             List.empty[Datum[String]],
             List(WaitingContinuation(List(Wildcard, Wildcard), new StringsCaptor, false)))

      consume(store,
              gnat1.channels,
              gnat1.wks.head.patterns,
              gnat1.wks.head.continuation,
              gnat1.wks.head.persist)

      val checkpoint1 = getCheckpoint(store)

      val contents1: Map[Seq[String], Row[Pattern, String, StringsCaptor]] = store.toMap

      store.isEmpty shouldBe false

      store.withTxn(store.createTxnRead()) { txn =>
        store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
        store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
      }

      // Rollback to second checkpoint

      reset(store, checkpoint0)

      store.isEmpty shouldBe true

      store.withTxn(store.createTxnRead()) { txn =>
        store.getJoin(txn, "ch1") shouldBe Nil
        store.getJoin(txn, "ch2") shouldBe Nil
      }

      // Rollback to second checkpoint

      reset(store, checkpoint1)

      store.isEmpty shouldBe false

      store.withTxn(store.createTxnRead()) { txn =>
        store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
        store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
      }

      store.toMap shouldBe contents1
  }

  "when resetting to a bunch of checkpoints made with produces, the store" should
    "have the expected contents" in {
    val prop = Prop.forAllNoShrink { (data: Seq[TestProduceMap]) =>
      withTestStore { store =>
        logger.debug(s"Test: ${data.length} stages")

        val states = data.zipWithIndex.map {
          case (produces, chunkNo) =>
            produces.foreach {
              case (channel, datum) =>
                produce(store, channel, datum.a, datum.persist)
            }
            val num  = "%02d".format(chunkNo)
            val size = "%02d".format(produces.size)
            logger.debug(s"$num: checkpointing $size produces")
            (State(getCheckpoint(store), store.toMap), chunkNo)
        }

        val tests = states.map {
          case (State(checkpoint, expectedContents), chunkNo) =>
            reset(store, checkpoint)
            val test = store.toMap == expectedContents
            val num  = "%02d".format(chunkNo)
            if (test) {
              logger.debug(s"$num: store had expected contents")
            } else {
              logger.error(s"$num: store had unexpected contents")
            }
            test
        }

        !tests.contains(false)
      }
    }
    check(prop)
  }

  "when resetting to a bunch of checkpoints made with consumes, the store" should
    "have the expected contents" in {
    val prop = Prop.forAllNoShrink { (data: Seq[TestConsumeMap]) =>
      withTestStore { store =>
        logger.debug(s"Test: ${data.length} stages")

        val states = data.zipWithIndex.map {
          case (consumes, chunkNo) =>
            consumes.foreach {
              case (channels, wk) =>
                consume(store, channels, wk.patterns, wk.continuation, wk.persist)
            }
            val num  = "%02d".format(chunkNo)
            val size = "%02d".format(consumes.size)
            logger.debug(s"$num: checkpointing $size consumes")
            (State(getCheckpoint(store), store.toMap), chunkNo)
        }

        val tests = states.map {
          case (State(checkpoint, expectedContents), chunkNo) =>
            reset(store, checkpoint)
            val test = store.toMap == expectedContents
            val num  = "%02d".format(chunkNo)
            if (test) {
              logger.debug(s"$num: store had expected contents")
            } else {
              logger.error(s"$num: store had unexpected contents")
            }
            test
        }

        !tests.contains(false)
      }
    }
    check(prop)
  }

  // TODO: get the join map in the mix
  "when resetting to a bunch of checkpoints made with consumes and produces, the store" should
    "have the expected contents" in {
    val prop = Prop.forAllNoShrink { (data: Seq[(TestConsumeMap, TestProduceMap)]) =>
      withTestStore { store =>
        logger.debug(s"Test: ${data.length} stages")

        val states = data.zipWithIndex.map {
          case ((consumes, produces), chunkNo) =>
            consumes.foreach {
              case (channels, wk) =>
                consume(store, channels, wk.patterns, wk.continuation, wk.persist)
            }
            produces.foreach {
              case (channel, datum) =>
                produce(store, channel, datum.a, datum.persist)
            }
            val num          = "%02d".format(chunkNo)
            val consumesSize = "%02d".format(consumes.size)
            val producesSize = "%02d".format(produces.size)
            logger.debug(s"$num: checkpointing $consumesSize consumes and $producesSize produces")
            (State(getCheckpoint(store), store.toMap), chunkNo)
        }

        val tests = states.map {
          case (State(checkpoint, expectedContents), chunkNo) =>
            reset(store, checkpoint)
            val test = store.toMap == expectedContents
            val num  = "%02d".format(chunkNo)
            if (test) {
              logger.debug(s"$num: store had expected contents")
            } else {
              logger.error(s"$num: store had unexpected contents")
            }
            test
        }

        !tests.contains(false)
      }
    }
    check(prop)
  }
}

class LMDBStoreHistoryActionsTests extends LMDBStoreTestsBase with HistoryActionsTests
