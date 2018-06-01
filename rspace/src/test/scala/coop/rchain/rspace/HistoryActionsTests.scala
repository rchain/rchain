package coop.rchain.rspace

import java.lang.{Byte => JByte}

import cats.implicits._
import coop.rchain.catscontrib.seq._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringsCaptor, Wildcard}
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal.{Datum, GNAT, WaitingContinuation}
import coop.rchain.rspace.test.ArbitraryInstances._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scodec.Codec

import scala.collection.immutable.Seq

trait HistoryActionsTests
    extends StorageTestsBase[String, Pattern, String, StringsCaptor]
    with GeneratorDrivenPropertyChecks {

  implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
  implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
  implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

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

      val nodeHash = Trie.hash[Blake2b256Hash, GNAT[String, Pattern, String, StringsCaptor]](
        Node(
          PointerBlock
            .create()
            .updated(
              List((JByte.toUnsignedInt(channelsHash.bytes.head),
                    Some(Trie.hash[Blake2b256Hash, GNAT[String, Pattern, String, StringsCaptor]](
                      Leaf(channelsHash, gnat))))))))

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
                       List(WaitingContinuation(List(Wildcard), new StringsCaptor, false)))

      val channelsHash1: Blake2b256Hash = store.hashChannels(gnat1.channels)

      consume(store,
              gnat1.channels,
              gnat1.wks.head.patterns,
              gnat1.wks.head.continuation,
              gnat1.wks.head.persist)

      val gnat2 = GNAT(List("ch2"),
                       List.empty[Datum[String]],
                       List(WaitingContinuation(List(Wildcard), new StringsCaptor, false)))

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
      forAll { (data: Map[String, Datum[String]]) =>
        val gnats: Seq[GNAT[String, Pattern, String, StringsCaptor]] =
          data.map {
            case (channel, datum) =>
              GNAT[String, Pattern, String, StringsCaptor](List(channel), List(datum), List.empty)
          }.toList

        gnats.foreach {
          case GNAT(List(channel), List(datum), _) =>
            produce(store, channel, datum.a, datum.persist)
        }

        val channelHashes = gnats.map(gnat => store.hashChannels(gnat.channels))

        val preActuals =
          channelHashes.traverse[Option, GNAT[String, Pattern, String, StringsCaptor]] { hash =>
            history.lookup(store.trieStore, hash)
          }

        preActuals shouldBe None

        val _ = getCheckpoint(store)

        val actuals =
          channelHashes.traverse[Option, GNAT[String, Pattern, String, StringsCaptor]] { hash =>
            history.lookup(store.trieStore, hash)
          }

        actuals.value should contain theSameElementsAs gnats
      }
  }

  "consume a bunch and then getCheckpoint" should "persist the expected values in the TrieStore" in withTestStore {
    store =>
      forAll { (data: Map[List[String], WaitingContinuation[Pattern, StringsCaptor]]) =>
        val gnats: Seq[GNAT[String, Pattern, String, StringsCaptor]] =
          data.map {
            case (channels, wk) =>
              GNAT[String, Pattern, String, StringsCaptor](channels, List.empty, List(wk))
          }.toList

        gnats.foreach {
          case GNAT(channels, _, List(wk)) =>
            consume(store, channels, wk.patterns, wk.continuation, wk.persist)
        }

        val channelHashes = gnats.map(gnat => store.hashChannels(gnat.channels))

        val preActuals =
          channelHashes.traverse[Option, GNAT[String, Pattern, String, StringsCaptor]] { hash =>
            history.lookup(store.trieStore, hash)
          }

        preActuals shouldBe None

        val _ = getCheckpoint(store)

        val actuals =
          channelHashes.traverse[Option, GNAT[String, Pattern, String, StringsCaptor]] { hash =>
            history.lookup(store.trieStore, hash)
          }

        actuals.value should contain theSameElementsAs gnats
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
}

class LMDBStoreHistoryActionsTests extends LMDBStoreTestsBase with HistoryActionsTests
