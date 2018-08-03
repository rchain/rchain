package coop.rchain.rspace

import java.lang.{Byte => JByte}

import cats.implicits._
import com.google.common.collect.HashMultiset
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringMatch, StringsCaptor, Wildcard}
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal.{Datum, GNAT, Row, WaitingContinuation}
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import org.scalacheck.Prop
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import scodec.Codec

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

//noinspection ZeroIndexToHead
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

  /**
    * Helper for testing purposes only.
    */
  private[this] def getRootHash(store: IStore[String, Pattern, String, StringsCaptor],
                                branch: Branch): Blake2b256Hash =
    store.withTxn(store.createTxnRead()) { txn =>
      store.withTrieTxn(txn) { trieTxn =>
        store.trieStore.getRoot(trieTxn, branch).get
      }
    }

  case class State(
      checkpoint: Blake2b256Hash,
      contents: Map[Seq[String], Row[Pattern, String, StringsCaptor]],
      joins: Map[Blake2b256Hash, Seq[Seq[String]]]
  )

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

      space.retrieve(getRootHash(space.store, space.store.trieBranch), channelsHash) shouldBe None

      space.createCheckpoint().root shouldBe nodeHash

      history
        .lookup(space.store.trieStore, space.store.trieBranch, channelsHash)
        .value shouldBe gnat

      space.retrieve(nodeHash, channelsHash).value shouldBe gnat
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

      space
        .retrieve(getRootHash(space.store, space.store.trieBranch), channelsHash1)
        .value shouldBe gnat1

      history
        .lookup(space.store.trieStore, space.store.trieBranch, channelsHash2)
        .value shouldBe gnat2

      space
        .retrieve(getRootHash(space.store, space.store.trieBranch), channelsHash2)
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

        val channelHashMap =
          gnats.map(gnat => space.store.hashChannels(gnat.channels) -> gnat).toMap
        val channelHashes = channelHashMap.keys.toList

        history.lookup(space.store.trieStore, space.store.trieBranch, channelHashes) shouldBe None

        val checkpoint = space.createCheckpoint()

        history
          .lookup(space.store.trieStore, space.store.trieBranch, channelHashes)
          .value should contain theSameElementsAs gnats

        for ((channelHash, gnat) <- channelHashMap) {
          space.retrieve(checkpoint.root, channelHash).value shouldBe gnat
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
}

class LMDBStoreHistoryActionsTests extends LMDBStoreTestsBase with HistoryActionsTests {
  "an install" should "not be persisted to the history trie" in withTestSpace { space =>
    val key      = List("ch1")
    val patterns = List(Wildcard)

    val emptyCheckpoint = space.createCheckpoint()
    space.install(key, patterns, new StringsCaptor)

    val checkpoint = space.createCheckpoint()

    checkpoint.log shouldBe empty
    emptyCheckpoint.root shouldBe checkpoint.root
  }

  it should "not be persisted to the history trie after second install" in withTestSpace { space =>
    val key      = List("ch1")
    val patterns = List(Wildcard)

    val emptyCheckpoint = space.createCheckpoint()
    space.install(key, patterns, new StringsCaptor)

    val checkpoint = space.createCheckpoint()

    checkpoint.log shouldBe empty
    emptyCheckpoint.root shouldBe checkpoint.root

    space.install(key, patterns, new StringsCaptor)

    val checkpoint2 = space.createCheckpoint()
    checkpoint2.log shouldBe empty
    emptyCheckpoint.root shouldBe checkpoint2.root
  }

  it should "be available after resetting to a checkpoint" in withTestSpace { space =>
    val channel      = "ch1"
    val datum        = "datum1"
    val key          = List(channel)
    val patterns     = List(Wildcard)
    val continuation = new StringsCaptor

    space.install(key, patterns, continuation)

    val afterInstall = space.createCheckpoint()
    space.reset(afterInstall.root)

    // Produce should produce a COMM event, because the continuation was installed during reset
    // even though it was not persisted to the history trie
    space.produce(channel, datum, persist = false)

    val afterProduce = space.createCheckpoint()
    val produceEvent = Produce.create(channel, datum, false)
    afterProduce.log should contain theSameElementsAs (Seq(
      COMM(Consume.create[String, Pattern, StringsCaptor](key, patterns, continuation, true),
           List(produceEvent)),
      produceEvent))
  }
}

class InMemStoreHistoryActionsTests extends InMemoryStoreTestsBase with HistoryActionsTests
