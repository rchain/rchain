package coop.rchain.rspace

import java.lang.{Byte => JByte}

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringMatch, StringsCaptor, Wildcard}
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal.{Datum, GNAT, Row, WaitingContinuation}
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import org.scalacheck.Prop
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import scodec.Codec
import scala.collection.immutable.Seq

//noinspection ZeroIndexToHead
trait HistoryActionsTests[F[_]]
    extends StorageTestsBase[F, String, Pattern, Nothing, String, StringsCaptor]
    with TestImplicitHelpers
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
  private[this] def getRootHash(
      store: IStore[String, Pattern, String, StringsCaptor],
      branch: Branch
  ): Blake2b256Hash =
    store.withTxn(store.createTxnRead()) { txn =>
      store.withTrieTxn(txn) { trieTxn =>
        store.trieStore.getRoot(trieTxn, branch).get
      }
    }
  "createCheckpoint on an empty store" should "return the expected hash" in withTestSpace { space =>
    for {
      checkpoint <- space.createCheckpoint()
      _ = checkpoint.root shouldBe Blake2b256Hash.fromHex(
        "ff3c5e70a028b7956791a6b3d8db9cd11f469e0088db22dd3afbc86997fe86a3"
      )
    } yield ()
  }

  "consume then createCheckpoint" should "return the expected hash and the TrieStore should contain the expected value" in
    withTestSpace { space =>
      val channels = List("ch1")
      val gnat = GNAT(
        channels,
        List.empty[Datum[String]],
        List(
          WaitingContinuation.create(channels, List[Pattern](Wildcard), new StringsCaptor, false)
        )
      )

      val channelsHash: Blake2b256Hash = space.store.hashChannels(gnat.channels)

      val leafPointer = LeafPointer(Trie.hash[Blake2b256Hash, TestGNAT](Leaf(channelsHash, gnat)))
      val skip        = Skip(channelsHash.bytes.drop(1), leafPointer)
      val skipHash    = Trie.hash(skip)(codecK, Codec[String])

      val nodeHash = Trie.hash[Blake2b256Hash, TestGNAT](
        Node(
          PointerBlock
            .create()
            .updated(List((JByte.toUnsignedInt(channelsHash.bytes.head), NodePointer(skipHash))))
        )
      )

      for {
        _ <- space.consume(
              gnat.channels,
              gnat.wks.head.patterns,
              gnat.wks.head.continuation,
              gnat.wks.head.persist
            )
        _          = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe None
        retrieved  <- space.retrieve(getRootHash(space.store, space.store.trieBranch), channelsHash)
        _          = retrieved shouldBe None
        checkpoint <- space.createCheckpoint()
        _          = checkpoint.root shouldBe nodeHash
        _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe Some(
          gnat
        )
        retrieved <- space.retrieve(nodeHash, channelsHash)
        _         = retrieved shouldBe Some(gnat)
      } yield ()
    }

  "consume twice then createCheckpoint" should "persist the expected values in the TrieStore" in
    withTestSpace { space =>
      val gnat1 = {
        val channels = List("ch1")
        GNAT(
          channels,
          List.empty[Datum[String]],
          List(
            WaitingContinuation
              .create(channels, List[Pattern](Wildcard), new StringsCaptor, false)
          )
        )
      }

      val gnat2 = {
        val channels = List("ch2")
        GNAT(
          channels,
          List.empty[Datum[String]],
          List(
            WaitingContinuation
              .create(channels, List[Pattern](Wildcard), new StringsCaptor, false)
          )
        )
      }

      val channelsHash1: Blake2b256Hash = space.store.hashChannels(gnat1.channels)
      val channelsHash2                 = space.store.hashChannels(gnat2.channels)

      for {
        _ <- space.consume(
              gnat1.channels,
              gnat1.wks.head.patterns,
              gnat1.wks.head.continuation,
              gnat1.wks.head.persist
            )
        _ <- space.consume(
              gnat2.channels,
              gnat2.wks.head.patterns,
              gnat2.wks.head.continuation,
              gnat2.wks.head.persist
            )
        _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash1) shouldBe None
        _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash2) shouldBe None
        _ <- space.createCheckpoint()
        _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash1) shouldBe Some(
          gnat1
        )
        retrieved1 <- space.retrieve(
                       getRootHash(space.store, space.store.trieBranch),
                       channelsHash1
                     )
        _ = retrieved1 shouldBe Some(
          gnat1
        )
        _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash2) shouldBe Some(
          gnat2
        )
        retrieved2 <- space.retrieve(
                       getRootHash(space.store, space.store.trieBranch),
                       channelsHash2
                     )
        _ = retrieved2 shouldBe Some(
          gnat2
        )
      } yield (())
    }

  "produce a bunch and then createCheckpoint" should "persist the expected values in the TrieStore" in
    forAll { (data: TestProduceMap) =>
      withTestSpace { space =>
        (for {
          gnats <- (data
                    .map {
                      case (channel, datum) =>
                        GNAT(
                          List(channel),
                          List(datum),
                          List.empty[WaitingContinuation[Pattern, StringsCaptor]]
                        )
                    }
                    .toList)
                    .pure[F]
          _ = gnats.map {
            case GNAT(List(channel), List(datum), _) =>
              space.produce(channel, datum.a, datum.persist)
          }
          channelHashes = gnats.map(gnat => space.store.hashChannels(gnat.channels))
          _ = history
            .lookup(space.store.trieStore, space.store.trieBranch, channelHashes) shouldBe None
          _ <- space.createCheckpoint()
        } yield
          (history
            .lookup(space.store.trieStore, space.store.trieBranch, channelHashes)
            .get should contain theSameElementsAs gnats))
      }
    }

  "consume a bunch and then createCheckpoint" should "persist the expected values in the TrieStore" in
    forAll { (data: TestConsumeMap) =>
      withTestSpace { space =>
        for {
          gnats <- (data
                    .map {
                      case (channels, wk) =>
                        GNAT(channels, List.empty[Datum[String]], List(wk))
                    }
                    .toList)
                    .pure[F]

          _ = gnats.map {
            case GNAT(channels, _, List(wk)) =>
              space.consume(channels, wk.patterns, wk.continuation, wk.persist)
          }
          channelHashMap                      = gnats.map(gnat => space.store.hashChannels(gnat.channels) -> gnat).toMap
          channelHashes: List[Blake2b256Hash] = channelHashMap.keys.toList

          _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelHashes) shouldBe None

          checkpoint <- space.createCheckpoint()
          _ = history
            .lookup(space.store.trieStore, space.store.trieBranch, channelHashes)
            .get should contain theSameElementsAs gnats
          _ <- channelHashes
                .map(
                  channelHash =>
                    space.retrieve(checkpoint.root, channelHash).map { retrieved =>
                      retrieved.get shouldBe channelHashMap(channelHash)
                    }
                )
                .sequence
        } yield (())
      }
    }

  "consume and produce a match and then createCheckpoint " should "result in an empty TrieStore" in
    withTestSpace { space =>
      val channels     = List("ch1")
      val channelsHash = space.store.hashChannels(channels)

      for {
        r1         <- space.consume(channels, List(Wildcard), new StringsCaptor, persist = false)
        _          = r1 shouldBe Right(None)
        r2         <- space.produce(channels.head, "datum", persist = false)
        _          = r2 shouldBe defined
        _          = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe None
        checkpoint <- space.createCheckpoint()
        _ = checkpoint.root shouldBe Blake2b256Hash.fromHex(
          "ff3c5e70a028b7956791a6b3d8db9cd11f469e0088db22dd3afbc86997fe86a3"
        )
        _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe None
      } yield (())
    }

  "createCheckpoint, consume, reset" should "result in an empty store" in
    withTestSpace { space =>
      for {
        checkpoint <- space.createCheckpoint()
        root0      = checkpoint.root
        gnat1 = {
          val channels = List("ch1")
          GNAT(
            channels,
            List.empty[Datum[String]],
            List(
              WaitingContinuation
                .create(channels, List[Pattern](Wildcard), new StringsCaptor, false)
            )
          )
        }
        _ <- space.consume(
              gnat1.channels,
              gnat1.wks.head.patterns,
              gnat1.wks.head.continuation,
              gnat1.wks.head.persist
            )
        _ = space.store.isEmpty shouldBe false
        _ <- space.reset(root0)

      } yield (space.store.isEmpty shouldBe true)
    }

  "createCheckpoint, consume, createCheckpoint, reset to first checkpoint, reset to second checkpoint" should
    "result in a store that contains the consume and appropriate join map" in withTestSpace {
    space =>
      for {
        checkpoint0 <- space.createCheckpoint()
        root0       = checkpoint0.root
        gnat1 = {
          val channels = List("ch1", "ch2")
          GNAT(
            channels,
            List.empty[Datum[String]],
            List(
              WaitingContinuation
                .create(channels, List[Pattern](Wildcard, Wildcard), new StringsCaptor, false)
            )
          )
        }
        _ <- space.consume(
              gnat1.channels,
              gnat1.wks.head.patterns,
              gnat1.wks.head.continuation,
              gnat1.wks.head.persist
            )
        checkpoint1                                                      <- space.createCheckpoint()
        root1                                                            = checkpoint1.root
        contents1: Map[Seq[String], Row[Pattern, String, StringsCaptor]] = space.store.toMap
        _                                                                = space.store.isEmpty shouldBe false
        _ = space.store.withTxn(space.store.createTxnRead()) { txn =>
          space.store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
          space.store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
        }

        // Rollback to first checkpoint
        _ <- space.reset(root0)
        _ = space.store.isEmpty shouldBe true
        _ = space.store.withTxn(space.store.createTxnRead()) { txn =>
          space.store.getJoin(txn, "ch1") shouldBe Nil
          space.store.getJoin(txn, "ch2") shouldBe Nil
        }

        // Rollback to second checkpoint
        _ <- space.reset(root1)
        _ = space.store.isEmpty shouldBe false
        _ = space.store.withTxn(space.store.createTxnRead()) { txn =>
          space.store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
          space.store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
        }

      } yield (space.store.toMap shouldBe contents1)
  }

  "when resetting to a bunch of checkpoints made with produces, the store" should
    "have the expected contents" in {
    val prop = Prop.forAllNoShrink { (data: Seq[TestProduceMap]) =>
      withTestSpace { space =>
        logger.debug(s"Test: ${data.length} stages")

        val states = data.zipWithIndex.map {
          case (produces, chunkNo) =>
            for {
              produceEffects <- produces
                                 .map {
                                   case (channel, datum) =>
                                     space.produce(channel, datum.a, datum.persist)
                                 }
                                 .toList
                                 .sequence
              checkpoint <- space.createCheckpoint()
            } yield {
              val num  = "%02d".format(chunkNo)
              val size = "%02d".format(produces.size)
              logger.debug(s"$num: checkpointing $size produces")
              (State(checkpoint.root, space.store.toMap, space.store.joinMap), chunkNo)
            }
        }
        for {
          stateEffect <- states.toList.sequence
        } yield (validateIndexedStates(space, stateEffect, "produces_reset"))

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
            for {
              consumeEffects <- consumes
                                 .map {
                                   case (channels, wk) =>
                                     space.consume(
                                       channels,
                                       wk.patterns,
                                       wk.continuation,
                                       wk.persist
                                     )
                                 }
                                 .toList
                                 .sequence
              checkpoint <- space.createCheckpoint()
            } yield {
              val num  = "%02d".format(chunkNo)
              val size = "%02d".format(consumes.size)
              logger.debug(s"$num: checkpointing $size consumes")
              (State(checkpoint.root, space.store.toMap, space.store.joinMap), chunkNo)
            }
        }
        for {
          stateEffect <- states.toList.sequence
        } yield (validateIndexedStates(space, stateEffect, "consumes_reset"))
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
            for {
              consumeEffects <- consumes
                                 .map {
                                   case (channels, wk) =>
                                     space.consume(
                                       channels,
                                       wk.patterns,
                                       wk.continuation,
                                       wk.persist
                                     )
                                 }
                                 .toList
                                 .sequence
              produceEffects <- produces
                                 .map {
                                   case (channel, datum) =>
                                     space.produce(channel, datum.a, datum.persist)
                                 }
                                 .toList
                                 .sequence

              checkpoint <- space.createCheckpoint()
            } yield {
              val num          = "%02d".format(chunkNo)
              val consumesSize = "%02d".format(consumes.size)
              val producesSize = "%02d".format(produces.size)
              logger.debug(s"$num: checkpointing $consumesSize consumes and $producesSize produces")
              (
                State(checkpoint.root, space.store.toMap, space.store.joinMap),
                chunkNo
              )
            }
        }
        for {
          stateEffect <- states.toList.sequence
        } yield (validateIndexedStates(space, stateEffect, "produces_consumes_reset"))
      }
    }
    check(prop)
  }

  "consume, produce, produce" should "result in the expected trace log" in withTestSpace { space =>
    val channels = List("ch1", "ch2")
    val patterns = List[Pattern](Wildcard, Wildcard)
    val k        = new StringsCaptor
    val data     = List("datum1", "datum2")

    for {
      _                  <- space.consume(channels, patterns, k, false)
      _                  <- space.produce(channels(0), data(0), false)
      _                  <- space.produce(channels(1), data(1), false)
      expectedConsume    = Consume.create(channels, patterns, k, false)
      expectedProduce1   = Produce.create(channels(0), data(0), false)
      expectedProduce2   = Produce.create(channels(1), data(1), false)
      commEvent          = COMM(expectedConsume, Seq(expectedProduce1, expectedProduce2))
      Checkpoint(_, log) = space.createCheckpoint()
    } yield
      (log should contain theSameElementsInOrderAs Seq(
        commEvent,
        expectedProduce2,
        expectedProduce1,
        expectedConsume
      ))
  }

  "an install" should "not be persisted to the history trie" in withTestSpace { space =>
    val key      = List("ch1")
    val patterns = List(Wildcard)

    for {
      emptyCheckpoint <- space.createCheckpoint()
      _               <- space.install(key, patterns, new StringsCaptor)
      checkpoint      <- space.createCheckpoint()
      _               = checkpoint.log shouldBe empty
    } yield (emptyCheckpoint.root shouldBe checkpoint.root)
  }

  it should "not be persisted to the history trie after second install" in withTestSpace { space =>
    val key      = List("ch1")
    val patterns = List(Wildcard)

    for {
      emptyCheckpoint <- space.createCheckpoint()
      _               <- space.install(key, patterns, new StringsCaptor)
      checkpoint      <- space.createCheckpoint()
      _               = checkpoint.log shouldBe empty
      _               = emptyCheckpoint.root shouldBe checkpoint.root
      _               <- space.install(key, patterns, new StringsCaptor)
      checkpoint2     <- space.createCheckpoint()
      _               = checkpoint2.log shouldBe empty
    } yield (emptyCheckpoint.root shouldBe checkpoint2.root)
  }

  it should "be available after resetting to a checkpoint" in withTestSpace { space =>
    val channel      = "ch1"
    val datum        = "datum1"
    val key          = List(channel)
    val patterns     = List(Wildcard)
    val continuation = new StringsCaptor

    for {
      _            <- space.install(key, patterns, continuation)
      afterInstall <- space.createCheckpoint()
      _            <- space.reset(afterInstall.root)
      // Produce should produce a COMM event, because the continuation was installed during reset
      // even though it was not persisted to the history trie
      _            <- space.produce(channel, datum, persist = false)
      afterProduce <- space.createCheckpoint()
      produceEvent = Produce.create(channel, datum, false)
    } yield
      (afterProduce.log should contain theSameElementsAs (Seq(
        COMM(
          Consume.create[String, Pattern, StringsCaptor](key, patterns, continuation, true),
          List(produceEvent)
        ),
        produceEvent
      )))
  }

}

trait LegacyHistoryActionsTests
    extends StorageTestsBase[Id, String, Pattern, Nothing, String, StringsCaptor]
    with TestImplicitHelpers
    with GeneratorDrivenPropertyChecks
    with Checkers {

  "reset to an unknown checkpoint" should "result in an exception" in
    withTestSpace { space =>
      val unknownHash =
        Blake2b256Hash.fromHex("ff3c5e70a028b7956791a6b3d8db00000f469e0088db22dd3afbc86997fe86a0")
      (the[Exception] thrownBy {
        space.reset(unknownHash)
      } should have message "Unknown root.")
    }
}

class MixedStoreHistoryActionsTests
    extends MixedStoreTestsBase[Id]
    with HistoryActionsTests[Id]
    with LegacyHistoryActionsTests
    with IdTests[String, Pattern, Nothing, String, StringsCaptor]
class LMDBStoreHistoryActionsTests
    extends LMDBStoreTestsBase[Id]
    with HistoryActionsTests[Id]
    with LegacyHistoryActionsTests
    with IdTests[String, Pattern, Nothing, String, StringsCaptor]
class InMemStoreHistoryActionsTests
    extends InMemoryStoreTestsBase[Id]
    with HistoryActionsTests[Id]
    with LegacyHistoryActionsTests
    with IdTests[String, Pattern, Nothing, String, StringsCaptor]
