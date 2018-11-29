package coop.rchain.rspace

import java.lang.{Byte => JByte}

import cats._
import cats.effect._
import cats.implicits._
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
import scala.collection.immutable.Seq
import coop.rchain.rspace.test.ArbitraryInstances._
import org.scalatest.enablers.Definition

import scala.util.Random

trait StorageActionsTests[F[_]]
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

  "produce" should
    "persist a piece of data in the store" in withTestSpace { space =>
    val store   = space.store
    val key     = List("ch1")
    val keyHash = store.hashChannels(key)

    for {
      r <- space.produce(key.head, "datum", persist = false)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe key
        store.getPatterns(txn, key) shouldBe Nil
        store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum", persist = false))
        store.getWaitingContinuation(txn, key) shouldBe Nil
      }

      _ = r shouldBe Right(None)
      //store is not empty - we have 'A' stored
    } yield (store.isEmpty shouldBe false)
  }

  "producing twice on the same channel" should
    "persist two pieces of data in the store" in withTestSpace { space =>
    val store   = space.store
    val key     = List("ch1")
    val keyHash = store.hashChannels(key)

    for {
      r1 <- space.produce(key.head, "datum1", persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe key
        store.getPatterns(txn, key) shouldBe Nil
        store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum1", false))
        store.getWaitingContinuation(txn, key) shouldBe Nil
      }
      _  = r1 shouldBe Right(None)
      r2 <- space.produce(key.head, "datum2", persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe key
        store.getPatterns(txn, key) shouldBe Nil
        store.getData(txn, key) should contain theSameElementsAs List(
          Datum.create(key.head, "datum1", false),
          Datum.create(key.head, "datum2", false)
        )
        store.getWaitingContinuation(txn, key) shouldBe Nil
      }
      _ = r2 shouldBe Right(None)
      //store is not empty - we have 2 As stored
    } yield (store.isEmpty shouldBe false)
  }

  "consuming on one channel" should
    "persist a continuation in the store" in withTestSpace { space =>
    val store    = space.store
    val key      = List("ch1")
    val patterns = List(Wildcard)
    val keyHash  = store.hashChannels(key)

    for {
      r <- space.consume(key, patterns, new StringsCaptor, persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe List("ch1")
        store.getPatterns(txn, key) shouldBe List(patterns)
        store.getData(txn, key) shouldBe Nil
        store.getWaitingContinuation(txn, key) should not be empty
      }
      _ = r shouldBe Right(None)
      //there is a continuation stored in the storage
    } yield (store.isEmpty shouldBe false)
  }

  "consuming on three channels" should
    "persist a continuation in the store" in withTestSpace { space =>
    val store    = space.store
    val key      = List("ch1", "ch2", "ch3")
    val patterns = List(Wildcard, Wildcard, Wildcard)
    val keyHash  = store.hashChannels(key)

    for {
      r <- space.consume(key, patterns, new StringsCaptor, persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe key
        store.getPatterns(txn, key) shouldBe List(patterns)
        store.getData(txn, key) shouldBe Nil
        store.getWaitingContinuation(txn, key) should not be empty
      }
      _ = r shouldBe Right(None)
      //continuation is left in the storage
    } yield (store.isEmpty shouldBe false)
  }

  "producing and then consuming on the same channel" should
    "return the continuation and data" in withTestSpace { space =>
    val store   = space.store
    val key     = List("ch1")
    val keyHash = store.hashChannels(key)

    for {
      r1 <- space.produce(key.head, "datum", persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe key
        store.getPatterns(txn, key) shouldBe Nil
        store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum", false))
        store.getWaitingContinuation(txn, key) shouldBe Nil
      }
      _ = r1 shouldBe Right(None)

      r2 <- space.consume(key, List(Wildcard), new StringsCaptor, persist = false)
      _  = store.isEmpty shouldBe true
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe Nil
        store.getPatterns(txn, key) shouldBe Nil
        store.getData(txn, key) shouldBe Nil
        store.getWaitingContinuation(txn, key) shouldBe Nil
      }
      _ = r2 shouldBe defined
      _ = runK(r2)
      _ = getK(r2).results should contain theSameElementsAs List(List("datum"))
    } yield (store.isEmpty shouldBe true)
  }

  "producing three times then doing consuming three times" should "work" in withTestSpace { space =>
    val store = space.store
    for {
      r1 <- space.produce("ch1", "datum1", persist = false)
      r2 <- space.produce("ch1", "datum2", persist = false)
      r3 <- space.produce("ch1", "datum3", persist = false)
      _  = r1 shouldBe Right(None)
      _  = r2 shouldBe Right(None)
      _  = r3 shouldBe Right(None)
      r4 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      _  = runK(r4)
      _  = getK(r4).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
      r5 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      _  = runK(r5)
      _  = getK(r5).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
      r6 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      _  = runK(r6)
      _  = getK(r6).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
    } yield (store.isEmpty shouldBe true)
  }

  "producing on channel, consuming on that channel and another, and then producing on the other channel" should
    "return a continuation and all the data" in withTestSpace { space =>
    val store           = space.store
    val produceKey1     = List("ch1")
    val produceKey1Hash = store.hashChannels(produceKey1)

    val consumeKey     = List("ch1", "ch2")
    val consumeKeyHash = store.hashChannels(consumeKey)
    val consumePattern = List(Wildcard, Wildcard)

    val produceKey2     = List("ch2")
    val produceKey2Hash = store.hashChannels(produceKey2)

    for {
      r1 <- space.produce(produceKey1.head, "datum1", persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, produceKey1Hash) shouldBe produceKey1
        store.getPatterns(txn, produceKey1) shouldBe Nil
        store.getData(txn, produceKey1) shouldBe List(
          Datum.create(produceKey1.head, "datum1", persist = false)
        )
        store.getWaitingContinuation(txn, produceKey1) shouldBe Nil
      }
      _ = r1 shouldBe Right(None)

      r2 <- space.consume(consumeKey, consumePattern, new StringsCaptor, persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, produceKey1Hash) shouldBe produceKey1
        store.getPatterns(txn, produceKey1) shouldBe Nil
        store.getData(txn, produceKey1) shouldBe List(
          Datum.create(produceKey1.head, "datum1", persist = false)
        )
        store.getWaitingContinuation(txn, produceKey1) shouldBe Nil
        store.getChannels(txn, consumeKeyHash) shouldBe consumeKey
        store.getPatterns(txn, consumeKey) shouldBe List(consumePattern)
        store.getData(txn, consumeKey) shouldBe Nil
        store.getWaitingContinuation(txn, consumeKey) should not be empty
      }
      _  = r2 shouldBe Right(None)
      r3 <- space.produce(produceKey2.head, "datum2", persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
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
      _ = r3 shouldBe defined
      _ = runK(r3)
      _ = getK(r3).results should contain theSameElementsAs List(List("datum1", "datum2"))
    } yield (store.isEmpty shouldBe true)
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

    for {
      r1 <- space.produce(produceKey1.head, "datum1", persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, produceKey1Hash) shouldBe produceKey1
        store.getPatterns(txn, produceKey1) shouldBe Nil
        store.getData(txn, produceKey1) shouldBe List(
          Datum.create(produceKey1.head, "datum1", false)
        )
        store.getWaitingContinuation(txn, produceKey1) shouldBe Nil
      }
      _  = r1 shouldBe Right(None)
      r2 <- space.produce(produceKey2.head, "datum2", persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, produceKey2Hash) shouldBe produceKey2
        store.getPatterns(txn, produceKey2) shouldBe Nil
        store.getData(txn, produceKey2) shouldBe List(
          Datum.create(produceKey2.head, "datum2", false)
        )
        store.getWaitingContinuation(txn, produceKey2) shouldBe Nil
      }
      _  = r2 shouldBe Right(None)
      r3 <- space.produce(produceKey3.head, "datum3", persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, produceKey3Hash) shouldBe produceKey3
        store.getPatterns(txn, produceKey3) shouldBe Nil
        store.getData(txn, produceKey3) shouldBe List(
          Datum.create(produceKey3.head, "datum3", false)
        )
        store.getWaitingContinuation(txn, produceKey3) shouldBe Nil
      }
      _  = r3 shouldBe Right(None)
      r4 <- space.consume(List("ch1", "ch2", "ch3"), patterns, new StringsCaptor, persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, consumeKeyHash) shouldBe Nil
        store.getPatterns(txn, consumeKey) shouldBe Nil
        store.getData(txn, consumeKey) shouldBe Nil
        store.getWaitingContinuation(txn, consumeKey) shouldBe Nil
      }
      _ = r4 shouldBe defined
      _ = runK(r4)
      _ = getK(r4).results should contain theSameElementsAs List(List("datum1", "datum2", "datum3"))
    } yield (store.isEmpty shouldBe true)
  }

  "producing three times on the same channel then consuming three times on the same channel" should
    "return three pairs of continuations and data" in withTestSpace { space =>
    val store  = space.store
    val captor = new StringsCaptor

    val key = List("ch1")

    for {
      r1 <- space.produce(key.head, "datum1", persist = false)
      r2 <- space.produce(key.head, "datum2", persist = false)
      r3 <- space.produce(key.head, "datum3", persist = false)
      _  = r1 shouldBe Right(None)
      _  = r2 shouldBe Right(None)
      _  = r3 shouldBe Right(None)
      r4 <- space.consume(key, List(Wildcard), captor, persist = false)
      r5 <- space.consume(key, List(Wildcard), captor, persist = false)
      r6 <- space.consume(key, List(Wildcard), captor, persist = false)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, store.hashChannels(key)) shouldBe Nil
        store.getData(txn, key) shouldBe Nil
        store.getPatterns(txn, key) shouldBe Nil
        store.getWaitingContinuation(txn, key) shouldBe Nil
      }
      continuations = List(r4, r5, r6)
      _             = continuations.forall(_.right.get.isDefined) shouldBe true
      _ = continuations
        .map(unpackEither[Id, String, Pattern, Nothing, StringsCaptor, String])
        .foreach(runK)
      _ = captor.results should contain theSameElementsAs List(
        List("datum3"),
        List("datum2"),
        List("datum1")
      )
    } yield (store.isEmpty shouldBe true)
  }

  "consuming three times on the same channel, then producing three times on that channel" should
    "return three continuations, each paired with distinct pieces of data" in withTestSpace {
    space =>
      val store = space.store

      for {
        _  <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
        _  <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
        _  <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
        r1 <- space.produce("ch1", "datum1", persist = false)
        r2 <- space.produce("ch1", "datum2", persist = false)
        r3 <- space.produce("ch1", "datum3", persist = false)
        _  = r1 shouldBe defined
        _  = r2 shouldBe defined
        _  = r3 shouldBe defined

        _ = List(r1, r2, r3)
          .map(unpackEither[Id, String, Pattern, Nothing, StringsCaptor, String])
          .foreach(runK)

        _ = getK(r1).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
        _ = getK(r2).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
        _ = getK(r3).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))
        _ = getK(r1).results shouldNot contain theSameElementsAs getK(r2).results
        _ = getK(r1).results shouldNot contain theSameElementsAs getK(r3).results
        _ = getK(r2).results shouldNot contain theSameElementsAs getK(r3).results
      } yield (store.isEmpty shouldBe true)
  }

  "consuming three times on the same channel with non-trivial matches, then producing three times on that channel" should
    "return three continuations, each paired with matching data" in withTestSpace { space =>
    val store = space.store

    for {
      _ <- space.consume(
            List("ch1"),
            List(StringMatch("datum1")),
            new StringsCaptor,
            persist = false
          )
      _ <- space.consume(
            List("ch1"),
            List(StringMatch("datum2")),
            new StringsCaptor,
            persist = false
          )
      _ <- space.consume(
            List("ch1"),
            List(StringMatch("datum3")),
            new StringsCaptor,
            persist = false
          )

      r1 <- space.produce("ch1", "datum1", persist = false)
      r2 <- space.produce("ch1", "datum2", persist = false)
      r3 <- space.produce("ch1", "datum3", persist = false)

      _ = r1 shouldBe defined
      _ = r2 shouldBe defined
      _ = r3 shouldBe defined

      _ = List(r1, r2, r3)
        .map(unpackEither[Id, String, Pattern, Nothing, StringsCaptor, String])
        .foreach(runK)

      _ = getK(r1).results shouldBe List(List("datum1"))
      _ = getK(r2).results shouldBe List(List("datum2"))
      _ = getK(r3).results shouldBe List(List("datum3"))

    } yield (store.isEmpty shouldBe true)

  }

  "consuming on two channels, producing on one, then producing on the other" should
    "return a continuation with both pieces of data" in withTestSpace { space =>
    val store = space.store

    for {
      r1 <- space.consume(
             List("ch1", "ch2"),
             List(Wildcard, Wildcard),
             new StringsCaptor,
             persist = false
           )
      r2 <- space.produce("ch1", "datum1", persist = false)
      r3 <- space.produce("ch2", "datum2", persist = false)

      _ = r1 shouldBe Right(None)
      _ = r2 shouldBe Right(None)
      _ = r3 shouldBe defined

      _ = runK(r3)

      _ = getK(r3).results should contain theSameElementsAs List(List("datum1", "datum2"))

    } yield (store.isEmpty shouldBe true)

  }

  "A joined consume with the same channel given twice followed by a produce" should
    "not raises any errors (CORE-365)" in withTestSpace { space =>
    val store = space.store

    val channels = List("ch1", "ch1")

    for {
      r1 <- space.consume(
             channels,
             List(StringMatch("datum1"), StringMatch("datum1")),
             new StringsCaptor,
             persist = false
           )

      r2 <- space.produce("ch1", "datum1", persist = false)
      r3 <- space.produce("ch1", "datum1", persist = false)

      _ = r1 shouldBe Right(None)
      _ = r2 shouldBe Right(None)
      _ = r3 shouldBe defined

      _ = runK(r3)
      _ = getK(r3).results shouldBe List(List("datum1", "datum1"))

    } yield (store.isEmpty shouldBe true)

  }

  "consuming twice on the same channels with different patterns, and then producing on those channels" should
    "return continuations with the expected data" in withTestSpace { space =>
    val store = space.store

    val channels = List("ch1", "ch2")

    for {
      r1 <- space.consume(
             channels,
             List(StringMatch("datum1"), StringMatch("datum2")),
             new StringsCaptor,
             persist = false
           )
      r2 <- space.consume(
             channels,
             List(StringMatch("datum3"), StringMatch("datum4")),
             new StringsCaptor,
             persist = false
           )

      r3 <- space.produce("ch1", "datum3", persist = false)
      r4 <- space.produce("ch2", "datum4", persist = false)
      r5 <- space.produce("ch1", "datum1", persist = false)
      r6 <- space.produce("ch2", "datum2", persist = false)

      _ = r1 shouldBe Right(None)
      _ = r2 shouldBe Right(None)
      _ = r3 shouldBe Right(None)
      _ = r4 shouldBe defined
      _ = r5 shouldBe Right(None)
      _ = r6 shouldBe defined

      _ = List(r4, r6)
        .map(unpackEither[Id, String, Pattern, Nothing, StringsCaptor, String])
        .foreach(runK)

      _ = getK(r4).results should contain theSameElementsAs List(List("datum3", "datum4"))
      _ = getK(r6).results should contain theSameElementsAs List(List("datum1", "datum2"))

    } yield (store.isEmpty shouldBe true)

  }

  "consuming and producing with non-trivial matches" should
    "work" in withTestSpace { space =>
    val store = space.store

    for {
      r1 <- space.consume(
             List("ch1", "ch2"),
             List(Wildcard, StringMatch("datum1")),
             new StringsCaptor,
             persist = false
           )
      r2 <- space.produce("ch1", "datum1", persist = false)

      _ = r1 shouldBe Right(None)
      _ = r2 shouldBe Right(None)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1", "ch2")) shouldBe Nil
        store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", false))
      }

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getWaitingContinuation(txn, List("ch1", "ch2")) should not be empty
        store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
        store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
      }

    } yield (store.isEmpty shouldBe false)
  }

  "consuming twice and producing twice with non-trivial matches" should
    "work" in withTestSpace { space =>
    val store = space.store

    for {
      r1 <- space.consume(
             List("ch1"),
             List(StringMatch("datum1")),
             new StringsCaptor,
             persist = false
           )
      r2 <- space.consume(
             List("ch2"),
             List(StringMatch("datum2")),
             new StringsCaptor,
             persist = false
           )
      r3 <- space.produce("ch1", "datum1", persist = false)
      r4 <- space.produce("ch2", "datum2", persist = false)

      _ = List(r1, r2, r3, r4)
        .map(unpackEither[Id, String, Pattern, Nothing, StringsCaptor, String])
        .foreach(runK)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe Nil
        store.getData(txn, List("ch2")) shouldBe Nil
      }

      _ = getK(r3).results should contain theSameElementsAs List(List("datum1"))
      _ = getK(r4).results should contain theSameElementsAs List(List("datum2"))

    } yield (store.isEmpty shouldBe true)

  }

  "consuming on two channels, consuming on one of those channels, and then producing on both of those channels separately" should
    "return a continuation paired with one piece of data" in
    withTestSpace { space =>
      val store = space.store

      for {
        _ <- space.consume(
              List("ch1", "ch2"),
              List(Wildcard, Wildcard),
              new StringsCaptor,
              persist = false
            )
        _ <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

        r3 <- space.produce("ch1", "datum1", persist = false)
        r4 <- space.produce("ch2", "datum2", persist = false)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getWaitingContinuation(txn, List("ch1", "ch2")) should not be empty
          store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
          store.getWaitingContinuation(txn, List("ch2")) shouldBe Nil
          store.getData(txn, List("ch1")) shouldBe Nil
          store.getData(txn, List("ch2")) shouldBe List(Datum.create("ch2", "datum2", false))
        }

        _ = r3 shouldBe defined
        _ = r4 shouldBe Right(None)

        _ = runK(r3)

        _ = getK(r3).results should contain theSameElementsAs List(List("datum1"))
        //ensure that joins are cleaned-up after all
        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
          store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
        }

      } yield (store.isEmpty shouldBe false)
    }

  /* Persist tests */

  "producing and then doing a persistent consume on the same channel" should
    "return the continuation and data" in withTestSpace { space =>
    val store   = space.store
    val key     = List("ch1")
    val keyHash = store.hashChannels(key)

    for {
      r1 <- space.produce(key.head, "datum", persist = false)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe key
        store.getPatterns(txn, key) shouldBe Nil
        store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum", false))
        store.getWaitingContinuation(txn, key) shouldBe Nil
      }

      _ = r1 shouldBe Right(None)

      // Data exists so the write will not "stick"
      r2 <- space.consume(key, List(Wildcard), new StringsCaptor, persist = true)

      _ = store.isEmpty shouldBe true

      _ = r2 shouldBe defined

      _ = runK(r2)

      _ = getK(r2).results should contain theSameElementsAs List(List("datum"))

      // the data has been consumed, so the write will "stick"
      r3 <- space.consume(key, List(Wildcard), new StringsCaptor, persist = true)
      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getChannels(txn, keyHash) shouldBe List("ch1")
        store.getPatterns(txn, key) shouldBe List(List(Wildcard))
        store.getData(txn, key) shouldBe Nil
        store.getWaitingContinuation(txn, key) should not be empty
      }
    } yield (r3 shouldBe Right(None))
  }

  "producing, doing a persistent consume, and producing again on the same channel" should
    "return the continuation for the first produce, and then the second produce" in withTestSpace {
    space =>
      val store   = space.store
      val key     = List("ch1")
      val keyHash = store.hashChannels(key)

      for {
        r1 <- space.produce(key.head, "datum1", persist = false)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getChannels(txn, keyHash) shouldBe key
          store.getPatterns(txn, key) shouldBe Nil
          store.getData(txn, key) shouldBe List(Datum.create(key.head, "datum1", false))
          store.getWaitingContinuation(txn, key) shouldBe Nil
        }

        _ = r1 shouldBe Right(None)

        // Matching data exists so the write will not "stick"
        r2 <- space.consume(key, List(Wildcard), new StringsCaptor, persist = true)

        _ = store.isEmpty shouldBe true

        _ = r2 shouldBe defined

        _ = runK(r2)

        _ = getK(r2).results should contain theSameElementsAs List(List("datum1"))

        // All matching data has been consumed, so the write will "stick"
        r3 <- space.consume(key, List(Wildcard), new StringsCaptor, persist = true)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getChannels(txn, keyHash) shouldBe List("ch1")
          store.getPatterns(txn, key) shouldBe List(List(Wildcard))
          store.getData(txn, key) shouldBe Nil
          store.getWaitingContinuation(txn, key) should not be empty
        }

        _ = r3 shouldBe Right(None)

        r4 <- space.produce(key.head, "datum2", persist = false)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getChannels(txn, keyHash) shouldBe List("ch1")
          store.getPatterns(txn, key) shouldBe List(List(Wildcard))
          store.getData(txn, key) shouldBe Nil
          store.getWaitingContinuation(txn, key) should not be empty
        }

        _ = r4 shouldBe defined

        _ = runK(r4)

      } yield (getK(r4).results should contain theSameElementsAs List(List("datum2")))
  }

  "doing a persistent consume and producing multiple times" should "work" in withTestSpace {
    space =>
      val store = space.store

      for {
        r1 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getData(txn, List("ch1")) shouldBe Nil
          store.getWaitingContinuation(txn, List("ch1")) should not be empty
        }

        _ = r1 shouldBe Right(None)

        r2 <- space.produce("ch1", "datum1", persist = false)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getData(txn, List("ch1")) shouldBe Nil
          store.getWaitingContinuation(txn, List("ch1")) should not be empty
        }

        _ = r2 shouldBe defined

        _ = runK(r2)

        _ = getK(r2).results should contain theSameElementsAs List(List("datum1"))

        r3 <- space.produce("ch1", "datum2", persist = false)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getData(txn, List("ch1")) shouldBe Nil
          store.getWaitingContinuation(txn, List("ch1")) should not be empty
        }

        _ = r3 shouldBe defined

        _ = runK(r3)

      } yield (getK(r3).results should contain theSameElementsAs List(List("datum2")))
  }

  "consuming and doing a persistient produce" should "work" in withTestSpace { space =>
    val store = space.store

    for {
      r1 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      _ = r1 shouldBe Right(None)

      // A matching continuation exists so the write will not "stick"
      r2 <- space.produce("ch1", "datum1", persist = true)

      _ = store.isEmpty shouldBe true

      _ = r2 shouldBe defined

      _ = runK(r2)

      _ = getK(r2).results should contain theSameElementsAs List(List("datum1"))

      // All matching continuations have been produced, so the write will "stick"
      r3 <- space.produce("ch1", "datum1", persist = true)
      _  = r3 shouldBe Right(None)

    } yield
      (store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      })
  }

  "consuming, doing a persistient produce, and consuming again" should "work" in withTestSpace {
    space =>
      val store = space.store

      for {
        r1 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

        _ = r1 shouldBe Right(None)

        // A matching continuation exists so the write will not "stick"
        r2 <- space.produce("ch1", "datum1", persist = true)

        _ = store.isEmpty shouldBe true

        _ = r2 shouldBe defined

        _ = runK(r2)

        _ = getK(r2).results should contain theSameElementsAs List(List("datum1"))

        // All matching continuations have been produced, so the write will "stick"
        r3 <- space.produce("ch1", "datum1", persist = true)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
          store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
        }

        _ = r3 shouldBe Right(None)

        r4 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
          store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
        }

        _ = r4 shouldBe defined

        _ = runK(r4)

      } yield (getK(r4).results should contain theSameElementsAs List(List("datum1")))
  }

  "doing a persistent produce and consuming twice" should "work" in withTestSpace { space =>
    val store = space.store

    for {
      r1 <- space.produce("ch1", "datum1", persist = true)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      }

      _ = r1 shouldBe Right(None)

      r2 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      }

      _ = r2 shouldBe defined

      _ = runK(r2)

      _ = getK(r2).results should contain theSameElementsAs List(List("datum1"))

      r3 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe List(Datum.create("ch1", "datum1", true))
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      }

      _ = r3 shouldBe defined

      _ = runK(r3)

    } yield (getK(r3).results should contain theSameElementsAs List(List("datum1")))
  }

  "producing three times and doing a persistent consume" should "work" in withTestSpace { space =>
    val store = space.store

    for {
      r1 <- space.produce("ch1", "datum1", persist = false)
      r2 <- space.produce("ch1", "datum2", persist = false)
      r3 <- space.produce("ch1", "datum3", persist = false)

      _ = r1 shouldBe Right(None)
      _ = r2 shouldBe Right(None)
      _ = r3 shouldBe Right(None)

      // Matching data exists so the write will not "stick"
      r4 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) should contain atLeastOneOf (
          Datum.create("ch1", "datum1", false),
          Datum.create("ch1", "datum2", false),
          Datum.create("ch1", "datum3", false)
        )
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      }

      _ = r4 shouldBe defined

      _ = runK(r4)

      _ = getK(r4).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

      // Matching data exists so the write will not "stick"
      r5 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) should contain oneOf (
          Datum.create("ch1", "datum1", false),
          Datum.create("ch1", "datum2", false),
          Datum.create("ch1", "datum3", false)
        )
        store.getWaitingContinuation(txn, List("ch1")) shouldBe Nil
      }

      _ = r5 shouldBe defined

      _ = runK(r5)

      _ = getK(r5).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

      // Matching data exists so the write will not "stick"
      r6 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

      _ = store.isEmpty shouldBe true

      _ = r6 shouldBe defined

      _ = runK(r6)

      _ = getK(r6).results should contain oneOf (List("datum1"), List("datum2"), List("datum3"))

      // All matching data has been consumed, so the write will "stick"
      r7 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)

      _ = store.withTxn(store.createTxnRead()) { txn =>
        store.getData(txn, List("ch1")) shouldBe Nil
        store.getWaitingContinuation(txn, List("ch1")) should not be empty
      }

    } yield (r7 shouldBe Right(None))

  }

  "A persistent produce" should "be available for multiple matches (CORE-633)" in withTestSpace {
    space =>
      val channel = "chan"

      for {
        r1 <- space.produce(channel, data = "datum", persist = true)

        _ = r1 shouldBe Right(None)

        r2 <- space.consume(
               List(channel, channel),
               List(Wildcard, Wildcard),
               new StringsCaptor,
               persist = false
             )

        _ = r2 shouldBe defined

        _ = runK(r2)

      } yield (getK(r2).results should contain(List("datum", "datum")))
  }

  "reset" should "change the state of the store, and reset the trie updates log" in withTestSpace {
    space =>
      val store    = space.store
      val key      = List("ch1")
      val patterns = List(Wildcard)

      for {
        checkpoint0 <- space.createCheckpoint()
        r           <- space.consume(key, patterns, new StringsCaptor, persist = false)
        _           = r shouldBe Right(None)
        _           = store.isEmpty shouldBe false
        _           = store.getTrieUpdates.length shouldBe 1
        _           = store.getTrieUpdateCount shouldBe 1

        _ <- space.reset(checkpoint0.root)

        _ = store.isEmpty shouldBe true
        _ = store.getTrieUpdates.length shouldBe 0
        _ = store.getTrieUpdateCount shouldBe 0

        checkpoint1 <- space.createCheckpoint()
      } yield (checkpoint1.log shouldBe empty)
  }

  "clear" should "empty the store, reset the event log, and reset the trie updates log" in withTestSpace {
    space =>
      val store    = space.store
      val key      = List("ch1")
      val patterns = List(Wildcard)
      val keyHash  = store.hashChannels(key)

      for {
        r <- space.consume(key, patterns, new StringsCaptor, persist = false)

        _ = store.withTxn(store.createTxnRead()) { txn =>
          store.getChannels(txn, keyHash) shouldBe List("ch1")
          store.getPatterns(txn, key) shouldBe List(patterns)
          store.getData(txn, key) shouldBe Nil
          store.getWaitingContinuation(txn, key) should not be empty
        }

        _ = r shouldBe Right(None)
        _ = store.isEmpty shouldBe false
        _ = store.getTrieUpdates.length shouldBe 1
        _ = store.getTrieUpdateCount shouldBe 1

        checkpoint0 <- space.createCheckpoint()
        _           = checkpoint0.log should not be empty

        _ <- space.clear()
        _ = store.isEmpty shouldBe true
        _ = store.getTrieUpdates.length shouldBe 0
        _ = store.getTrieUpdateCount shouldBe 0

        checkpoint1 <- space.createCheckpoint()
      } yield (checkpoint1.log shouldBe empty)
  }

  "clear" should "reset to the same hash on multiple runs" in withTestSpace { space =>
    val store    = space.store
    val key      = List("ch1")
    val patterns = List(Wildcard)

    for {
      emptyCheckpoint <- space.createCheckpoint()
      //put some data so the checkpoint is != empty
      _           <- space.consume(key, patterns, new StringsCaptor, persist = false)
      checkpoint0 <- space.createCheckpoint()
      _           = checkpoint0.log should not be empty
      _           <- space.createCheckpoint()
      _           <- space.clear()
      //force clearing of trie store state
      _ = store.createCheckpoint()
      _ <- space.clear()
      //the checkpointing mechanism should not interfere with the empty root
      checkpoint2 <- space.createCheckpoint()
      _           = checkpoint2.log shouldBe empty
    } yield (checkpoint2.root shouldBe emptyCheckpoint.root)
  }

  "createCheckpoint on an empty store" should "return the expected hash" in withTestSpace { space =>
    for {
      checkpoint <- space.createCheckpoint()
    } yield
      (checkpoint.root shouldBe Blake2b256Hash.fromHex(
        "ff3c5e70a028b7956791a6b3d8db9cd11f469e0088db22dd3afbc86997fe86a3"
      ))
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

        _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe None

        checkpoint <- space.createCheckpoint()
        _          = checkpoint.root shouldBe nodeHash

      } yield
        (history
          .lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe Some(gnat))
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

      val channelsHash1: Blake2b256Hash = space.store.hashChannels(gnat1.channels)

      for {
        _ <- space.consume(
              gnat1.channels,
              gnat1.wks.head.patterns,
              gnat1.wks.head.continuation,
              gnat1.wks.head.persist
            )

        gnat2 = {
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

        channelsHash2: Blake2b256Hash = space.store.hashChannels(gnat2.channels)

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

      } yield
        (history
          .lookup(space.store.trieStore, space.store.trieBranch, channelsHash2)
          shouldBe Some(gnat2))
    }

  "produce a bunch and then createCheckpoint" should "persist the expected values in the TrieStore" in
    forAll { (data: TestProduceMap) =>
      if (data.nonEmpty) {
        withTestSpace { space =>
          val gnats: Seq[TestGNAT] =
            data.map {
              case (channel, datum) =>
                GNAT(
                  List(channel),
                  List(datum),
                  List.empty[WaitingContinuation[Pattern, StringsCaptor]]
                )
            }.toList

          for {
            produces <- gnats
                         .map {
                           case GNAT(List(channel), List(datum), _) =>
                             space.produce(channel, datum.a, datum.persist)
                         }
                         .toList
                         .sequence

            channelHashes = gnats.map(gnat => space.store.hashChannels(gnat.channels))

            _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelHashes) shouldBe None

            _ <- space.createCheckpoint()

          } yield
            (history
              .lookup(space.store.trieStore, space.store.trieBranch, channelHashes)
              .get should contain theSameElementsAs gnats)
        }
      }
    }

  "consume a bunch and then createCheckpoint" should "persist the expected values in the TrieStore" in
    forAll { (data: TestConsumeMap) =>
      val gnats: List[TestGNAT] =
        data
          .filter(_._1.nonEmpty) //channels == Seq.empty will faill in consume
          .map {
            case (channels, wk) =>
              GNAT(channels, List.empty[Datum[String]], List(wk))
          }
          .toList

      if (gnats.nonEmpty) {
        withTestSpace { space =>
          for {
            consumes <- gnats.map {
                         case GNAT(channels, _, List(wk)) =>
                           space.consume(channels, wk.patterns, wk.continuation, wk.persist)
                       }.sequence

            channelHashes = gnats.map(gnat => space.store.hashChannels(gnat.channels))

            _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelHashes) shouldBe None

            _ <- space.createCheckpoint()

          } yield
            (history
              .lookup(space.store.trieStore, space.store.trieBranch, channelHashes)
              .get should contain theSameElementsAs gnats)
        }
      }
    }

  "consume and produce a match and then createCheckpoint " should "result in an empty TrieStore" in
    withTestSpace { space =>
      val channels     = List("ch1")
      val channelsHash = space.store.hashChannels(channels)

      for {
        r1 <- space.consume(channels, List(Wildcard), new StringsCaptor, persist = false)

        _ = r1 shouldBe Right(None)

        r2 <- space.produce(channels.head, "datum", persist = false)

        _ = r2 shouldBe defined

        _ = history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe None

        checkpoint <- space.createCheckpoint()
        _ = checkpoint.root shouldBe Blake2b256Hash.fromHex(
          "ff3c5e70a028b7956791a6b3d8db9cd11f469e0088db22dd3afbc86997fe86a3"
        )

      } yield
        (history.lookup(space.store.trieStore, space.store.trieBranch, channelsHash) shouldBe None)
    }

  "produce and consume" should "store channel hashes" in withTestSpace { space =>
    val channels = List("ch1", "ch2")
    val patterns = List[Pattern](Wildcard, Wildcard)
    val k        = new StringsCaptor
    val data     = List("datum1", "datum2")

    for {
      _                <- space.consume(channels, patterns, k, false)
      _                <- space.produce(channels(0), data(0), false)
      _                <- space.produce(channels(1), data(1), false)
      expectedConsume  = Consume.create(channels, patterns, k, false)
      _                = expectedConsume.channelsHashes shouldBe channels.map(StableHashProvider.hash(_))
      expectedProduce1 = Produce.create(channels(0), data(0), false)
      _                = expectedProduce1.channelsHash shouldBe StableHashProvider.hash(Seq(channels(0)))
      expectedProduce2 = Produce.create(channels(1), data(1), false)
      _                = expectedProduce2.channelsHash shouldBe StableHashProvider.hash(Seq(channels(1)))
      commEvent        = COMM(expectedConsume, Seq(expectedProduce1, expectedProduce2))
      cp               <- space.createCheckpoint()
      log              = cp.log
      _ = log should contain theSameElementsInOrderAs Seq(
        commEvent,
        expectedProduce2,
        expectedProduce1,
        expectedConsume
      )
    } yield
      (log match {
        case COMM(
              chkCommConsume1: Consume,
              (chkCommProduce1: Produce) :: (chkCommProduce2: Produce) :: Nil
            )
              :: (chkProduce2: Produce) :: (chkProduce1: Produce) :: (chkConsume: Consume) :: Nil =>
          chkCommConsume1.channelsHashes shouldBe expectedConsume.channelsHashes
          chkCommProduce1.channelsHash shouldBe expectedProduce1.channelsHash
          chkCommProduce2.channelsHash shouldBe expectedProduce2.channelsHash

          chkProduce2.channelsHash shouldBe expectedProduce2.channelsHash
          chkProduce1.channelsHash shouldBe expectedProduce1.channelsHash
          chkConsume.channelsHashes shouldBe expectedConsume.channelsHashes

        case _ => fail("unexpected trace log")
      })
  }

}
trait MonadicStorageActionsTests[F[_]]
    extends StorageTestsBase[F, String, Pattern, Nothing, String, StringsCaptor]
    with TestImplicitHelpers
    with GeneratorDrivenPropertyChecks
    with Checkers {

  "consuming with a list of patterns that is a different length than the list of channels" should
    "raise error" in withTestSpace { space =>
    for {
      res <- Sync[F].attempt(
              space.consume(List("ch1", "ch2"), List(Wildcard), new StringsCaptor, persist = false)
            )
      err = res.left.get
      _   = err shouldBe an[IllegalArgumentException]
      _   = err.getMessage shouldBe "channels.length must equal patterns.length"
    } yield (space.store.isEmpty shouldBe true)
  }

  "an install" should "not allow installing after a produce operation" in withTestSpace { space =>
    val channel  = "ch1"
    val datum    = "datum1"
    val key      = List(channel)
    val patterns = List(Wildcard)

    for {
      _   <- space.produce(channel, datum, persist = false)
      res <- Sync[F].attempt(space.install(key, patterns, new StringsCaptor))
      ex  = res.left.get
    } yield (ex.getMessage shouldBe "Installing can be done only on startup")
  }

  "after space was closed" should "reject all store operations" in withTestSpace { space =>
    val channel  = "ch1"
    val key      = List(channel)
    val patterns = List(Wildcard)

    for {
      _ <- space.close()
      //using some nulls here to ensure that exception is thrown even before args check
      e1 <- Sync[F]
             .attempt(space.install(key, patterns, null))
      _ = e1.left.get shouldBe an[RSpaceClosedException]
      e2 <- Sync[F]
             .attempt(space.consume(key, patterns, null, false))
      _ = e2.left.get shouldBe an[RSpaceClosedException]
      e3 <- Sync[F]
             .attempt(space.produce(channel, "test-data", false))
      _ = e3.left.get shouldBe an[RSpaceClosedException]
    } yield ()
  }
}

trait LegacyStorageActionsTests
    extends StorageTestsBase[Id, String, Pattern, Nothing, String, StringsCaptor]
    with TestImplicitHelpers
    with GeneratorDrivenPropertyChecks
    with Checkers {

  "consuming with a list of patterns that is a different length than the list of channels" should
    "throw" in withTestSpace { space =>
    an[IllegalArgumentException] shouldBe thrownBy(
      space.consume(List("ch1", "ch2"), List(Wildcard), new StringsCaptor, persist = false)
    )
    space.store.isEmpty shouldBe true
  }

  "an install" should "not allow installing after a produce operation" in withTestSpace { space =>
    val channel  = "ch1"
    val datum    = "datum1"
    val key      = List(channel)
    val patterns = List(Wildcard)

    val ex = the[RuntimeException] thrownBy {
      space.produce(channel, datum, persist = false)
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
        space.produce(channel, "test data", false)
      )
  }
}

trait IdTests[C, P, A, R, K] extends StorageTestsBase[Id, C, P, A, R, K] {
  override implicit val syncF: Sync[Id]   = coop.rchain.catscontrib.effect.implicits.syncId
  override implicit val monadF: Monad[Id] = syncF
  override implicit val contextShiftF: ContextShift[Id] =
    coop.rchain.rspace.test.contextShiftId

  override def run[RES](f: Id[RES]): RES = f
}

import monix.eval.Task
trait TaskTests[C, P, A, R, K] extends StorageTestsBase[Task, C, P, A, R, K] {
  import coop.rchain.catscontrib.TaskContrib._
  import scala.concurrent.ExecutionContext

  override implicit val syncF: Sync[Task] = new monix.eval.instances.CatsEffectForTask()(
    monix.execution.Scheduler.Implicits.global,
    Task.defaultOptions
  )
  override implicit val monadF: Monad[Task] = syncF
  override implicit val contextShiftF: ContextShift[Task] = new ContextShift[Task] {
    override def shift: Task[Unit] =
      Task.shift
    override def evalOn[B](ec: ExecutionContext)(fa: Task[B]): Task[B] =
      Task.shift(ec).bracket(_ => fa)(_ => Task.shift)
  }

  override def run[RES](f: Task[RES]): RES =
    f.unsafeRunSync(monix.execution.Scheduler.Implicits.global)
}

class InMemoryStoreStorageActionsTests
    extends InMemoryStoreTestsBase[Task]
    with TaskTests[String, Pattern, Nothing, String, StringsCaptor]
    with StorageActionsTests[Task]
    with MonadicStorageActionsTests[Task]

class LMDBStoreStorageActionsTests
    extends LMDBStoreTestsBase[Task]
    with TaskTests[String, Pattern, Nothing, String, StringsCaptor]
    with StorageActionsTests[Task]
    with MonadicStorageActionsTests[Task]

class MixedStoreStorageActionsTests
    extends MixedStoreTestsBase[Task]
    with TaskTests[String, Pattern, Nothing, String, StringsCaptor]
    with StorageActionsTests[Task]
    with MonadicStorageActionsTests[Task]

class LegacyInMemoryStoreStorageActionsTests
    extends InMemoryStoreTestsBase[Id]
    with IdTests[String, Pattern, Nothing, String, StringsCaptor]
    with JoinOperationsTests
    with LegacyStorageActionsTests

class LegacyLMDBStoreActionsTests
    extends LMDBStoreTestsBase[Id]
    with IdTests[String, Pattern, Nothing, String, StringsCaptor]
    with StorageActionsTests[Id]
    with JoinOperationsTests
    with BeforeAndAfterAll
    with LegacyStorageActionsTests

class LegacyMixedStoreActionsTests
    extends MixedStoreTestsBase[Id]
    with IdTests[String, Pattern, Nothing, String, StringsCaptor]
    with StorageActionsTests[Id]
    with JoinOperationsTests
    with BeforeAndAfterAll
    with LegacyStorageActionsTests
