package coop.rchain.rspace

import cats.effect._
import cats.syntax.all._
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.History._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test._
import coop.rchain.rspace.trace.Consume
import coop.rchain.rspace.util.{getK, runK, unpackOption}
import coop.rchain.shared.Serialize
import monix.eval.Task
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}

import scala.collection.SortedSet

trait StorageActionsTests[F[_]]
    extends StorageTestsBase[F, String, Pattern, String, StringsCaptor]
    with GeneratorDrivenPropertyChecks
    with Checkers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 5, sizeRange = 30)

  implicit val serializeString: Serialize[String] = coop.rchain.rspace.util.stringSerialize

  "produce" should
    "persist a piece of data in the store" in fixture { (store, _, space) =>
    val channel = "ch1"
    val key     = List(channel)

    for {
      r          <- space.produce(key.head, "datum", persist = false)
      data       <- store.getData(channel)
      _          = data shouldBe List(Datum.create(channel, "datum", persist = false))
      cont       <- store.getContinuations(key)
      _          = cont shouldBe Nil
      _          = r shouldBe None
      insertData <- store.changes().map(collectActions[InsertData[String, String]])
      //store is not empty - we have 'A' stored
      _ = insertData.size shouldBe 1
      _ = insertData.map(_.channel) should contain only channel
    } yield ()
  }

  "producing twice on the same channel" should
    "persist two pieces of data in the store" in fixture { (store, _, space) =>
    val channel = "ch1"
    val key     = List(channel)

    for {
      r1  <- space.produce(channel, "datum1", persist = false)
      d1  <- store.getData(channel)
      _   = d1 shouldBe List(Datum.create(channel, "datum1", false))
      wc1 <- store.getContinuations(key)
      _   = wc1 shouldBe Nil
      _   = r1 shouldBe None
      r2  <- space.produce(key.head, "datum2", persist = false)
      d2  <- store.getData(channel)
      _ = d2 should contain theSameElementsAs List(
        Datum.create(key.head, "datum1", false),
        Datum.create(key.head, "datum2", false)
      )
      wc2        <- store.getContinuations(key)
      _          = wc2 shouldBe Nil
      _          = r2 shouldBe None
      insertData <- store.changes().map(collectActions[InsertData[String, String]])
      //store is not empty - we have 2 As stored
      _ = insertData.size shouldBe 1
      _ = insertData.map(_.channel) should contain only channel

    } yield ()
  }

  "consuming on one channel" should
    "persist a continuation in the store" in fixture { (store, _, space) =>
    val channel  = "ch1"
    val key      = List(channel)
    val patterns = List(Wildcard)

    for {
      r  <- space.consume(key, patterns, new StringsCaptor, persist = false)
      d1 <- store.getData(channel)
      _  = d1 shouldBe Nil
      c1 <- store.getContinuations(key)
      _  = c1 should not be empty
      _  = r shouldBe None
      insertContinuations <- store
                              .changes()
                              .map(
                                collectActions[InsertContinuations[String, Pattern, StringsCaptor]]
                              )
      //there is a continuation stored in the storage
      _ = insertContinuations.size shouldBe 1
      _ = insertContinuations.map(_.channels) should contain only key
    } yield ()
  }

  "consuming on three channels" should
    "persist a continuation in the store" in fixture { (store, _, space) =>
    val key      = List("ch1", "ch2", "ch3")
    val patterns = List(Wildcard, Wildcard, Wildcard)

    for {
      r  <- space.consume(key, patterns, new StringsCaptor, persist = false)
      d  <- key.map(store.getData(_)).sequence
      _  = d should contain only Nil
      c1 <- store.getContinuations(key)
      _  = c1 should not be empty
      _  = r shouldBe None
      insertContinuations <- store
                              .changes()
                              .map(
                                collectActions[InsertContinuations[String, Pattern, StringsCaptor]]
                              )

      //continuation is left in the storage
      _ = insertContinuations.size shouldBe 1
    } yield ()
  }

  "producing and then consuming on the same channel" should
    "return the continuation and data" in fixture { (store, _, space) =>
    val channel = "ch1"
    val key     = List(channel)

    for {
      r1 <- space.produce(channel, "datum", persist = false)
      d1 <- store.getData(channel)
      _  = d1 shouldBe List(Datum.create(channel, "datum", false))
      c1 <- store.getContinuations(key)
      _  = c1 shouldBe Nil
      _  = r1 shouldBe None

      r2            <- space.consume(key, List(Wildcard), new StringsCaptor, persist = false)
      d2            <- store.getData(channel)
      _             = d2 shouldBe Nil
      c2            <- store.getContinuations(key)
      _             = c2 shouldBe Nil
      _             = r2 shouldBe defined
      _             = runK(unpackOption(r2))
      _             = getK(r2).continuation.results should contain theSameElementsAs List(List("datum"))
      insertActions <- store.changes().map(collectActions[InsertAction])
      _             = insertActions shouldBe empty
    } yield ()
  }

  "producing and then consuming on the same channel with peek" should
    "return the continuation and data and remove the peeked data" in fixture { (store, _, space) =>
    val channel = "ch1"
    val key     = List(channel)

    for {
      r1 <- space.produce(channel, "datum", persist = false)
      d1 <- store.getData(channel)
      _  = d1 shouldBe List(Datum.create(channel, "datum", false))
      c1 <- store.getContinuations(key)
      _  = c1 shouldBe Nil
      _  = r1 shouldBe None

      r2 <- space.consume(
             key,
             List(Wildcard),
             new StringsCaptor,
             persist = false,
             peeks = SortedSet(0)
           )
      d2            <- store.getData(channel)
      _             = d2 shouldBe Nil
      c2            <- store.getContinuations(key)
      _             = c2 shouldBe Nil
      _             = r2 shouldBe defined
      _             = runK(unpackOption(r2))
      _             = getK(r2).continuation.results should contain theSameElementsAs List(List("datum"))
      insertActions <- store.changes().map(collectActions[InsertAction])
      _             = insertActions shouldBe empty
    } yield ()
  }

  "consuming and then producing on the same channel with peek" should
    "return the continuation and data and remove the peeked data" in fixture { (store, _, space) =>
    val channel = "ch1"
    val key     = List(channel)

    for {
      r1 <- space.consume(
             key,
             List(Wildcard),
             new StringsCaptor,
             persist = false,
             peeks = SortedSet(0)
           )
      _  = r1 shouldBe None
      c1 <- store.getContinuations(key)
      _  = c1 should have size 1

      r2            <- space.produce(channel, "datum", persist = false)
      d1            <- store.getData(channel)
      _             = d1 shouldBe Nil
      c2            <- store.getContinuations(key)
      _             = c2 shouldBe Nil
      _             = r2 shouldBe defined
      _             = runK(unpackOption(r2))
      _             = getK(r2).continuation.results should contain theSameElementsAs List(List("datum"))
      insertActions <- store.changes().map(collectActions[InsertAction])
      _             = insertActions should have size 0
    } yield ()
  }
  "consuming and then producing on the same channel with persistent flag" should
    "return the continuation and data and not insert the persistent data" in fixture {
    (store, _, space) =>
      val channel = "ch1"
      val key     = List(channel)

      for {
        r1 <- space.consume(
               key,
               List(Wildcard),
               new StringsCaptor,
               persist = false
             )
        _  = r1 shouldBe None
        c1 <- store.getContinuations(key)
        _  = c1 should have size 1

        r2            <- space.produce(channel, "datum", persist = true)
        d1            <- store.getData(channel)
        _             = d1 shouldBe Nil
        c2            <- store.getContinuations(key)
        _             = c2 shouldBe Nil
        _             = r2 shouldBe defined
        _             = runK(unpackOption(r2))
        _             = getK(r2).continuation.results should contain theSameElementsAs List(List("datum"))
        insertActions <- store.changes().map(collectActions[InsertAction])
        _             = insertActions should have size 0
      } yield ()
  }

  "producing three times then doing consuming three times" should "work" in fixture {
    (store, _, space) =>
      for {
        r1 <- space.produce("ch1", "datum1", persist = false)
        r2 <- space.produce("ch1", "datum2", persist = false)
        r3 <- space.produce("ch1", "datum3", persist = false)
        _  = r1 shouldBe None
        _  = r2 shouldBe None
        _  = r3 shouldBe None
        r4 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
        _  = runK(unpackOption(r4))
        _ = getK(r4).continuation.results should contain oneOf (List("datum1"), List("datum2"), List(
          "datum3"
        ))
        r5 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
        _  = runK(unpackOption(r5))
        _ = getK(r5).continuation.results should contain oneOf (List("datum1"), List("datum2"), List(
          "datum3"
        ))
        r6 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
        _  = runK(unpackOption(r6))
        _ = getK(r6).continuation.results should contain oneOf (List("datum1"), List("datum2"), List(
          "datum3"
        ))
        insertActions <- store.changes().map(collectActions[InsertAction])
        _             = insertActions shouldBe empty
      } yield ()
  }

  "producing on channel, consuming on that channel and another, and then producing on the other channel" should
    "return a continuation and all the data" in fixture { (store, _, space) =>
    val produceKey1 = List("ch1")
    val produceKey2 = List("ch2")

    val consumeKey     = List("ch1", "ch2")
    val consumePattern = List(Wildcard, Wildcard)

    for {
      r1 <- space.produce(produceKey1.head, "datum1", persist = false)
      d1 <- store.getData(produceKey1.head)
      _ = d1 shouldBe List(
        Datum.create(produceKey1.head, "datum1", persist = false)
      )
      c1 <- store.getContinuations(produceKey1)
      _  = c1 shouldBe Nil
      _  = r1 shouldBe None

      r2 <- space.consume(consumeKey, consumePattern, new StringsCaptor, persist = false)
      d2 <- store.getData(produceKey1.head)
      _ = d2 shouldBe List(
        Datum.create(produceKey1.head, "datum1", persist = false)
      )
      _ <- store.getContinuations(produceKey1).map(_ shouldBe Nil)
      _ <- store.getData(produceKey2.head).map(_ shouldBe Nil)
      _ <- store.getContinuations(consumeKey).map(_ should not be empty)
      _ = r2 shouldBe None

      r3 <- space.produce(produceKey2.head, "datum2", persist = false)
      _  <- store.getContinuations(consumeKey).map(_ shouldBe Nil)
      _  <- store.getData(produceKey1.head).map(_ shouldBe Nil)
      _  <- store.getData(produceKey2.head).map(_ shouldBe Nil)
      _  = r3 shouldBe defined

      _ = runK(unpackOption(r3))
      _ = getK(r3).continuation.results should contain theSameElementsAs List(
        List("datum1", "datum2")
      )
      insertActions <- store.changes().map(collectActions[InsertAction])
      _             = insertActions shouldBe empty
    } yield ()
  }

  "producing on three different channels and then consuming once on all three" should
    "return the continuation and all the data" in fixture { (store, _, space) =>
    val produceKey1 = List("ch1")
    val produceKey2 = List("ch2")
    val produceKey3 = List("ch3")
    val consumeKey  = List("ch1", "ch2", "ch3")
    val patterns    = List(Wildcard, Wildcard, Wildcard)

    for {
      r1 <- space.produce(produceKey1.head, "datum1", persist = false)
      d1 <- store.getData(produceKey1.head)
      _ = d1 shouldBe List(
        Datum.create(produceKey1.head, "datum1", false)
      )
      c1 <- store.getContinuations(produceKey1)
      _  = c1 shouldBe Nil
      _  = r1 shouldBe None

      r2 <- space.produce(produceKey2.head, "datum2", persist = false)
      d2 <- store.getData(produceKey2.head)
      _ = d2 shouldBe List(
        Datum.create(produceKey2.head, "datum2", false)
      )
      c2 <- store.getContinuations(produceKey2)
      _  = c2 shouldBe Nil
      _  = r2 shouldBe None

      r3 <- space.produce(produceKey3.head, "datum3", persist = false)
      d3 <- store.getData(produceKey3.head)
      _ = d3 shouldBe List(
        Datum.create(produceKey3.head, "datum3", false)
      )
      c3 <- store.getContinuations(produceKey3)
      _  = c3 shouldBe Nil
      _  = r3 shouldBe None

      r4 <- space.consume(List("ch1", "ch2", "ch3"), patterns, new StringsCaptor, persist = false)
      d4 <- consumeKey.map(key => store.getData(key)).sequence
      _  = d4 should contain only Nil
      c4 <- store.getContinuations(consumeKey)
      _  = c4 shouldBe Nil
      _  = r4 shouldBe defined

      _ = runK(unpackOption(r4))
      _ = getK(r4).continuation.results should contain theSameElementsAs List(
        List("datum1", "datum2", "datum3")
      )
      insertActions <- store.changes().map(collectActions[InsertAction])
      _             = insertActions shouldBe empty
    } yield ()
  }

  "producing three times on the same channel then consuming three times on the same channel" should
    "return three pairs of continuations and data" in fixture { (store, _, space) =>
    val captor = new StringsCaptor

    val key = List("ch1")

    for {
      r1 <- space.produce(key.head, "datum1", persist = false)
      r2 <- space.produce(key.head, "datum2", persist = false)
      r3 <- space.produce(key.head, "datum3", persist = false)
      _  = r1 shouldBe None
      _  = r2 shouldBe None
      _  = r3 shouldBe None
      r4 <- space.consume(key, List(Wildcard), captor, persist = false)
      r5 <- space.consume(key, List(Wildcard), captor, persist = false)
      r6 <- space.consume(key, List(Wildcard), captor, persist = false)
      _  <- store.getContinuations(key).map(_ shouldBe Nil)

      continuations = List(r4, r5, r6)
      _             = continuations.forall(_.isDefined) shouldBe true
      _ = continuations
        .map(unpackOption)
        .foreach(runK)
      _ = captor.results should contain theSameElementsAs List(
        List("datum3"),
        List("datum2"),
        List("datum1")
      )
      insertActions <- store.changes().map(collectActions[InsertAction])
      _             = insertActions shouldBe empty
    } yield ()
  }

  "consuming three times on the same channel, then producing three times on that channel" should
    "return three continuations, each paired with distinct pieces of data" in fixture {
    (store, _, space) =>
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
          .map(unpackOption)
          .foreach(runK)

        _ = getK(r1).continuation.results should contain oneOf (List("datum1"), List("datum2"), List(
          "datum3"
        ))
        _ = getK(r2).continuation.results should contain oneOf (List("datum1"), List("datum2"), List(
          "datum3"
        ))
        _ = getK(r3).continuation.results should contain oneOf (List("datum1"), List("datum2"), List(
          "datum3"
        ))
        _             = getK(r1).continuation.results shouldNot contain theSameElementsAs getK(r2).continuation.results
        _             = getK(r1).continuation.results shouldNot contain theSameElementsAs getK(r3).continuation.results
        _             = getK(r2).continuation.results shouldNot contain theSameElementsAs getK(r3).continuation.results
        insertActions <- store.changes().map(collectActions[InsertAction])
        _             = insertActions shouldBe empty
      } yield ()
  }

  "consuming three times on the same channel with non-trivial matches, then producing three times on that channel" should
    "return three continuations, each paired with matching data" in fixture { (store, _, space) =>
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
        .map(unpackOption)
        .foreach(runK)

      _             = getK(r1).continuation.results shouldBe List(List("datum1"))
      _             = getK(r2).continuation.results shouldBe List(List("datum2"))
      _             = getK(r3).continuation.results shouldBe List(List("datum3"))
      insertActions <- store.changes().map(collectActions[InsertAction])
    } yield (insertActions shouldBe empty)
  }

  "consuming on two channels, producing on one, then producing on the other" should
    "return a continuation with both pieces of data" in fixture { (store, _, space) =>
    for {
      r1 <- space.consume(
             List("ch1", "ch2"),
             List(Wildcard, Wildcard),
             new StringsCaptor,
             persist = false
           )
      r2 <- space.produce("ch1", "datum1", persist = false)
      r3 <- space.produce("ch2", "datum2", persist = false)

      _ = r1 shouldBe None
      _ = r2 shouldBe None
      _ = r3 shouldBe defined

      _ = runK(unpackOption(r3))

      _ = getK(r3).continuation.results should contain theSameElementsAs List(
        List("datum1", "datum2")
      )
      insertActions <- store.changes().map(collectActions[InsertAction])
    } yield (insertActions shouldBe empty)

  }

  "A joined consume with the same channel given twice followed by a produce" should
    "not raise any errors (CORE-365)" in fixture { (store, _, space) =>
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

      _ = r1 shouldBe None
      _ = r2 shouldBe None
      _ = r3 shouldBe defined

      _             = runK(unpackOption(r3))
      _             = getK(r3).continuation.results shouldBe List(List("datum1", "datum1"))
      insertActions <- store.changes().map(collectActions[InsertAction])
    } yield (insertActions shouldBe empty)
  }

  "consuming twice on the same channels with different patterns, and then producing on those channels" should
    "return continuations with the expected data" in fixture { (store, _, space) =>
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

      _ = r1 shouldBe None
      _ = r2 shouldBe None
      _ = r3 shouldBe None
      _ = r4 shouldBe defined
      _ = r5 shouldBe None
      _ = r6 shouldBe defined

      _ = List(r4, r6)
        .map(unpackOption)
        .foreach(runK)

      _ = getK(r4).continuation.results should contain theSameElementsAs List(
        List("datum3", "datum4")
      )
      _ = getK(r6).continuation.results should contain theSameElementsAs List(
        List("datum1", "datum2")
      )
      insertActions <- store.changes().map(collectActions[InsertAction])
    } yield (insertActions shouldBe empty)

  }

  "consuming and producing with non-trivial matches" should "work" in fixture { (store, _, space) =>
    for {
      r1 <- space.consume(
             List("ch1", "ch2"),
             List(Wildcard, StringMatch("datum1")),
             new StringsCaptor,
             persist = false
           )
      r2 <- space.produce("ch1", "datum1", persist = false)

      _ = r1 shouldBe None
      _ = r2 shouldBe None

      d1 <- store.getData("ch2")
      _  = d1 shouldBe Nil
      d2 <- store.getData("ch1")
      _  = d2 shouldBe List(Datum.create("ch1", "datum1", false))

      c1 <- store.getContinuations(List("ch1", "ch2"))
      _  = c1 should not be empty
      j1 <- store.getJoins("ch1")
      _  = j1 shouldBe List(List("ch1", "ch2"))
      j2 <- store.getJoins("ch2")
      _  = j2 shouldBe List(List("ch1", "ch2"))

      insertActions <- store.changes().map(collectActions[InsertAction])
    } yield (insertActions should not be empty)

  }

  "consuming twice and producing twice with non-trivial matches" should
    "work" in fixture { (store, _, space) =>
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
        .map(unpackOption)
        .foreach(runK)

      d1 <- store.getData("ch1")
      _  = d1 shouldBe Nil
      d2 <- store.getData("ch2")
      _  = d2 shouldBe Nil

      _             = getK(r3).continuation.results should contain theSameElementsAs List(List("datum1"))
      _             = getK(r4).continuation.results should contain theSameElementsAs List(List("datum2"))
      insertActions <- store.changes().map(collectActions[InsertAction])
    } yield (insertActions shouldBe empty)
  }

  "consuming on two channels, consuming on one of those channels, and then producing on both of those channels separately" should
    "return a continuation paired with one piece of data" in
    fixture { (store, _, space) =>
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

        _ <- store.getContinuations(List("ch1", "ch2")).map(_ should not be empty)
        _ <- store.getContinuations(List("ch1")).map(_ shouldBe Nil)
        _ <- store.getContinuations(List("ch2")).map(_ shouldBe Nil)
        _ <- store.getData("ch1").map(_ shouldBe Nil)
        _ <- store.getData("ch2").map(_ shouldBe List(Datum.create("ch2", "datum2", false)))

        _ = r3 shouldBe defined
        _ = r4 shouldBe None

        _ = runK(unpackOption(r3))

        _ = getK(r3).continuation.results should contain theSameElementsAs List(List("datum1"))
        //ensure that joins are cleaned-up after all
        _             <- store.getJoins("ch1").map(_ shouldBe List(List("ch1", "ch2")))
        _             <- store.getJoins("ch2").map(_ shouldBe List(List("ch1", "ch2")))
        insertActions <- store.changes().map(collectActions[InsertAction])
      } yield (insertActions should not be empty)
    }

  /* Persist tests */

  "producing and then doing a persistent consume on the same channel" should
    "return the continuation and data" in fixture { (store, _, space) =>
    val key = List("ch1")

    for {
      r1 <- space.produce(key.head, "datum", persist = false)
      _  <- store.getData(key.head).map(_ shouldBe List(Datum.create(key.head, "datum", false)))
      _  <- store.getContinuations(key).map(_ shouldBe Nil)
      _  = r1 shouldBe None

      // Data exists so the write will not "stick"
      r2            <- space.consume(key, List(Wildcard), new StringsCaptor, persist = true)
      _             = r2 shouldBe defined
      insertActions <- store.changes().map(collectActions[InsertAction])
      _             = insertActions shouldBe empty

      _ = runK(unpackOption(r2))

      _ = getK(r2).continuation.results should contain theSameElementsAs List(List("datum"))

      // the data has been consumed, so the write will "stick"
      r3 <- space.consume(key, List(Wildcard), new StringsCaptor, persist = true)
      _  <- store.getData(key.head).map(_ shouldBe Nil)
      _  <- store.getContinuations(key).map(_ should not be empty)
    } yield (r3 shouldBe None)
  }

  "producing, doing a persistent consume, and producing again on the same channel" should
    "return the continuation for the first produce, and then the second produce" in fixture {
    (store, _, space) =>
      val key = List("ch1")

      for {
        r1 <- space.produce(key.head, "datum1", persist = false)
        _  <- store.getData(key.head).map(_ shouldBe List(Datum.create(key.head, "datum1", false)))
        _  <- store.getContinuations(key).map(_ shouldBe Nil)
        _  = r1 shouldBe None

        // Matching data exists so the write will not "stick"
        r2            <- space.consume(key, List(Wildcard), new StringsCaptor, persist = true)
        _             = r2 shouldBe defined
        insertActions <- store.changes().map(collectActions[InsertAction])
        _             = insertActions shouldBe empty

        _ = runK(unpackOption(r2))
        _ = getK(r2).continuation.results should contain theSameElementsAs List(List("datum1"))

        // All matching data has been consumed, so the write will "stick"
        r3 <- space.consume(key, List(Wildcard), new StringsCaptor, persist = true)
        _  = r3 shouldBe None

        _ <- store.getData(key.head).map(_ shouldBe Nil)
        _ <- store.getContinuations(key).map(_ should not be empty)

        r4 <- space.produce(key.head, "datum2", persist = false)
        _  = r4 shouldBe defined
        _  <- store.getData(key.head).map(_ shouldBe Nil)
        _  <- store.getContinuations(key).map(_ should not be empty)

        _ = runK(unpackOption(r4))
      } yield getK(r4).continuation.results should contain theSameElementsAs List(List("datum2"))
  }

  "doing a persistent consume and producing multiple times" should "work" in fixture {
    (store, _, space) =>
      for {
        r1 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)
        _  <- store.getData("ch1").map(_ shouldBe Nil)
        _  <- store.getContinuations(List("ch1")).map(_ should not be empty)
        _  = r1 shouldBe None

        r2 <- space.produce("ch1", "datum1", persist = false)
        _  <- store.getData("ch1").map(_ shouldBe Nil)
        _  <- store.getContinuations(List("ch1")).map(_ should not be empty)
        _  = r2 shouldBe defined

        _ = runK(unpackOption(r2))
        _ = getK(r2).continuation.results should contain theSameElementsAs List(List("datum1"))

        r3 <- space.produce("ch1", "datum2", persist = false)
        _  <- store.getData("ch1").map(_ shouldBe Nil)
        _  <- store.getContinuations(List("ch1")).map(_ should not be empty)

        _ = r3 shouldBe defined
        _ = runK(unpackOption(r3))
      } yield getK(r3).continuation.results should contain theSameElementsAs List(
        List("datum1"),
        List("datum2")
      )
  }

  "consuming and doing a persistient produce" should "work" in fixture { (store, _, space) =>
    for {
      r1 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      _  = r1 shouldBe None

      // A matching continuation exists so the write will not "stick"
      r2            <- space.produce("ch1", "datum1", persist = true)
      _             = r2 shouldBe defined
      insertActions <- store.changes().map(collectActions[InsertAction])
      _             = insertActions shouldBe empty

      _ = runK(unpackOption(r2))
      _ = getK(r2).continuation.results should contain theSameElementsAs List(List("datum1"))

      // All matching continuations have been produced, so the write will "stick"
      r3 <- space.produce("ch1", "datum1", persist = true)
      _  = r3 shouldBe None
      _  <- store.getData("ch1").map(_ shouldBe List(Datum.create("ch1", "datum1", true)))
      _  <- store.getContinuations(List("ch1")) map (_ shouldBe Nil)
    } yield ()
  }

  "consuming, doing a persistient produce, and consuming again" should "work" in fixture {
    (store, _, space) =>
      for {
        r1 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)

        _ = r1 shouldBe None

        // A matching continuation exists so the write will not "stick"
        r2            <- space.produce("ch1", "datum1", persist = true)
        _             = r2 shouldBe defined
        insertActions <- store.changes().map(collectActions[InsertAction])
        _             = insertActions shouldBe empty

        _ = runK(unpackOption(r2))
        _ = getK(r2).continuation.results should contain theSameElementsAs List(List("datum1"))

        // All matching continuations have been produced, so the write will "stick"
        r3 <- space.produce("ch1", "datum1", persist = true)
        _  = r3 shouldBe None
        _  <- store.getData("ch1") map (_ shouldBe List(Datum.create("ch1", "datum1", true)))
        _  <- store.getContinuations(List("ch1")) map (_ shouldBe Nil)

        r4 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
        _  = r4 shouldBe defined
        _  <- store.getData("ch1") map (_ shouldBe List(Datum.create("ch1", "datum1", true)))
        _  <- store.getContinuations(List("ch1")) map (_ shouldBe Nil)

        _ = runK(unpackOption(r4))
      } yield getK(r4).continuation.results should contain theSameElementsAs List(List("datum1"))
  }

  "doing a persistent produce and consuming twice" should "work" in fixture { (store, _, space) =>
    for {
      r1 <- space.produce("ch1", "datum1", persist = true)
      _  <- store.getData("ch1") map (_ shouldBe List(Datum.create("ch1", "datum1", true)))
      _  <- store.getContinuations(List("ch1")) map (_ shouldBe Nil)
      _  = r1 shouldBe None

      r2 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      _  <- store.getData("ch1") map (_ shouldBe List(Datum.create("ch1", "datum1", true)))
      _  <- store.getContinuations(List("ch1")) map (_ shouldBe Nil)
      _  = r2 shouldBe defined

      _ = runK(unpackOption(r2))
      _ = getK(r2).continuation.results should contain theSameElementsAs List(List("datum1"))

      r3 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = false)
      _  <- store.getData("ch1") map (_ shouldBe List(Datum.create("ch1", "datum1", true)))
      _  <- store.getContinuations(List("ch1")) map (_ shouldBe Nil)

      _ = r3 shouldBe defined
      _ = runK(unpackOption(r3))
    } yield getK(r3).continuation.results should contain theSameElementsAs List(List("datum1"))
  }

  "producing three times and doing a persistent consume" should "work" in fixture {
    (store, _, space) =>
      for {
        r1 <- space.produce("ch1", "datum1", persist = false)
        r2 <- space.produce("ch1", "datum2", persist = false)
        r3 <- space.produce("ch1", "datum3", persist = false)

        _ = r1 shouldBe None
        _ = r2 shouldBe None
        _ = r3 shouldBe None

        // Matching data exists so the write will not "stick"
        r4 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)
        _ <- store.getData("ch1") map (_ should contain atLeastOneOf (
              Datum.create("ch1", "datum1", false),
              Datum.create("ch1", "datum2", false),
              Datum.create("ch1", "datum3", false)
            ))
        _ <- store.getContinuations(List("ch1")) map (_ shouldBe Nil)
        _ = r4 shouldBe defined

        _ = runK(unpackOption(r4))
        _ = getK(r4).continuation.results should contain oneOf (List("datum1"), List("datum2"), List(
          "datum3"
        ))

        // Matching data exists so the write will not "stick"
        r5 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)
        _ <- store.getData("ch1") map (_ should contain oneOf (
              Datum.create("ch1", "datum1", false),
              Datum.create("ch1", "datum2", false),
              Datum.create("ch1", "datum3", false)
            ))
        _ <- store.getContinuations(List("ch1")) map (_ shouldBe Nil)
        _ = r5 shouldBe defined

        _ = runK(unpackOption(r5))
        _ = getK(r5).continuation.results should contain oneOf (List("datum1"), List("datum2"), List(
          "datum3"
        ))

        // Matching data exists so the write will not "stick"
        r6            <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)
        insertActions <- store.changes().map(collectActions[InsertAction])
        _             = insertActions shouldBe empty
        _             = r6 shouldBe defined

        _ = runK(unpackOption(r6))
        _ = getK(r6).continuation.results should contain oneOf (List("datum1"), List("datum2"), List(
          "datum3"
        ))

        // All matching data has been consumed, so the write will "stick"
        r7 <- space.consume(List("ch1"), List(Wildcard), new StringsCaptor, persist = true)
        _  <- store.getData("ch1") map (_ shouldBe Nil)
        _  <- store.getContinuations(List("ch1")) map (_ should not be empty)
      } yield (r7 shouldBe None)

  }

  "A persistent produce" should "be available for multiple matches (CORE-633)" in fixture {
    (store, _, space) =>
      val channel = "chan"

      for {
        r1 <- space.produce(channel, data = "datum", persist = true)
        _  = r1 shouldBe None

        r2 <- space.consume(
               List(channel, channel),
               List(Wildcard, Wildcard),
               new StringsCaptor,
               persist = false
             )
        _ = r2 shouldBe defined
        _ = runK(unpackOption(r2))
      } yield getK(r2).continuation.results should contain(List("datum", "datum"))
  }

  "clear" should "reset to the same hash on multiple runs" in fixture { (store, _, space) =>
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
      _ <- space.clear()
      //the checkpointing mechanism should not interfere with the empty root
      checkpoint2 <- space.createCheckpoint()
      _           = checkpoint2.log shouldBe empty
    } yield checkpoint2.root shouldBe emptyCheckpoint.root
  }

  "createCheckpoint on an empty store" should "return the expected hash" in fixture {
    (_, _, space) =>
      space.createCheckpoint().map(_.root shouldBe emptyRootHash)
  }

  "createCheckpoint" should "clear the store contents" in fixture { (_, storeAtom, space) =>
    val key1     = List("ch1")
    val patterns = List(Wildcard)

    for {
      _                  <- space.consume(key1, patterns, new StringsCaptor, persist = false)
      _                  <- space.createCheckpoint()
      store0             = storeAtom.get()
      checkpoint0Changes <- store0.changes()
      _                  = checkpoint0Changes.length shouldBe 0
    } yield ()
  }

  "reset" should "change the state of the store, and reset the trie updates log" in fixture {
    (_, storeAtom, space) =>
      val key      = List("ch1")
      val patterns = List(Wildcard)

      for {
        checkpoint0          <- space.createCheckpoint()
        r                    <- space.consume(key, patterns, new StringsCaptor, persist = false)
        _                    = r shouldBe None
        postCheckpoint0Store = storeAtom.get()
        checkpoint0Changes <- postCheckpoint0Store
                               .changes()
                               .map(
                                 collectActions[InsertContinuations[String, Pattern, StringsCaptor]]
                               )
        _ = checkpoint0Changes.isEmpty shouldBe false
        _ = checkpoint0Changes.length shouldBe 1

        _              <- space.reset(checkpoint0.root)
        postResetStore = storeAtom.get()
        resetChanges   <- postResetStore.changes()
        _              = resetChanges.isEmpty shouldBe true
        _              = resetChanges.length shouldBe 0

        checkpoint1 <- space.createCheckpoint()
      } yield checkpoint1.log shouldBe empty
  }

  "consume and produce a match and then createCheckpoint" should "result in an empty TrieStore" in
    fixture { (_, _, space) =>
      val channels = List("ch1")

      for {
        checkpointInit <- space.createCheckpoint()
        _              = checkpointInit.root shouldBe emptyRootHash
        r1             <- space.consume(channels, List(Wildcard), new StringsCaptor, persist = false)
        _              = r1 shouldBe None
        r2             <- space.produce(channels.head, "datum", persist = false)
        _              = r2 shouldBe defined
        checkpoint     <- space.createCheckpoint()
        _              = checkpoint.root shouldBe emptyRootHash
      } yield ()
    }

  "produce a bunch and then createCheckpoint, consume on same channels" should "result in checkpoint pointing at empty state" in
    forAll(minSuccessful(50)) { data: List[String] =>
      fixture { (_, _, space) =>
        def consume(channel: String) = space.consume(
          channel :: Nil,
          List(Wildcard),
          new StringsCaptor,
          persist = false
        )
        for {
          _           <- data.traverse(channel => space.produce(channel, "data", false))
          checkpoint1 <- space.createCheckpoint()
          _           <- data.traverse(channel => consume(channel).map(_ shouldBe defined))
          checkpoint2 <- space.createCheckpoint()
          _           <- data.traverse(channel => consume(channel).map(_ shouldBe empty))
          _           = checkpoint2.root shouldBe emptyRootHash
          _           <- space.reset(checkpoint1.root)
          _           <- data.traverse(channel => consume(channel).map(_ shouldBe defined))
          checkpoint3 <- space.createCheckpoint()
          _           = checkpoint3.root shouldBe emptyRootHash
        } yield ()
      }
    }

  "an install" should "not allow installing after a produce operation" in fixture { (_, _, space) =>
    val channel  = "ch1"
    val datum    = "datum1"
    val key      = List(channel)
    val patterns = List(Wildcard)

    for {
      _   <- space.produce(channel, datum, persist = false)
      res <- Sync[F].attempt(space.install(key, patterns, new StringsCaptor))
      ex  = res.left.get
    } yield ex.getMessage shouldBe "Installing can be done only on startup"
  }

  "consuming with a list of patterns that is a different length than the list of channels" should
    "raise error" in fixture { (store, _, space) =>
    for {
      res <- Sync[F].attempt(
              space.consume(List("ch1", "ch2"), List(Wildcard), new StringsCaptor, persist = false)
            )
      err           = res.left.get
      _             = err shouldBe an[IllegalArgumentException]
      _             = err.getMessage shouldBe "channels.length must equal patterns.length"
      insertActions <- store.changes().map(collectActions[InsertAction])
    } yield insertActions shouldBe empty
  }

  "createSoftCheckpoint" should "capture the current state of the store" in fixture {
    (_, _, space) =>
      val channel      = "ch1"
      val channels     = List(channel)
      val patterns     = List(Wildcard)
      val continuation = new StringsCaptor

      val expectedContinuation = Seq(
        WaitingContinuation
          .create[String, Pattern, StringsCaptor](
            channels,
            patterns,
            continuation,
            persist = false,
            SortedSet.empty
          )
      )

      for {
        // do an operation
        _ <- space.consume(channels, patterns, continuation, persist = false)
        // create a soft checkpoint
        s <- space.createSoftCheckpoint()
        // assert that the snapshot contains the continuation
        _ = s.cacheSnapshot.cache.continuations.values should contain only expectedContinuation
        // consume again
        _ <- space.consume(channels, patterns, continuation, persist = false)
        // assert that the snapshot contains only the first continuation
        _ = s.cacheSnapshot.cache.continuations.values should contain only expectedContinuation
      } yield ()
  }

  it should "create checkpoints which have separate state" in fixture { (_, _, space) =>
    val channel      = "ch1"
    val channels     = List(channel)
    val datum        = "datum1"
    val patterns     = List(Wildcard)
    val continuation = new StringsCaptor

    val expectedContinuation = WaitingContinuation
      .create[String, Pattern, StringsCaptor](
        channels,
        patterns,
        continuation,
        persist = false,
        SortedSet.empty
      )

    for {
      // do an operation
      _ <- space.consume(channels, patterns, continuation, persist = false)
      // create a soft checkpoint
      s1 <- space.createSoftCheckpoint()
      // assert that the snapshot contains the continuation
      _ = s1.cacheSnapshot.cache.continuations.values should contain only Seq(expectedContinuation)
      // produce thus removing the continuation
      _  <- space.produce(channel, datum, persist = false)
      s2 <- space.createSoftCheckpoint()
      // assert that the first snapshot still contains the first continuation
      _ = s1.cacheSnapshot.cache.continuations.values should contain only Seq(expectedContinuation)
      _ = s2.cacheSnapshot.cache.continuations(channels) shouldBe empty
    } yield ()
  }

  it should "clear the event log" in fixture { (_, _, space) =>
    val channel      = "ch1"
    val channels     = List(channel)
    val patterns     = List(Wildcard)
    val continuation = new StringsCaptor

    for {
      // do an operation
      _ <- space.consume(channels, patterns, continuation, persist = false)
      // create a soft checkpoint
      s1 <- space.createSoftCheckpoint()
      // the log contains the above operation
      _ = s1.log should contain only
        Consume[String, Pattern, StringsCaptor](
          channels,
          patterns,
          continuation,
          persistent = false
        )
      s2 <- space.createSoftCheckpoint()
      // assert that the event log has been cleared
      _ = s2.log shouldBe empty
    } yield ()
  }

  "revertToSoftCheckpoint" should "revert the state of the store to the given checkpoint" in fixture {
    (_, storeAtom, space) =>
      val channel      = "ch1"
      val channels     = List(channel)
      val patterns     = List(Wildcard)
      val continuation = new StringsCaptor

      for {
        // create an initial soft checkpoint
        s1 <- space.createSoftCheckpoint()
        // do an operation
        _ <- space.consume(channels, patterns, continuation, persist = false)
        changes <- storeAtom
                    .get()
                    .changes()
                    .map(
                      collectActions[InsertContinuations[String, Pattern, StringsCaptor]]
                    )
        // the operation should be on the list of changes
        _ = changes should not be empty
        _ <- space.revertToSoftCheckpoint(s1)
        changes <- storeAtom
                    .get()
                    .changes()
                    .map(
                      collectActions[InsertContinuations[String, Pattern, StringsCaptor]]
                    )
        // after reverting to the initial soft checkpoint the operation is no longer present in the hot store
        _ = changes shouldBe empty
      } yield ()
  }

  it should "inject the event log" in fixture { (_, storeAtom, space) =>
    val channel      = "ch1"
    val channels     = List(channel)
    val patterns     = List(Wildcard)
    val continuation = new StringsCaptor

    for {
      // do an operation
      _ <- space.consume(channels, patterns, continuation, persist = false)
      // create a soft checkpoint
      s1 <- space.createSoftCheckpoint()
      // do some other operation
      _  <- space.consume(channels, patterns, continuation, persist = true)
      s2 <- space.createSoftCheckpoint()
      _  = s2.log should not be s1.log
      _  <- space.revertToSoftCheckpoint(s1)
      s3 <- space.createSoftCheckpoint()
      _  = s3.log shouldBe s1.log
    } yield ()
  }
}

class InMemoryHotStoreStorageActionsTests
    extends InMemoryHotStoreTestsBase[Task]
    with TaskTests[String, Pattern, Nothing, String, StringsCaptor]
    with StorageActionsTests[Task]
    with StorageTestsBase[Task, String, Pattern, String, StringsCaptor] {
  implicit val parF = Task.catsParallel
}
