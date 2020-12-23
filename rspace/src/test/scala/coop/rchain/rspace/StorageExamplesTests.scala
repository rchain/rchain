package coop.rchain.rspace

import cats._
import cats.implicits._
import cats.syntax.parallel._
import cats.instances.parallel._
import coop.rchain.rspace.examples.AddressBookExample
import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.util._
import coop.rchain.rspace.test._
import scodec.Codec

import scala.concurrent.ExecutionContext.Implicits.global
import monix.eval.Task
import monix.execution.atomic.AtomicAny

trait StorageExamplesTests[F[_]]
    extends StorageTestsBase[F, Channel, Pattern, Entry, EntriesCaptor] {

  "CORE-365: A joined consume on duplicate channels followed by two produces on that channel" should
    "return a continuation and the produced data" in fixture { (store, _, space) =>
    for {
      r1 <- space
             .consume(
               List(Channel("friends"), Channel("friends")),
               List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
               new EntriesCaptor,
               persist = false
             )
      _             = r1 shouldBe None
      r2            <- space.produce(Channel("friends"), bob, persist = false)
      _             = r2 shouldBe None
      r3            <- space.produce(Channel("friends"), bob, persist = false)
      _             = r3 shouldBe defined
      _             = runK(r3)
      _             = getK(r3).results shouldBe List(List(bob, bob))
      insertActions <- store.changes().map(collectActions[InsertAction])
      _             = insertActions shouldBe empty
    } yield ()
  }

  "CORE-365: Two produces on the same channel followed by a joined consume on duplicates of that channel" should
    "return a continuation and the produced data" in fixture { (store, _, space) =>
    for {
      r1 <- space.produce(Channel("friends"), bob, persist = false)
      _  = r1 shouldBe None
      r2 <- space.produce(Channel("friends"), bob, persist = false)
      _  = r2 shouldBe None
      r3 <- space
             .consume(
               List(Channel("friends"), Channel("friends")),
               List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
               new EntriesCaptor,
               persist = false
             )
      _ = r3 shouldBe defined
      _ = runK(r3)
      _ = getK(r3).results shouldBe List(List(bob, bob))
    } yield (store.changes().map(_.isEmpty shouldBe true))
  }

  "CORE-365: A joined consume on duplicate channels given twice followed by three produces" should
    "return a continuation and the produced data" in fixture { (store, _, space) =>
    for {
      r1 <- space
             .consume(
               List(Channel("colleagues"), Channel("friends"), Channel("friends")),
               List(
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Crystal Lake")
               ),
               new EntriesCaptor,
               persist = false
             )
      _  = r1 shouldBe None
      r2 <- space.produce(Channel("friends"), bob, persist = false)
      _  = r2 shouldBe None
      r3 <- space.produce(Channel("friends"), bob, persist = false)
      _  = r3 shouldBe None
      r4 <- space.produce(Channel("colleagues"), alice, persist = false)
      _  = r4 shouldBe defined
      _  = runK(r4)
      _  = getK(r4).results shouldBe List(List(alice, bob, bob))
    } yield (store.changes().map(_.isEmpty shouldBe true))

  }

  "CORE-365: A joined consume on multiple duplicate channels followed by the requisite produces" should
    "return a continuation and the produced data" in fixture { (store, _, space) =>
    for {
      r1 <- space
             .consume(
               List(
                 Channel("family"),
                 Channel("family"),
                 Channel("family"),
                 Channel("family"),
                 Channel("colleagues"),
                 Channel("colleagues"),
                 Channel("colleagues"),
                 Channel("friends"),
                 Channel("friends")
               ),
               List(
                 CityMatch(city = "Herbert"),
                 CityMatch(city = "Herbert"),
                 CityMatch(city = "Herbert"),
                 CityMatch(city = "Herbert"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Crystal Lake")
               ),
               new EntriesCaptor,
               persist = false
             )
      _   = r1 shouldBe None
      r2  <- space.produce(Channel("friends"), bob, persist = false)
      r3  <- space.produce(Channel("family"), carol, persist = false)
      r4  <- space.produce(Channel("colleagues"), alice, persist = false)
      r5  <- space.produce(Channel("friends"), bob, persist = false)
      r6  <- space.produce(Channel("family"), carol, persist = false)
      r7  <- space.produce(Channel("colleagues"), alice, persist = false)
      r8  <- space.produce(Channel("colleagues"), alice, persist = false)
      r9  <- space.produce(Channel("family"), carol, persist = false)
      r10 <- space.produce(Channel("family"), carol, persist = false)

      _ = r2 shouldBe None
      _ = r3 shouldBe None
      _ = r4 shouldBe None
      _ = r5 shouldBe None
      _ = r6 shouldBe None
      _ = r7 shouldBe None
      _ = r8 shouldBe None
      _ = r9 shouldBe None
      _ = r10 shouldBe defined

      _ = runK(r10)
      _ = getK(r10).results shouldBe List(
        List(carol, carol, carol, carol, alice, alice, alice, bob, bob)
      )

    } yield (store.changes().map(_.isEmpty shouldBe true))
  }

  "CORE-365: Multiple produces on multiple duplicate channels followed by the requisite consume" should
    "return a continuation and the produced data" in fixture { (store, _, space) =>
    for {
      r1 <- space.produce(Channel("friends"), bob, persist = false)
      r2 <- space.produce(Channel("family"), carol, persist = false)
      r3 <- space.produce(Channel("colleagues"), alice, persist = false)
      r4 <- space.produce(Channel("friends"), bob, persist = false)
      r5 <- space.produce(Channel("family"), carol, persist = false)
      r6 <- space.produce(Channel("colleagues"), alice, persist = false)
      r7 <- space.produce(Channel("colleagues"), alice, persist = false)
      r8 <- space.produce(Channel("family"), carol, persist = false)
      r9 <- space.produce(Channel("family"), carol, persist = false)

      _ = r1 shouldBe None
      _ = r2 shouldBe None
      _ = r3 shouldBe None
      _ = r4 shouldBe None
      _ = r5 shouldBe None
      _ = r6 shouldBe None
      _ = r7 shouldBe None
      _ = r8 shouldBe None
      _ = r9 shouldBe None

      r10 <- space
              .consume(
                List(
                  Channel("family"),
                  Channel("family"),
                  Channel("family"),
                  Channel("family"),
                  Channel("colleagues"),
                  Channel("colleagues"),
                  Channel("colleagues"),
                  Channel("friends"),
                  Channel("friends")
                ),
                List(
                  CityMatch(city = "Herbert"),
                  CityMatch(city = "Herbert"),
                  CityMatch(city = "Herbert"),
                  CityMatch(city = "Herbert"),
                  CityMatch(city = "Crystal Lake"),
                  CityMatch(city = "Crystal Lake"),
                  CityMatch(city = "Crystal Lake"),
                  CityMatch(city = "Crystal Lake"),
                  CityMatch(city = "Crystal Lake")
                ),
                new EntriesCaptor,
                persist = false
              )

      _ = r10 shouldBe defined
      _ = runK(r10)
      _ = getK(r10).results shouldBe List(
        List(carol, carol, carol, carol, alice, alice, alice, bob, bob)
      )
    } yield (store.changes().map(_.isEmpty shouldBe true))
  }

  "CORE-365: A joined consume on multiple mixed up duplicate channels followed by the requisite produces" should
    "return a continuation and the produced data" in fixture { (store, _, space) =>
    for {
      r1 <- space
             .consume(
               List(
                 Channel("family"),
                 Channel("colleagues"),
                 Channel("family"),
                 Channel("friends"),
                 Channel("friends"),
                 Channel("family"),
                 Channel("colleagues"),
                 Channel("colleagues"),
                 Channel("family")
               ),
               List(
                 CityMatch(city = "Herbert"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Herbert"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Herbert"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Crystal Lake"),
                 CityMatch(city = "Herbert")
               ),
               new EntriesCaptor,
               persist = false
             )

      _ = r1 shouldBe None

      r2  <- space.produce(Channel("friends"), bob, persist = false)
      r3  <- space.produce(Channel("family"), carol, persist = false)
      r4  <- space.produce(Channel("colleagues"), alice, persist = false)
      r5  <- space.produce(Channel("friends"), bob, persist = false)
      r6  <- space.produce(Channel("family"), carol, persist = false)
      r7  <- space.produce(Channel("colleagues"), alice, persist = false)
      r8  <- space.produce(Channel("colleagues"), alice, persist = false)
      r9  <- space.produce(Channel("family"), carol, persist = false)
      r10 <- space.produce(Channel("family"), carol, persist = false)

      _ = r2 shouldBe None
      _ = r3 shouldBe None
      _ = r4 shouldBe None
      _ = r5 shouldBe None
      _ = r6 shouldBe None
      _ = r7 shouldBe None
      _ = r8 shouldBe None
      _ = r9 shouldBe None
      _ = r10 shouldBe defined

      _ = runK(r10)
      _ = getK(r10).results shouldBe List(
        List(carol, alice, carol, bob, bob, carol, alice, alice, carol)
      )
    } yield (store.changes().map(_.isEmpty shouldBe true))
  }
}

abstract class InMemoryHotStoreStorageExamplesTestsBase[F[_]]
    extends StorageTestsBase[F, Channel, Pattern, Entry, EntriesCaptor] {

  implicit val channelCodec: Codec[Channel] =
    AddressBookExample.implicits.serializeChannel.toSizeHeadCodec
  implicit val patternCodec: Codec[Pattern] =
    AddressBookExample.implicits.serializePattern.toSizeHeadCodec
  implicit val entryCodec: Codec[Entry] = AddressBookExample.implicits.serializeInfo.toSizeHeadCodec
  implicit val entryCaptorCodec: Codec[EntriesCaptor] =
    AddressBookExample.implicits.serializeEntriesCaptor.toSizeHeadCodec

  override def fixture[R](f: (ST, AtST, T) => F[R]): R = {
    val creator: (HR, ST, Branch) => F[(ST, AtST, T)] =
      (hr, ts, b) => {
        val atomicStore = AtomicAny(ts)
        val space =
          new RSpace[F, Channel, Pattern, Entry, EntriesCaptor](hr, atomicStore, b)
        Applicative[F].pure((ts, atomicStore, space))
      }
    setupTestingSpace(creator, f)
  }
}

class InMemoryHotStoreStorageExamplesTests
    extends InMemoryHotStoreStorageExamplesTestsBase[Task]
    with TaskTests[Channel, Pattern, Entry, Entry, EntriesCaptor]
    with StorageExamplesTests[Task] {
  implicit val parF = Task.catsParallel
}
