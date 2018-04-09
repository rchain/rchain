package coop.rchain.rspace

import java.nio.file.{Files, Path}

import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.extended._
import coop.rchain.rspace.test.InMemoryStore
import org.scalatest.BeforeAndAfterAll

trait StorageExamplesTests extends StorageTestsBase[Channel, Pattern, Entry, EntriesCaptor] {

  "CORE-365: A joined consume on duplicate channels followed by two produces on that channel" should
    "return a continuation and the produced data" in withTestStore { store =>
    val r1 = consume(
      store,
      List(Channel("friends"), Channel("friends")),
      List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
      new EntriesCaptor,
      persist = false
    )

    r1 shouldBe None

    val r2 = produce(store, Channel("friends"), bob, persist = false)

    r2 shouldBe None

    val r3 = produce(store, Channel("friends"), bob, persist = false)

    r3 shouldBe defined

    runK(r3)
    getK(r3).results shouldBe List(List(bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: Two produces on the same channel followed by a joined consume on duplicates of that channel" should
    "return a continuation and the produced data" in withTestStore { store =>
    val r1 = produce(store, Channel("friends"), bob, persist = false)

    r1 shouldBe None

    val r2 = produce(store, Channel("friends"), bob, persist = false)

    r2 shouldBe None

    val r3 = consume(
      store,
      List(Channel("friends"), Channel("friends")),
      List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
      new EntriesCaptor,
      persist = false
    )

    r3 shouldBe defined

    runK(r3)
    getK(r3).results shouldBe List(List(bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: A joined consume on duplicate channels given twice followed by three produces" should
    "return a continuation and the produced data" in withTestStore { store =>
    val r1 = consume(
      store,
      List(Channel("colleagues"), Channel("friends"), Channel("friends")),
      List(CityMatch(city = "Crystal Lake"),
           CityMatch(city = "Crystal Lake"),
           CityMatch(city = "Crystal Lake")),
      new EntriesCaptor,
      persist = false
    )

    r1 shouldBe None

    val r2 = produce(store, Channel("friends"), bob, persist = false)

    r2 shouldBe None

    val r3 = produce(store, Channel("friends"), bob, persist = false)

    r3 shouldBe None

    val r4 = produce(store, Channel("colleagues"), alice, persist = false)

    r4 shouldBe defined

    runK(r4)
    getK(r4).results shouldBe List(List(alice, bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: A joined consume on multiple duplicate channels followed by the requisite produces" should
    "return a continuation and the produced data" in withTestStore { store =>
    val r1 = consume(
      store,
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

    r1 shouldBe None

    val r2  = produce(store, Channel("friends"), bob, persist = false)
    val r3  = produce(store, Channel("family"), carol, persist = false)
    val r4  = produce(store, Channel("colleagues"), alice, persist = false)
    val r5  = produce(store, Channel("friends"), bob, persist = false)
    val r6  = produce(store, Channel("family"), carol, persist = false)
    val r7  = produce(store, Channel("colleagues"), alice, persist = false)
    val r8  = produce(store, Channel("colleagues"), alice, persist = false)
    val r9  = produce(store, Channel("family"), carol, persist = false)
    val r10 = produce(store, Channel("family"), carol, persist = false)

    r2 shouldBe None
    r3 shouldBe None
    r4 shouldBe None
    r5 shouldBe None
    r6 shouldBe None
    r7 shouldBe None
    r8 shouldBe None
    r9 shouldBe None
    r10 shouldBe defined

    runK(r10)
    getK(r10).results shouldBe List(List(carol, carol, carol, carol, alice, alice, alice, bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: Multiple produces on multiple duplicate channels followed by the requisite consume" should
    "return a continuation and the produced data" in withTestStore { store =>
    val r1 = produce(store, Channel("friends"), bob, persist = false)
    val r2 = produce(store, Channel("family"), carol, persist = false)
    val r3 = produce(store, Channel("colleagues"), alice, persist = false)
    val r4 = produce(store, Channel("friends"), bob, persist = false)
    val r5 = produce(store, Channel("family"), carol, persist = false)
    val r6 = produce(store, Channel("colleagues"), alice, persist = false)
    val r7 = produce(store, Channel("colleagues"), alice, persist = false)
    val r8 = produce(store, Channel("family"), carol, persist = false)
    val r9 = produce(store, Channel("family"), carol, persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None
    r4 shouldBe None
    r5 shouldBe None
    r6 shouldBe None
    r7 shouldBe None
    r8 shouldBe None
    r9 shouldBe None

    val r10 = consume(
      store,
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

    r10 shouldBe defined
    runK(r10)
    getK(r10).results shouldBe List(List(carol, carol, carol, carol, alice, alice, alice, bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: A joined consume on multiple mixed up duplicate channels followed by the requisite produces" should
    "return a continuation and the produced data" in withTestStore { store =>
    val r1 = consume(
      store,
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

    r1 shouldBe None

    val r2  = produce(store, Channel("friends"), bob, persist = false)
    val r3  = produce(store, Channel("family"), carol, persist = false)
    val r4  = produce(store, Channel("colleagues"), alice, persist = false)
    val r5  = produce(store, Channel("friends"), bob, persist = false)
    val r6  = produce(store, Channel("family"), carol, persist = false)
    val r7  = produce(store, Channel("colleagues"), alice, persist = false)
    val r8  = produce(store, Channel("colleagues"), alice, persist = false)
    val r9  = produce(store, Channel("family"), carol, persist = false)
    val r10 = produce(store, Channel("family"), carol, persist = false)

    r2 shouldBe None
    r3 shouldBe None
    r4 shouldBe None
    r5 shouldBe None
    r6 shouldBe None
    r7 shouldBe None
    r8 shouldBe None
    r9 shouldBe None
    r10 shouldBe defined

    runK(r10)
    getK(r10).results shouldBe List(List(carol, alice, carol, bob, bob, carol, alice, alice, carol))

    store.isEmpty shouldBe true
  }
}

class InMemoryStoreStorageExamplesTests extends StorageExamplesTests {

  override def withTestStore(f: T => Unit): Unit = {
    val testStore = InMemoryStore.create[Channel, Pattern, Entry, EntriesCaptor]
    testStore.clear()
    try {
      f(testStore)
    } finally {
      testStore.close()
    }
  }
}

class LMDBStoreStorageExamplesTest extends StorageExamplesTests with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-storage-test-")
  val mapSize: Long = 1024L * 1024L * 1024L

  override def withTestStore(f: T => Unit): Unit = {
    val testStore = LMDBStore.create[Channel, Pattern, Entry, EntriesCaptor](dbDir, mapSize)
    try {
      testStore.clear()
      f(testStore)
    } finally {
      testStore.close()
    }
  }

  override def afterAll(): Unit = {
    test.recursivelyDeletePath(dbDir)
    super.afterAll()
  }
}
