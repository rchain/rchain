package coop.rchain.rspace

import java.nio.file.{Files, Path}

import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.history.{initialize, Branch, ITrieStore, InMemoryTrieStore, LMDBTrieStore}
import coop.rchain.rspace.internal.{codecGNAT, GNAT}
import coop.rchain.rspace.util._
import coop.rchain.shared.PathOps._
import org.scalatest.BeforeAndAfterAll
import scodec.Codec

trait StorageExamplesTests extends StorageTestsBase[Channel, Pattern, Entry, EntriesCaptor] {

  "CORE-365: A joined consume on duplicate channels followed by two produces on that channel" should
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space.consume(
      List(Channel("friends"), Channel("friends")),
      List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
      new EntriesCaptor,
      persist = false
    )

    r1 shouldBe None

    val r2 = space.produce(Channel("friends"), bob, persist = false)

    r2 shouldBe None

    val r3 = space.produce(Channel("friends"), bob, persist = false)

    r3 shouldBe defined

    runK(r3)
    getK(r3).results shouldBe List(List(bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: Two produces on the same channel followed by a joined consume on duplicates of that channel" should
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space.produce(Channel("friends"), bob, persist = false)

    r1 shouldBe None

    val r2 = space.produce(Channel("friends"), bob, persist = false)

    r2 shouldBe None

    val r3 = space.consume(
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
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space.consume(
      List(Channel("colleagues"), Channel("friends"), Channel("friends")),
      List(CityMatch(city = "Crystal Lake"),
           CityMatch(city = "Crystal Lake"),
           CityMatch(city = "Crystal Lake")),
      new EntriesCaptor,
      persist = false
    )

    r1 shouldBe None

    val r2 = space.produce(Channel("friends"), bob, persist = false)

    r2 shouldBe None

    val r3 = space.produce(Channel("friends"), bob, persist = false)

    r3 shouldBe None

    val r4 = space.produce(Channel("colleagues"), alice, persist = false)

    r4 shouldBe defined

    runK(r4)
    getK(r4).results shouldBe List(List(alice, bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: A joined consume on multiple duplicate channels followed by the requisite produces" should
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space.consume(
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

    val r2  = space.produce(Channel("friends"), bob, persist = false)
    val r3  = space.produce(Channel("family"), carol, persist = false)
    val r4  = space.produce(Channel("colleagues"), alice, persist = false)
    val r5  = space.produce(Channel("friends"), bob, persist = false)
    val r6  = space.produce(Channel("family"), carol, persist = false)
    val r7  = space.produce(Channel("colleagues"), alice, persist = false)
    val r8  = space.produce(Channel("colleagues"), alice, persist = false)
    val r9  = space.produce(Channel("family"), carol, persist = false)
    val r10 = space.produce(Channel("family"), carol, persist = false)

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
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space.produce(Channel("friends"), bob, persist = false)
    val r2 = space.produce(Channel("family"), carol, persist = false)
    val r3 = space.produce(Channel("colleagues"), alice, persist = false)
    val r4 = space.produce(Channel("friends"), bob, persist = false)
    val r5 = space.produce(Channel("family"), carol, persist = false)
    val r6 = space.produce(Channel("colleagues"), alice, persist = false)
    val r7 = space.produce(Channel("colleagues"), alice, persist = false)
    val r8 = space.produce(Channel("family"), carol, persist = false)
    val r9 = space.produce(Channel("family"), carol, persist = false)

    r1 shouldBe None
    r2 shouldBe None
    r3 shouldBe None
    r4 shouldBe None
    r5 shouldBe None
    r6 shouldBe None
    r7 shouldBe None
    r8 shouldBe None
    r9 shouldBe None

    val r10 = space.consume(
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
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space.consume(
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

    val r2  = space.produce(Channel("friends"), bob, persist = false)
    val r3  = space.produce(Channel("family"), carol, persist = false)
    val r4  = space.produce(Channel("colleagues"), alice, persist = false)
    val r5  = space.produce(Channel("friends"), bob, persist = false)
    val r6  = space.produce(Channel("family"), carol, persist = false)
    val r7  = space.produce(Channel("colleagues"), alice, persist = false)
    val r8  = space.produce(Channel("colleagues"), alice, persist = false)
    val r9  = space.produce(Channel("family"), carol, persist = false)
    val r10 = space.produce(Channel("family"), carol, persist = false)

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

class InMemoryStoreStorageExamplesTestsBase
    extends StorageTestsBase[Channel, Pattern, Entry, EntriesCaptor] {

  override def withTestSpace[R](f: T => R): R = {
    implicit val cg: Codec[GNAT[Channel, Pattern, Entry, EntriesCaptor]] = codecGNAT(
      implicits.serializeChannel.toCodec,
      implicits.serializePattern.toCodec,
      implicits.serializeInfo.toCodec,
      implicits.serializeEntriesCaptor.toCodec)

    val branch = Branch("inmem")

    val trieStore =
      InMemoryTrieStore.create[Blake2b256Hash, GNAT[Channel, Pattern, Entry, EntriesCaptor]]()

    val testStore = InMemoryStore.create[
      InMemTransaction[history.State[Blake2b256Hash, GNAT[Channel, Pattern, Entry, EntriesCaptor]]],
      Channel,
      Pattern,
      Entry,
      EntriesCaptor](trieStore, branch)

    val testSpace = RSpace.create[Channel, Pattern, Entry, Entry, EntriesCaptor](testStore, branch)
    testStore.withTxn(testStore.createTxnWrite())(testStore.clear)
    trieStore.withTxn(trieStore.createTxnWrite())(trieStore.clear)
    initialize(trieStore, branch)
    try {
      f(testSpace)
    } finally {
      trieStore.close()
      testStore.close()
    }
  }
}

class InMemoryStoreStorageExamplesTests
    extends InMemoryStoreStorageExamplesTestsBase
    with StorageExamplesTests

class LMDBStoreStorageExamplesTestBase
    extends StorageTestsBase[Channel, Pattern, Entry, EntriesCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path    = Files.createTempDirectory("rchain-storage-test-")
  val mapSize: Long  = 1024L * 1024L * 1024L
  val noTls: Boolean = false

  override def withTestSpace[R](f: T => R): R = {
    val context   = Context.create[Channel, Pattern, Entry, EntriesCaptor](dbDir, mapSize, noTls)
    val testStore = LMDBStore.create[Channel, Pattern, Entry, EntriesCaptor](context)
    val testSpace =
      RSpace.create[Channel, Pattern, Entry, Entry, EntriesCaptor](testStore, Branch.MASTER)
    try {
      testStore.withTxn(testStore.createTxnWrite())(txn => testStore.clear(txn))
      f(testSpace)
    } finally {
      testStore.close()
      testSpace.close()
      context.close()
    }
  }

  override def afterAll(): Unit = {
    dbDir.recursivelyDelete
    super.afterAll()
  }
}

class LMDBStoreStorageExamplesTest
    extends LMDBStoreStorageExamplesTestBase
    with StorageExamplesTests
