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

trait StorageExamplesTests
    extends StorageTestsBase[Channel, Pattern, Nothing, Null, Entry, EntriesCaptor]
    with TestImplicitHelpers {

  "CORE-365: A joined consume on duplicate channels followed by two produces on that channel" should
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space
      .consume(
        List(Channel("friends"), Channel("friends")),
        List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
        new EntriesCaptor,
        persist = false
      )

    assert(r1.isNotFound)

    val r2 = space.produce(Channel("friends"), bob, persist = false)

    assert(r2.isNotFound)

    val r3 = space.produce(Channel("friends"), bob, persist = false)

    assert(r3.isFound)

    runK(r3.toEither)
    getK(r3.toEither).results should contain theSameElementsAs List(List(bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: Two produces on the same channel followed by a joined consume on duplicates of that channel" should
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space.produce(Channel("friends"), bob, persist = false)

    assert(r1.isNotFound)

    val r2 = space.produce(Channel("friends"), bob, persist = false)

    assert(r2.isNotFound)

    val r3 = space
      .consume(
        List(Channel("friends"), Channel("friends")),
        List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
        new EntriesCaptor,
        persist = false
      )

    assert(r3.isFound)

    runK(r3)
    getK(r3).results should contain theSameElementsAs List(List(bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: A joined consume on duplicate channels given twice followed by three produces" should
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space
      .consume(
        List(Channel("colleagues"), Channel("friends"), Channel("friends")),
        List(CityMatch(city = "Crystal Lake"),
             CityMatch(city = "Crystal Lake"),
             CityMatch(city = "Crystal Lake")),
        new EntriesCaptor,
        persist = false
      )

    assert(r1.isNotFound)

    val r2 = space.produce(Channel("friends"), bob, persist = false)

    assert(r2.isNotFound)

    val r3 = space.produce(Channel("friends"), bob, persist = false)

    assert(r3.isNotFound)

    val r4 = space.produce(Channel("colleagues"), alice, persist = false)

    assert(r4.isFound)

    runK(r4.toEither)
    getK(r4.toEither).results shouldBe List(List(alice, bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: A joined consume on multiple duplicate channels followed by the requisite produces" should
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space
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

    assert(r1.isNotFound)

    val r2  = space.produce(Channel("friends"), bob, persist = false)
    val r3  = space.produce(Channel("family"), carol, persist = false)
    val r4  = space.produce(Channel("colleagues"), alice, persist = false)
    val r5  = space.produce(Channel("friends"), bob, persist = false)
    val r6  = space.produce(Channel("family"), carol, persist = false)
    val r7  = space.produce(Channel("colleagues"), alice, persist = false)
    val r8  = space.produce(Channel("colleagues"), alice, persist = false)
    val r9  = space.produce(Channel("family"), carol, persist = false)
    val r10 = space.produce(Channel("family"), carol, persist = false)

    assert(r2.isNotFound)
    assert(r3.isNotFound)
    assert(r4.isNotFound)
    assert(r5.isNotFound)
    assert(r6.isNotFound)
    assert(r7.isNotFound)
    assert(r8.isNotFound)
    assert(r9.isNotFound)
    assert(r10.isFound)

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

    assert(r1.isNotFound)
    assert(r2.isNotFound)
    assert(r3.isNotFound)
    assert(r4.isNotFound)
    assert(r5.isNotFound)
    assert(r6.isNotFound)
    assert(r7.isNotFound)
    assert(r8.isNotFound)
    assert(r9.isNotFound)

    val r10 = space
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

    assert(r10.isFound)
    runK(r10)
    getK(r10).results shouldBe List(List(carol, carol, carol, carol, alice, alice, alice, bob, bob))

    store.isEmpty shouldBe true
  }

  "CORE-365: A joined consume on multiple mixed up duplicate channels followed by the requisite produces" should
    "return a continuation and the produced data" in withTestSpace { space =>
    val store = space.store

    val r1 = space
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

    assert(r1.isNotFound)

    val r2  = space.produce(Channel("friends"), bob, persist = false)
    val r3  = space.produce(Channel("family"), carol, persist = false)
    val r4  = space.produce(Channel("colleagues"), alice, persist = false)
    val r5  = space.produce(Channel("friends"), bob, persist = false)
    val r6  = space.produce(Channel("family"), carol, persist = false)
    val r7  = space.produce(Channel("colleagues"), alice, persist = false)
    val r8  = space.produce(Channel("colleagues"), alice, persist = false)
    val r9  = space.produce(Channel("family"), carol, persist = false)
    val r10 = space.produce(Channel("family"), carol, persist = false)

    assert(r2.isNotFound)
    assert(r3.isNotFound)
    assert(r4.isNotFound)
    assert(r5.isNotFound)
    assert(r6.isNotFound)
    assert(r7.isNotFound)
    assert(r8.isNotFound)
    assert(r9.isNotFound)
    assert(r10.isFound)

    runK(r10)
    getK(r10).results shouldBe List(List(carol, alice, carol, bob, bob, carol, alice, alice, carol))

    store.isEmpty shouldBe true
  }
}

class InMemoryStoreStorageExamplesTestsBase
    extends StorageTestsBase[Channel, Pattern, Nothing, Null, Entry, EntriesCaptor] {

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

    val testSpace =
      RSpace.create[Channel, Pattern, Nothing, Entry, Null, Entry, EntriesCaptor](testStore, branch)
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
    extends StorageTestsBase[Channel, Pattern, Nothing, Null, Entry, EntriesCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path    = Files.createTempDirectory("rchain-storage-test-")
  val mapSize: Long  = 1024L * 1024L * 1024L
  val noTls: Boolean = false

  override def withTestSpace[R](f: T => R): R = {
    val context   = Context.create[Channel, Pattern, Entry, EntriesCaptor](dbDir, mapSize, noTls)
    val testStore = LMDBStore.create[Channel, Pattern, Entry, EntriesCaptor](context)
    val testSpace =
      RSpace.create[Channel, Pattern, Nothing, Entry, Null, Entry, EntriesCaptor](testStore,
                                                                                  Branch.MASTER)
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
