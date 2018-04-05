package coop.rchain.rspace

import java.nio.file.Files

import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample.implicits._
import coop.rchain.rspace.test.recursivelyDeletePath
import org.scalatest.BeforeAndAfterAll

class StorageExamplesTest
    extends StorageActionsBase[
      IStore[Channel, Pattern, Entry, Printer] with ITestableStore[Channel, Pattern]]
    with BeforeAndAfterAll {
  private[this] val dbDir = Files.createTempDirectory("rchain-storage-test-")

  "A joined consume with the same channel given twice followed by a produce" should
    "not raises any errors (CORE-365)" in withTestStore { store =>
    val r1 = consume(store,
                     List(Channel("friends"), Channel("friends")),
                     List(CityMatch(city = "Crystal Lake"), CityMatch(city = "Crystal Lake")),
                     new Printer,
                     persist = false)

    val r2 = produce(store, Channel("friends"), bob, persist = false)

    r1 shouldBe None
    r2 shouldBe defined

    store.isEmpty shouldBe true
  }

  override def withTestStore(
      f: IStore[Channel, Pattern, Entry, Printer] with ITestableStore[Channel, Pattern] => Unit)
    : Unit = {
    val testStore =
      LMDBStore.create[Channel, Pattern, Entry, Printer](dbDir, 1024 * 1024 * 1024)
    testStore.clear()
    try {
      f(testStore)
    } finally {
      testStore.close()
    }
  }

  override def afterAll(): Unit =
    recursivelyDeletePath(dbDir)
}
