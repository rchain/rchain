package coop.rchain.rspace

import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.Logger
import org.scalatest._

trait StorageTestsBase[C, P, A, K] extends FlatSpec with Matchers with OptionValues {

  type T = IStore[C, P, A, K] with ITestableStore[C, P]

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestStore(f: T => Unit): Unit
}

trait InMemStorageTestBase[C, P, A, K] extends StorageTestsBase[C, P, A, K] {
  def createTestStore: IStore[C, P, A, K] with ITestableStore[C, P]

  override def withTestStore(f: T => Unit): Unit = {
    val testStore = createTestStore
    testStore.clear()
    try {
      f(testStore)
    } finally {
      testStore.close()
    }
  }
}

class LMDBStoreTestsBase[C: Serialize, P: Serialize, A: Serialize, K: Serialize]
    extends StorageTestsBase[C, P, A, K]
    with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-storage-test-")
  val mapSize: Long = 1024L * 1024L * 1024L

  override def withTestStore(f: T => Unit): Unit = {
    val testStore = LMDBStore.create[C, P, A, K](dbDir, mapSize)
    testStore.clear()
    try {
      f(testStore)
    } finally {
      testStore.close()
    }
  }

  override def afterAll(): Unit =
    test.recursivelyDeletePath(dbDir)
}
