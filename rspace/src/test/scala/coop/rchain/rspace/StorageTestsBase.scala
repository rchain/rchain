package coop.rchain.rspace

import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.test._
import org.scalatest._
import scodec.Codec

trait StorageTestsBase[C, P, A, K] extends FlatSpec with Matchers with OptionValues {

  type T = ISpace[C, P, A, K]

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestSpace[R](f: T => R): R
}

class InMemoryStoreTestsBase extends StorageTestsBase[String, Pattern, String, StringsCaptor] {

  override def withTestSpace[R](f: T => R): R = {
    val testStore = InMemoryStore.create[String, Pattern, String, StringsCaptor]
    val testSpace = new RSpace(testStore)
    testStore.withTxn(testStore.createTxnWrite())(testStore.clear)
    try {
      f(testSpace)
    } finally {
      testStore.close()
    }
  }
}

class LMDBStoreTestsBase
    extends StorageTestsBase[String, Pattern, String, StringsCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-storage-test-")
  val mapSize: Long = 1024L * 1024L * 4096L

  override def withTestSpace[R](f: T => R): R = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

    val testStore = LMDBStore.create[String, Pattern, String, StringsCaptor](dbDir, mapSize)
    val testSpace = new RSpace(testStore)
    testStore.withTxn(testStore.createTxnWrite()) { txn =>
      testStore.clear(txn)
      testStore.trieStore.clear(txn)
    }
    history.initialize(testStore.trieStore)
    try {
      f(testSpace)
    } finally {
      testStore.trieStore.close()
      testStore.close()
    }
  }

  override def afterAll(): Unit =
    recursivelyDeletePath(dbDir)
}
