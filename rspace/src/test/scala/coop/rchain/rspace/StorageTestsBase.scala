package coop.rchain.rspace
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.{initialize, Branch, ITrieStore, InMemoryTrieStore, LMDBTrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test._
import coop.rchain.shared.PathOps._
import org.scalatest._
import scodec.Codec

trait StorageTestsBase[C, P, A, K] extends FlatSpec with Matchers with OptionValues {

  type T = ISpace[C, P, A, A, K]

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestSpace[S](f: T => S): S
}

class InMemoryStoreTestsBase
    extends StorageTestsBase[String, Pattern, String, StringsCaptor]
    with BeforeAndAfterAll {

  override def withTestSpace[S](f: T => S): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec
    val branch                                = Branch("inmem")

    val trieStore =
      InMemoryTrieStore.create[Blake2b256Hash, GNAT[String, Pattern, String, StringsCaptor]]()

    val testStore = InMemoryStore.create[
      InMemTransaction[history.State[Blake2b256Hash, GNAT[String, Pattern, String, StringsCaptor]]],
      String,
      Pattern,
      String,
      StringsCaptor](trieStore, branch)

    val testSpace = RSpace.create[String, Pattern, String, String, StringsCaptor](testStore, branch)
    testStore.withTxn(testStore.createTxnWrite()) { txn =>
      testStore.withTrieTxn(txn) { trieTxn =>
        testStore.clear(txn)
        testStore.trieStore.clear(trieTxn)
      }
    }
    history.initialize(trieStore, branch)
    val _ = testSpace.createCheckpoint()
    try {
      f(testSpace)
    } finally {
      trieStore.close()
      testStore.close()
    }
  }

  override def afterAll(): Unit =
    super.afterAll()
}

class LMDBStoreTestsBase
    extends StorageTestsBase[String, Pattern, String, StringsCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-storage-test-")
  val mapSize: Long = 1024L * 1024L * 4096L

  override def withTestSpace[S](f: T => S): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

    val testBranch = Branch("test")
    val env        = Context.create[String, Pattern, String, StringsCaptor](dbDir, mapSize)
    val testStore  = LMDBStore.create[String, Pattern, String, StringsCaptor](env, testBranch)
    val testSpace =
      RSpace.create[String, Pattern, String, String, StringsCaptor](testStore, testBranch)
    testStore.withTxn(testStore.createTxnWrite()) { txn =>
      testStore.withTrieTxn(txn) { trieTxn =>
        testStore.clear(txn)
        testStore.trieStore.clear(trieTxn)
      }
    }
    history.initialize(testStore.trieStore, testBranch)
    val _ = testSpace.createCheckpoint()
    try {
      f(testSpace)
    } finally {
      testStore.trieStore.close()
      testStore.close()
      env.close()
    }
  }

  override def afterAll(): Unit =
    dbDir.recursivelyDelete
}
