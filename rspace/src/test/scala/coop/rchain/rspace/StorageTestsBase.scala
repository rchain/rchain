package coop.rchain.rspace

import coop.rchain.rspace.spaces.FineGrainedRSpace
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.Logger
import com.google.common.collect.HashMultiset
import coop.rchain.rspace.ISpace.IdISpace

import scala.collection.JavaConverters._
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.{initialize, Branch, ITrieStore, InMemoryTrieStore, LMDBTrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test._
import coop.rchain.shared.PathOps._
import org.scalatest._

import scala.collection.immutable.{Seq, Set}
import scodec.Codec

trait StorageTestsBase[C, P, E, A, K] extends FlatSpec with Matchers with OptionValues {

  type T = IdISpace[C, P, E, A, A, K]

  case class State(
      checkpoint: Blake2b256Hash,
      contents: Map[Seq[C], Row[P, A, K]],
      joins: Map[Blake2b256Hash, Seq[Seq[C]]]
  )

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def withTestSpace[S](f: T => S): S

  def validateIndexedStates(
      space: T,
      indexedStates: Seq[(State, Int)],
      reportName: String,
      differenceReport: Boolean = false
  ): Boolean = {
    final case class SetRow(data: Set[Datum[A]], wks: Set[WaitingContinuation[P, K]])

    def convertMap(m: Map[Seq[C], Row[P, A, K]]): Map[Seq[C], SetRow] =
      m.map { case (channels, row) => channels -> SetRow(row.data.toSet, row.wks.toSet) }

    val tests: Seq[Any] = indexedStates
      .map {
        case (State(checkpoint, rawExpectedContents, expectedJoins), chunkNo) =>
          space.reset(checkpoint)
          val num = "%02d".format(chunkNo)

          val expectedContents = convertMap(rawExpectedContents)
          val actualContents   = convertMap(space.store.toMap)

          val contentsTest = expectedContents == actualContents

          val actualJoins = space.store.joinMap

          val joinsTest =
            expectedJoins.forall {
              case (hash: Blake2b256Hash, expecteds: Seq[Seq[C]]) =>
                val expected = HashMultiset.create[Seq[C]](expecteds.asJava)
                val actual   = HashMultiset.create[Seq[C]](actualJoins(hash).asJava)
                expected.equals(actual)
            }

          val result = contentsTest && joinsTest
          if (!result) {
            if (!contentsTest) {
              logger.error(s"$num: store had unexpected contents ($reportName)")
            }

            if (!joinsTest) {
              logger.error(s"$num: store had unexpected joins ($reportName)")
            }

            if (differenceReport) {
              logger.error(s"difference report ($reportName)")
              for ((expectedChannels, expectedRow) <- expectedContents) {
                val actualRow = actualContents.get(expectedChannels)

                actualRow match {
                  case Some(row) =>
                    if (row != expectedRow) {
                      logger.error(
                        s"key [$expectedChannels] invalid actual value: $row !== $expectedRow"
                      )
                    }
                  case None => logger.error(s"key [$expectedChannels] not found in actual records")
                }
              }

              for ((actualChannels, actualRow) <- actualContents) {
                val expectedRow = expectedContents.get(actualChannels)

                expectedRow match {
                  case Some(row) =>
                    if (row != actualRow) {
                      logger.error(
                        s"key[$actualChannels] invalid actual value: $actualRow !== $row"
                      )
                    }
                  case None => logger.error(s"key [$actualChannels] not found in expected records")
                }
              }
            }
          }
          result
      }
    !tests.contains(false)
  }
}

class InMemoryStoreTestsBase
    extends StorageTestsBase[String, Pattern, Nothing, String, StringsCaptor]
    with BeforeAndAfterAll {

  override def withTestSpace[S](f: T => S): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec
    val branch                                = Branch("inmem")

    val trieStore =
      InMemoryTrieStore.create[Blake2b256Hash, GNAT[String, Pattern, String, StringsCaptor]]()

    val testStore = InMemoryStore.create[InMemTransaction[
      history.State[Blake2b256Hash, GNAT[String, Pattern, String, StringsCaptor]]
    ], String, Pattern, String, StringsCaptor](trieStore, branch)

    val testSpace =
      RSpace.create[String, Pattern, Nothing, String, String, StringsCaptor](testStore, branch)
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
    extends StorageTestsBase[String, Pattern, Nothing, String, StringsCaptor]
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
      RSpace.create[String, Pattern, Nothing, String, String, StringsCaptor](testStore, testBranch)
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

class MixedStoreTestsBase
    extends StorageTestsBase[String, Pattern, Nothing, String, StringsCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-mixed-storage-test-")
  val mapSize: Long = 1024L * 1024L * 4096L

  override def withTestSpace[S](f: T => S): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

    val testBranch = Branch("test")
    val env        = Context.createMixed[String, Pattern, String, StringsCaptor](dbDir, mapSize)
    val testStore = InMemoryStore
      .create[org.lmdbjava.Txn[java.nio.ByteBuffer], String, Pattern, String, StringsCaptor](
        env.trieStore,
        testBranch
      )

    val testSpace =
      RSpace.create[String, Pattern, Nothing, String, String, StringsCaptor](testStore, testBranch)
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

class FineGrainedTestsBase
    extends StorageTestsBase[String, Pattern, Nothing, String, StringsCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-storage-test-")
  val mapSize: Long = 1024L * 1024L * 4096L

  override def withTestSpace[S](f: T => S): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

    val testBranch = Branch("test")
    val env        = Context.createFineGrained[String, Pattern, String, StringsCaptor](dbDir, mapSize)
    val testStore =
      LMDBStore.create[String, Pattern, String, StringsCaptor](env, testBranch)
    val testSpace =
      new FineGrainedRSpace[String, Pattern, Nothing, String, String, StringsCaptor](
        testStore,
        testBranch
      )
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
