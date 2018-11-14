package coop.rchain.rspace

import java.nio.file.{Files, Path}

import cats._
import cats.implicits._
import cats.effect._
import com.typesafe.scalalogging.Logger
import com.google.common.collect.HashMultiset
import coop.rchain.rspace.ISpace.IdISpace

import scala.collection.JavaConverters._
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal._
import coop.rchain.shared.PathOps._
import org.scalatest._

import scala.collection.immutable.{Seq, Set}
import scala.concurrent.ExecutionContext
import scodec.Codec

import scala.concurrent.ExecutionContext.Implicits.global

trait StorageTestsBase[F[_], C, P, E, A, K] extends FlatSpec with Matchers with OptionValues {
  type T = ISpace[F, C, P, E, A, A, K]

  implicit def syncF: Sync[F]
  implicit def monadF: Monad[F]
  implicit def contextShiftF: ContextShift[F]

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
  def withTestSpace[R](f: T => F[R]): R
  def run[S](f: F[S]): S

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

abstract class InMemoryStoreTestsBase[F[_]]
    extends StorageTestsBase[F, String, Pattern, Nothing, String, StringsCaptor]
    with BeforeAndAfterAll {

  override def withTestSpace[S](f: T => F[S]): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec
    val branch                                = Branch("inmem")

    val ctx: Context[String, Pattern, String, StringsCaptor] = Context.createInMemory()

    run(for {
      testSpace <- RSpace.create[F, String, Pattern, Nothing, String, String, StringsCaptor](
                    ctx,
                    branch
                  )
      testStore = testSpace.store
      trieStore = testStore.trieStore
      _ <- testStore
            .withTxn(testStore.createTxnWrite()) { txn =>
              testStore.withTrieTxn(txn) { trieTxn =>
                testStore.clear(txn)
                testStore.trieStore.clear(trieTxn)
              }
            }
            .pure[F]
      _   <- history.initialize(trieStore, branch).pure[F]
      _   <- testSpace.createCheckpoint()
      res <- f(testSpace)
    } yield {
      try {
        res
      } finally {
        trieStore.close()
        testStore.close()
      }
    })
  }

  override def afterAll(): Unit =
    super.afterAll()
}

abstract class LMDBStoreTestsBase[F[_]]
    extends StorageTestsBase[F, String, Pattern, Nothing, String, StringsCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-storage-test-")
  val mapSize: Long = 1024L * 1024L * 4096L

  override def withTestSpace[S](f: T => F[S]): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

    val testBranch = Branch("test")
    val env        = Context.create[String, Pattern, String, StringsCaptor](dbDir, mapSize)

    run(for {
      testSpace <- RSpace.create[F, String, Pattern, Nothing, String, String, StringsCaptor](
                    env,
                    testBranch
                  )
      testStore = testSpace.store
      _ <- testStore
            .withTxn(testStore.createTxnWrite()) { txn =>
              testStore.withTrieTxn(txn) { trieTxn =>
                testStore.clear(txn)
                testStore.trieStore.clear(trieTxn)
              }
            }
            .pure[F]
      _   <- history.initialize(testStore.trieStore, testBranch).pure[F]
      _   <- testSpace.createCheckpoint()
      res <- f(testSpace)
    } yield {
      try {
        res
      } finally {
        testStore.trieStore.close()
        testStore.close()
        env.close()
      }
    })
  }

  override def afterAll(): Unit =
    dbDir.recursivelyDelete
}

abstract class MixedStoreTestsBase[F[_]]
    extends StorageTestsBase[F, String, Pattern, Nothing, String, StringsCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-mixed-storage-test-")
  val mapSize: Long = 1024L * 1024L * 4096L

  override def withTestSpace[S](f: T => F[S]): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

    val testBranch = Branch("test")
    val env        = Context.createMixed[String, Pattern, String, StringsCaptor](dbDir, mapSize)

    run(for {
      testSpace <- RSpace.create[F, String, Pattern, Nothing, String, String, StringsCaptor](
                    env,
                    testBranch
                  )
      testStore = testSpace.store
      _ <- testStore
            .withTxn(testStore.createTxnWrite()) { txn =>
              testStore.withTrieTxn(txn) { trieTxn =>
                testStore.clear(txn)
                testStore.trieStore.clear(trieTxn)
              }
            }
            .pure[F]
      _   <- history.initialize(testStore.trieStore, testBranch).pure[F]
      _   <- testSpace.createCheckpoint()
      res <- f(testSpace)
    } yield {
      try {
        res
      } finally {
        testStore.trieStore.close()
        testStore.close()
        env.close()
      }
    })
  }

  override def afterAll(): Unit =
    dbDir.recursivelyDelete
}
