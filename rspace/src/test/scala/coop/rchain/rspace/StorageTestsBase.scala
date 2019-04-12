package coop.rchain.rspace

import java.nio.file.{Files, Path}

import cats._
import cats.implicits._
import cats.effect._
import com.typesafe.scalalogging.Logger
import com.google.common.collect.HashMultiset
import coop.rchain.metrics.Metrics

import scala.collection.JavaConverters._
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal._
import coop.rchain.shared.PathOps._
import coop.rchain.shared.Log
import org.scalatest._

import scala.concurrent.ExecutionContext
import scodec.Codec

import scala.concurrent.ExecutionContext.Implicits.global

trait StorageTestsBase[F[_], C, P, A, K] extends FlatSpec with Matchers with OptionValues {
  type T  = ISpace[F, C, P, A, A, K]
  type ST = IStore[F, C, P, A, K]

  implicit def concurrentF: Concurrent[F]
  implicit def logF: Log[F]
  implicit def metricsF: Metrics[F]
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
  def withTestSpace[R](f: (ST, T) => F[R]): R
  def withTestSpaceNonF[R](f: (ST, T) => R): R =
    withTestSpace((st: ST, t: T) => concurrentF.delay(f(st, t)))
  def run[S](f: F[S]): S

  def validateIndexedStates(
      store: ST,
      space: T,
      indexedStates: Seq[(State, Int)],
      reportName: String,
      differenceReport: Boolean = false
  ): F[Boolean] = {
    final case class SetRow(data: Set[Datum[A]], wks: Set[WaitingContinuation[P, K]])

    def convertMap(m: Map[Seq[C], Row[P, A, K]]): Map[Seq[C], SetRow] =
      m.map { case (channels, row) => channels -> SetRow(row.data.toSet, row.wks.toSet) }

    val tests: F[List[Boolean]] = indexedStates.toList
      .traverse {
        case (State(checkpoint, rawExpectedContents, expectedJoins), chunkNo) =>
          for {
            _ <- space.reset(checkpoint)
          } yield {
            val num = "%02d".format(chunkNo)

            val expectedContents = convertMap(rawExpectedContents)
            val actualContents   = convertMap(space.toMap)

            val contentsTest = expectedContents == actualContents

            val actualJoins = store.joinMap

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
                    case None =>
                      logger.error(s"key [$expectedChannels] not found in actual records")
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
                    case None =>
                      logger.error(s"key [$actualChannels] not found in expected records")
                  }
                }
              }
            }
            result
          }
      }
    tests.map(!_.contains(false))
  }
}

abstract class InMemoryStoreTestsBase[F[_]]
    extends StorageTestsBase[F, String, Pattern, String, StringsCaptor]
    with BeforeAndAfterAll {

  override def withTestSpace[S](f: (ST, T) => F[S]): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec
    val branch                                = Branch("inmem")

    val trieStore =
      InMemoryTrieStore.create[Blake2b256Hash, GNAT[String, Pattern, String, StringsCaptor]]()
    val testStore = InMemoryStore.create(trieStore, branch)

    run(for {
      testSpace <- RSpace.create[F, String, Pattern, String, String, StringsCaptor](
                    testStore,
                    branch
                  )
      _   <- history.initialize(trieStore, branch).pure[F]
      _   <- testSpace.createCheckpoint()
      res <- f(testStore, testSpace)
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
    extends StorageTestsBase[F, String, Pattern, String, StringsCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-storage-test-")
  val mapSize: Long = 1024L * 1024L * 4096L

  override def withTestSpace[S](f: (ST, T) => F[S]): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

    val testBranch = Branch("test")
    val env        = Context.env(dbDir, mapSize)
    val trieStore = LMDBTrieStore
      .create[F, Blake2b256Hash, GNAT[String, Pattern, String, StringsCaptor]](env, dbDir)

    val testStore = LMDBStore.create[F, String, Pattern, String, StringsCaptor](
      Context.create(dbDir, mapSize),
      testBranch
    )

    run(for {
      testSpace <- RSpace.create[F, String, Pattern, String, String, StringsCaptor](
                    testStore,
                    testBranch
                  )
      _ <- testStore.withWriteTxnF { txn =>
            testStore.clear(txn)
          }
      _ <- Sync[F].delay(trieStore.withTxn(trieStore.createTxnWrite) { trieTxn =>
            trieStore.clear(trieTxn)
          })
      _   <- history.initialize(trieStore, testBranch).pure[F]
      _   <- testSpace.createCheckpoint()
      res <- f(testStore, testSpace)
    } yield {
      try {
        res
      } finally {
        trieStore.close()
        testStore.close()
        env.close()
      }
    })
  }

  override def afterAll(): Unit =
    dbDir.recursivelyDelete
}

abstract class MixedStoreTestsBase[F[_]]
    extends StorageTestsBase[F, String, Pattern, String, StringsCaptor]
    with BeforeAndAfterAll {

  val dbDir: Path   = Files.createTempDirectory("rchain-mixed-storage-test-")
  val mapSize: Long = 1024L * 1024L * 4096L

  override def withTestSpace[S](f: (ST, T) => F[S]): S = {
    implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
    implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
    implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

    val testBranch = Branch("test")
    val env        = Context.env(dbDir, mapSize)
    val trieStore = LMDBTrieStore
      .create[F, Blake2b256Hash, GNAT[String, Pattern, String, StringsCaptor]](env, dbDir)
    val testStore = LockFreeInMemoryStore.create(trieStore, testBranch)

    run(for {
      testSpace <- RSpace.create[F, String, Pattern, String, String, StringsCaptor](
                    testStore,
                    testBranch
                  )
      _ <- testStore.withWriteTxnF { txn =>
            testStore.clear(txn)
          }
      _ <- Sync[F].delay(trieStore.withTxn(trieStore.createTxnWrite) { trieTxn =>
            trieStore.clear(trieTxn)
          })
      _   <- history.initialize(trieStore, testBranch).pure[F]
      _   <- testSpace.createCheckpoint()
      res <- f(testStore, testSpace)
    } yield {
      try {
        res
      } finally {
        trieStore.close()
        testStore.close()
        env.close()
      }
    })
  }

  override def afterAll(): Unit =
    dbDir.recursivelyDelete
}
