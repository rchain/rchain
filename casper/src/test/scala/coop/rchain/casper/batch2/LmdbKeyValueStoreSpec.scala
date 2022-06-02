package coop.rchain.casper.batch2

import java.nio.file.Files

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.store.{KeyValueStoreSut, LmdbStoreManager}
import monix.eval.Task
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.reflect.io.{Directory, Path}
import scala.util.Random

class LmdbKeyValueStoreSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with BeforeAndAfterAll {
  implicit val scheduler = monix.execution.Scheduler.global

  val tempPath = Files.createTempDirectory(s"lmdb-test-")
  val tempDir  = Directory(Path(tempPath.toFile))

  override def beforeAll: Unit = tempDir.deleteRecursively

  override def afterAll: Unit = tempDir.deleteRecursively

  def withSut[F[_]: Concurrent: Log](f: KeyValueStoreSut[F] => F[Unit]) =
    for {
      kvm <- LmdbStoreManager[F](tempPath.resolve(Random.nextString(32)), 1024 * 1024 * 1024)
      sut = {
        implicit val kvm_ = kvm
        new KeyValueStoreSut[F]
      }
      _ <- f(sut)
      _ <- kvm.shutdown
    } yield ()

  def genData: Gen[Map[Long, String]] = {
    val arbKV = Arbitrary.arbitrary[(Long, String)]
    Gen.listOfN(2000, arbKV).map(_.toMap)
  }

  implicit val log: Log[Task] = new Log.NOPLog[Task]()

  it should "put and get data from the store" in {
    forAll(genData) { expected =>
      val test = withSut[Task] { sut =>
        for {
          result <- sut.testPutGet(expected)
        } yield result shouldBe expected
      }

      test.runSyncUnsafe()
    }
  }

  it should "put and get all data from the store" in {
    forAll(genData) { expected =>
      val test = withSut[Task] { sut =>
        for {
          result <- sut.testPutIterate(expected)
        } yield result shouldBe expected
      }

      test.runSyncUnsafe()
    }
  }

  it should "not have deleted keys in the store" in {
    forAll(genData) { input =>
      val test = withSut[Task] { sut =>
        val allKeys = input.keysIterator.toVector
        // Take some keys for deletion
        val (getKeys, deleteKeys) = allKeys.splitAt(allKeys.size / 2)
        val values                = getKeys.map(input.get)
        // Expected input without deleted keys
        val expected =
          getKeys.zip(values).filter(_._2.nonEmpty).map { case (k, v) => (k, v.get) }.toMap
        for {
          result <- sut.testPutDeleteGet(input, deleteKeys)
        } yield result shouldBe expected
      }

      test.runSyncUnsafe()
    }
  }

}
