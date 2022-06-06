package coop.rchain.store

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.syntax._
import monix.eval.Task
import monix.execution.Scheduler
import monix.testing.scalatest.MonixTaskTest
import org.scalacheck.effect.PropF
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.codecs.{int64, utf8}

class KeyValueStoreSut[F[_]: Sync: KeyValueStoreManager] {
  def copyToDb(data: Map[Long, String]): F[KeyValueTypedStore[F, Long, String]] =
    for {
      db <- KeyValueStoreManager[F].database("test", int64, utf8)
      _  <- db.put(data.toSeq)
    } yield db

  def testPutGet(input: Map[Long, String]): F[Map[Long, String]] =
    for {
      store  <- copyToDb(input)
      keys   = input.keysIterator.toVector
      values <- store.get(keys)
      result = keys.zip(values).filter(_._2.nonEmpty).map { case (k, v) => (k, v.get) }.toMap
    } yield result

  def testPutDeleteGet(input: Map[Long, String], deleteKeys: Seq[Long]): F[Map[Long, String]] =
    for {
      store  <- copyToDb(input)
      _      <- store.delete(deleteKeys)
      result <- store.toMap
    } yield result

  def testPutIterate(input: Map[Long, String]): F[Map[Long, String]] =
    for {
      store  <- copyToDb(input)
      result <- store.toMap
    } yield result

  def testPutCollect(
      input: Map[Long, String]
  )(pf: PartialFunction[(Long, () => String), (Long, String)]): F[Map[Long, String]] =
    for {
      store  <- copyToDb(input)
      result <- store.collect(pf)
    } yield result.toMap
}

class InMemoryKeyValueStoreSpec
    extends AsyncFlatSpec
    with MonixTaskTest
    with Matchers
    with ScalaCheckDrivenPropertyChecks {
  implicit override def scheduler: Scheduler = Scheduler.io("monix-task-support-spec")

  def genData: Gen[Map[Long, String]] = {
    val arbKV = Arbitrary.arbitrary[(Long, String)]
    Gen.listOfN(2000, arbKV).map(_.toMap)
  }

  it should "put and get data from the store" in {
    PropF
      .forAllF(genData) { expected =>
        implicit val kvm = InMemoryStoreManager[Task]
        val sut          = new KeyValueStoreSut[Task]
        val test = for {
          result <- sut.testPutGet(expected)
        } yield result shouldBe expected

        test.void
      }
      .check()
      .map(r => assert(r.passed, r.status.toString))
  }

  it should "put and get all data from the store" in {
    PropF
      .forAllF(genData) { expected =>
        implicit val kvm = InMemoryStoreManager[Task]
        val sut          = new KeyValueStoreSut[Task]
        val test = for {
          result <- sut.testPutIterate(expected)
        } yield result shouldBe expected

        test.void
      }
      .check()
      .map(r => assert(r.passed, r.status.toString))
  }

  it should "put and collect partial data from the store" in {
    PropF
      .forAllF(genData) { expected =>
        implicit val kvm = InMemoryStoreManager[Task]
        val sut          = new KeyValueStoreSut[Task]

        val keys = expected.toList.map(_._1)
        val kMin = keys.min
        val kMax = keys.min
        val kAvg = kMax - kMin / 2
        // Filter expected values
        val expectedFiltered = expected.filter {
          case (k, _) => k >= kAvg
        }

        val test = for {
          // Filter using partial function
          result <- sut.testPutCollect(expected) {
                     case (k, fv) if k >= kAvg => (k, fv())
                   }
        } yield result shouldBe expectedFiltered

        test.void
      }
      .check()
      .map(r => assert(r.passed, r.status.toString))
  }

  it should "not have deleted keys in the store" in {
    PropF
      .forAllF(genData) { input =>
        implicit val kvm = InMemoryStoreManager[Task]
        val sut          = new KeyValueStoreSut[Task]
        val allKeys      = input.keysIterator.toVector
        // Take some keys for deletion
        val (getKeys, deleteKeys) = allKeys.splitAt(allKeys.size / 2)
        val values                = getKeys.map(input.get)
        // Expected input without deleted keys
        val expected =
          getKeys.zip(values).filter(_._2.nonEmpty).map { case (k, v) => (k, v.get) }.toMap
        val test = for {
          result <- sut.testPutDeleteGet(input, deleteKeys)
        } yield result shouldBe expected

        test.void
      }
      .check()
      .map(r => assert(r.passed, r.status.toString))
  }

}
