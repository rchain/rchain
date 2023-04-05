package coop.rchain.store

import cats.effect.{IO, Sync}
import cats.syntax.all._
import coop.rchain.shared.syntax._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
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
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks {
  implicit val scheduler = monix.execution.Scheduler.global

  def genData: Gen[Map[Long, String]] = {
    val arbKV = Arbitrary.arbitrary[(Long, String)]
    Gen.listOfN(2000, arbKV).map(_.toMap)
  }

  it should "put and get data from the store" in {
    forAll(genData) { expected =>
      implicit val kvm = InMemoryStoreManager[IO]
      val sut          = new KeyValueStoreSut[IO]
      val test = for {
        result <- sut.testPutGet(expected)
      } yield result shouldBe expected

      test.unsafeRunSync
    }
  }

  it should "put and get all data from the store" in {
    forAll(genData) { expected =>
      implicit val kvm = InMemoryStoreManager[IO]
      val sut          = new KeyValueStoreSut[IO]
      val test = for {
        result <- sut.testPutIterate(expected)
      } yield result shouldBe expected

      test.unsafeRunSync
    }
  }

  it should "put and collect partial data from the store" in {
    forAll(genData) { expected =>
      implicit val kvm = InMemoryStoreManager[IO]
      val sut          = new KeyValueStoreSut[IO]

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

      test.unsafeRunSync
    }
  }

  it should "not have deleted keys in the store" in {
    forAll(genData) { input =>
      implicit val kvm = InMemoryStoreManager[IO]
      val sut          = new KeyValueStoreSut[IO]
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

      test.unsafeRunSync
    }
  }

}
