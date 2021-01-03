package coop.rchain.casper.batch1

import java.nio.ByteBuffer
import java.nio.file.Files

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.storage.LmdbStoreManager
import coop.rchain.shared.Log.NOPLog
import coop.rchain.store.{InMemoryStoreManager, KeyValueStore, KeyValueStoreManager}
import monix.eval.Task
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class KeyValueStoreSuit[F[_]: Sync: KeyValueStoreManager] {
  def copyToDb(data: Map[String, String]): F[KeyValueStore[F]] =
    for {
      db <- KeyValueStoreManager[F].store("test")
      _ <- db.put(data.toSeq.map {
            case (k, v) => (ByteBuffer.wrap(k.getBytes()), ByteBuffer.wrap(v.getBytes()))
          }, identity[ByteBuffer])
    } yield db

  def testPutGet(input: Map[String, String]) =
    for {
      store  <- copyToDb(input)
      keys   = input.keysIterator.toVector
      values <- store.get(keys.map(k => ByteBuffer.wrap(k.getBytes())), identity[ByteBuffer])
      result = keys.zip(values).filter(_._2.nonEmpty).map { case (k, v) => (k, v.get) }.toMap
    } yield result

  def testPutDeleteGet(input: Map[String, String], deleteKeys: Seq[String]) =
    for {
      store <- copyToDb(input)
      _     <- store.delete(deleteKeys.map(k => ByteBuffer.wrap(k.getBytes())))
    } yield ()

  def testPutIterate(input: Map[String, String]) =
    for {
      store <- copyToDb(input)
    } yield ()
}

abstract class KeyValueStoreSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit val scheduler = monix.execution.Scheduler.global

  def genData: Gen[Map[String, String]] = {
    val arbKV = Arbitrary.arbitrary[(String, String)]
    Gen.listOfN(2000, arbKV).map(_.toMap)
  }

  def createKeyValueStoreManager: KeyValueStoreManager[Task]

  it should "put and get data from the store" in {
    forAll(genData) { expected =>
      implicit val kvm = createKeyValueStoreManager
      val sut          = new KeyValueStoreSuit[Task]
      val test = for {
        result <- sut.testPutGet(expected)
        e      = expected.toSeq.map { case (k, v) => (k, ByteBuffer.wrap(v.getBytes())) }.toMap
      } yield result shouldBe e

      test.runSyncUnsafe()
    }
  }

}

class InMemKeyValueStoreSpec extends KeyValueStoreSpec {
  def createKeyValueStoreManager: KeyValueStoreManager[Task] = InMemoryStoreManager[Task]
}

class ILmdbKeyValueStoreSpec extends KeyValueStoreSpec {
  val dir          = Files.createTempDirectory(s"hash-set-casper-test-genesis-")
  implicit val log = new NOPLog[Task]()
  def createKeyValueStoreManager: KeyValueStoreManager[Task] =
    LmdbStoreManager[Task](dir, 1024L * 1024L * 1024L).runSyncUnsafe()
}
