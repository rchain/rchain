package coop.rchain.storage

import java.security.MessageDigest

import org.scalatest.{EitherValues, FlatSpec, Matchers}

//noinspection ScalaUnnecessaryParentheses
class StorageTest extends FlatSpec with Matchers with EitherValues with StorageFixtures {

  behavior of "A Storage instance"

  it should "allow a String value to be round-tripped and then removed" in withTestStorage {
    (storage: Storage) =>
      val bytes: Array[Byte] = Array(0xDE, 0xAD, 0xBE, 0xEF).map(_.toByte)
      val hash: Array[Byte]  = MessageDigest.getInstance("SHA-256").digest(bytes)
      val testKey: Key       = Hash(hash)
      val testValue: String  = "Fiery the angels fell"

      val Right((putResult, getResult, removeResult)) = for {
        pr <- storage.put(testKey, testValue)
        gr <- storage.get[String](testKey)
        yr <- storage.remove(testKey)
      } yield (pr, gr, yr)

      putResult shouldBe (())
      getResult shouldBe testValue
      removeResult shouldBe true

      storage.get[String](testKey).left.value shouldBe NotFound
  }
}
