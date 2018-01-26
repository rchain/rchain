package coop.rchain.storage

import java.security.MessageDigest

import com.google.protobuf.ByteString
import coop.rchain.storage.models.Block
import org.scalatest.{EitherValues, FlatSpec, Matchers}

//noinspection ScalaUnnecessaryParentheses
class StorageTest extends FlatSpec with Matchers with EitherValues with StorageFixtures {

  object TestData {
    val bytes: Array[Byte] = Array(0xDE, 0xAD, 0xBE, 0xEF).map(_.toByte)
    val hash: Array[Byte]  = MessageDigest.getInstance("SHA-256").digest(bytes)
    val testKey: Key       = Hash(hash)
    val testValue: Block   = Block(value = ByteString.copyFrom(bytes))
  }

  behavior of "A Storage instance"

  it should "allow a value to be round-tripped and then removed" in withTestStorage {
    (storage: Storage) =>
      import TestData._

      for {
        putResult    <- storage.put(testKey, testValue)
        getResult    <- storage.get[Block](testKey)
        removeResult <- storage.remove(testKey)
      } {
        putResult shouldBe (())
        getResult shouldBe testValue
        removeResult shouldBe true
      }

      storage.get[Block](testKey).left.value shouldBe NotFound
  }
}
