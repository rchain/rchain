package coop.rchain.storage

import java.security.MessageDigest

import com.google.protobuf.ByteString
import coop.rchain.storage.models.{Block, Contract, SystemContract}
import org.scalatest.{EitherValues, FlatSpec, Matchers}

//noinspection ScalaUnnecessaryParentheses
class StorageTest extends FlatSpec with Matchers with EitherValues with StorageFixtures {

  behavior of "A Storage instance"

  it should "allow a Block value to be round-tripped and then removed" in withTestStorage {
    (storage: Storage) =>
      val bytes: Array[Byte] = Array(0xDE, 0xAD, 0xBE, 0xEF).map(_.toByte)
      val hash: Array[Byte]  = MessageDigest.getInstance("SHA-256").digest(bytes)
      val testKey: Key       = Hash(hash)
      val testValue: Block   = Block(value = ByteString.copyFrom(bytes))

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

  it should "allow a Contract value to be round-tripped and then removed" in withTestStorage {
    (storage: Storage) =>
      val bytes: Array[Byte]  = Array(0xDE, 0xAD, 0xBE, 0xEF).map(_.toByte)
      val testKey: Key        = Flat("quux")
      val testValue: Contract = Contract(value = ByteString.copyFrom(bytes))

      for {
        putResult    <- storage.put(testKey, testValue)
        getResult    <- storage.get[Contract](testKey)
        removeResult <- storage.remove(testKey)
      } {
        putResult shouldBe (())
        getResult shouldBe testValue
        removeResult shouldBe true
      }

      storage.get[Contract](testKey).left.value shouldBe NotFound
  }

  it should "allow a SystemContract value to be round-tripped and then removed" in withTestStorage {
    (storage: Storage) =>
      val bytes: Array[Byte]        = Array(0xDE, 0xAD, 0xBE, 0xEF).map(_.toByte)
      val testKey: Key              = FlatSys("quux")
      val testValue: SystemContract = SystemContract(value = ByteString.copyFrom(bytes))

      for {
        putResult    <- storage.put(testKey, testValue)
        getResult    <- storage.get[SystemContract](testKey)
        removeResult <- storage.remove(testKey)
      } {
        putResult shouldBe (())
        getResult shouldBe testValue
        removeResult shouldBe true
      }

      storage.get[SystemContract](testKey).left.value shouldBe NotFound
  }
}
