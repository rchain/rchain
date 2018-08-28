package coop.rchain.shared

import org.scalatest.{FlatSpec, Matchers}

class SyncLockTest extends FlatSpec with Matchers {
  "SyncLock.trylock" should "return true on the first invokation" in {
    val lock = new SyncLock

    lock.tryLock shouldBe true
  }

  it should "return false on all invokations after the first, until unlock is called" in {
    val lock = new SyncLock
    val _    = lock.tryLock

    lock.tryLock shouldBe false
    lock.tryLock shouldBe false
    lock.tryLock shouldBe false
    lock.tryLock shouldBe false

    lock.unlock
    lock.tryLock shouldBe true
    lock.tryLock shouldBe false
  }
}
