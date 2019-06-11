package coop.rchain.rholang.interpreter.error

import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import org.scalatest.FlatSpec

import scala.concurrent.duration._
import scala.util.{Failure, Success}

class RWLockSpec extends FlatSpec {

  implicit val scheduler: TestScheduler = TestScheduler()

  val MAX_DURATION: FiniteDuration = 5.seconds

  def withFixture[A](f: RWLock[Task] => A): A = f(RWLock[Task].runSyncUnsafe(MAX_DURATION))

  behavior of "RWLock"

  "release write, release read" should "throw error when released lock is released" in withFixture {
    rwLock =>
      val a = rwLock.releaseRead.runToFuture
      val b = rwLock.releaseWrite.runToFuture

      scheduler.tick()
      assert(
        a.value.contains(Failure(LockReleaseException("attempted to release released read lock")))
      )
      assert(
        b.value.contains(Failure(LockReleaseException("attempted to release released write lock")))
      )
  }

  // TODO: How to assert that rwLock.stateRef has correct value w/o making stateRef public?
  "acquire read; acquire read" should "not block" in withFixture { rwLock =>
    val a = rwLock.acquireRead.runToFuture
    val b = rwLock.acquireRead.delayExecution(1.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(b.value.contains(Success(())))
  }

  "acquire read; acquire write" should "block until release read" in withFixture { rwLock =>
    val a = rwLock.acquireRead.runToFuture
    val b = rwLock.acquireWrite.delayExecution(1.second).runToFuture
    val c = rwLock.releaseRead.delayExecution(2.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.seconds)
    assert(b.value.isEmpty)

    scheduler.tick(1.seconds)
    assert(c.value.contains(Success(())))
    assert(b.value.contains(Success(())))
  }

  "acquire write; acquire read" should "block until release write" in withFixture { rwLock =>
    val a = rwLock.acquireWrite.runToFuture
    val b = rwLock.acquireRead.delayExecution(1.second).runToFuture
    val c = rwLock.releaseWrite.delayExecution(2.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(b.value.isEmpty)

    scheduler.tick(1.second)
    assert(c.value.contains(Success(())))
    assert(b.value.contains(Success(())))
  }

  "acquire write; acquire write" should "block until release write" in withFixture { rwLock =>
    val a = rwLock.acquireWrite.runToFuture
    val b = rwLock.acquireWrite.delayExecution(1.second).runToFuture
    val c = rwLock.releaseWrite.delayExecution(2.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(b.value.isEmpty)

    scheduler.tick(1.second)
    assert(c.value.contains(Success(())))
    assert(b.value.contains(Success(())))
  }

  "withReadLock(withReadLock(fa))" should "throw error" in withFixture { rwLock =>
    val a = rwLock.withReadLock(rwLock.withReadLock(Task.unit)).runToFuture

    scheduler.tick()
    assert(a.value.contains(Failure(LockReleaseException("attempted to release released lock"))))
  }

  "withReadLock(withWriteLock(fa))" should "block" in withFixture { rwLock =>
    val a = rwLock.withReadLock(rwLock.withWriteLock(Task.unit)).runToFuture

    scheduler.tick()
    assert(a.value.isEmpty)
  }

  "withWriteLock(withReadLock(fa))" should "block" in withFixture { rwLock =>
    val a = rwLock.withWriteLock(rwLock.withReadLock(Task.unit)).runToFuture

    scheduler.tick()
    assert(a.value.isEmpty)
  }

  "withWriteLock(withWriteLock(fa))" should "block" in withFixture { rwLock =>
    val a = rwLock.withWriteLock(rwLock.withWriteLock(Task.unit)).runToFuture

    scheduler.tick()
    assert(a.value.isEmpty)
  }

  "tryAcquireRead" should "return true when read lock is free" in withFixture { rwLock =>
    val a = rwLock.tryAcquireRead.runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(true)))
  }

  "tryAcquireRead" should "return false when read lock is not free" in withFixture { rwLock =>
    val a = rwLock.acquireWrite.runToFuture
    val b = rwLock.tryAcquireRead.delayExecution(1.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(b.value.contains(Success(false)))
  }

  "tryAcquireWrite" should "return true when write lock is free" in withFixture { rwLock =>
    val a = rwLock.tryAcquireWrite.runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(true)))
  }

  "tryAcquireWrite" should "return false when write lock is not free" in withFixture { rwLock =>
    val a = rwLock.acquireWrite.runToFuture
    val b = rwLock.tryAcquireWrite.delayExecution(1.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(b.value.contains(Success(false)))
  }

  "tryWithRead" should "return Some when read lock is free" in withFixture { rwLock =>
    val a = rwLock.tryWithRead(Task.unit).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(Some(()))))
  }

  "tryWithRead" should "return None when read lock is not free" in withFixture { rwLock =>
    val a = rwLock.acquireWrite.runToFuture
    val b = rwLock.tryWithRead(Task.unit).delayExecution(1.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(b.value.contains(Success(None)))
  }

  "tryWithWrite" should "return Some when write lock is free" in withFixture { rwLock =>
    val a = rwLock.tryWithWrite(Task.unit).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(Some(()))))
  }

  "tryWithWrite" should "return None when write lock is not free" in withFixture { rwLock =>
    val a = rwLock.acquireWrite.runToFuture
    val b = rwLock.tryWithWrite(Task.unit).delayExecution(1.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(b.value.contains(Success(None)))
  }
}
