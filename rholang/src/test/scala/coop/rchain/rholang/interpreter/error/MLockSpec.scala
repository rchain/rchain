package coop.rchain.rholang.interpreter.error

import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import org.scalatest.FlatSpec

import scala.concurrent.duration._
import scala.util.{Failure, Success}

class MLockSpec extends FlatSpec {

  implicit val scheduler: TestScheduler = TestScheduler()

  private val MAX_DURATION = 5.seconds

  def withFixture[A](f: MLock[Task] => A): A = f(MLock[Task].runSyncUnsafe(MAX_DURATION))

  behavior of "MLock"

  "release" should "throw error when released lock is released" in withFixture { lock =>
    val a = lock.release.runToFuture

    scheduler.tick()
    assert(a.value.contains(Failure(LockReleaseException("attempted to release released lock"))))
  }

  "release" should "unblock lock acquisitions in FIFO order" in withFixture { lock =>
    val a = lock.acquire.runToFuture
    val b = lock.acquire.delayExecution(1.second).runToFuture
    val c = lock.acquire.delayExecution(2.second).runToFuture
    val d = lock.release.delayExecution(3.second).runToFuture
    val e = lock.release.delayExecution(4.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(b.value.isEmpty)

    scheduler.tick(1.second)
    assert(c.value.isEmpty)

    scheduler.tick(1.second)
    assert(d.value.contains(Success(())))
    assert(b.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(e.value.contains(Success(())))
    assert(c.value.contains(Success(())))
  }

  "withLock" should "block lock acquisitions while lock is held" in withFixture { lock =>
    val a = lock.withLock(Task.unit.delayExecution(2.second)).runToFuture
    val b = lock.acquire.delayExecution(1.second).runToFuture

    scheduler.tick()
    assert(a.value.isEmpty)

    scheduler.tick(1.second)
    assert(b.value.isEmpty)

    scheduler.tick(1.second)
    assert(a.value.contains(Success(())))
    assert(b.value.contains(Success(())))
  }

  "release" should "unblock all waiting threads simultaneously" in withFixture { lock =>
    val a = lock.acquire.runToFuture
    val b = lock.waitForLock.delayExecution(1.second).runToFuture
    val c = lock.waitForLock.delayExecution(1.second).runToFuture
    val d = lock.release.delayExecution(2.second).runToFuture

    scheduler.tick()
    assert(a.value.contains(Success(())))

    scheduler.tick(1.second)
    assert(b.value.isEmpty)
    assert(c.value.isEmpty)

    scheduler.tick(1.second)
    assert(d.value.contains(Success(())))
    assert(b.value.contains(Success(())))
    assert(c.value.contains(Success(())))
  }
}
