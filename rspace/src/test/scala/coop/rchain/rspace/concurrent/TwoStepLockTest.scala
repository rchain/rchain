package coop.rchain.rspace.concurrent

import monix.eval.Task
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.Metrics

class TwoStepLockTest extends AnyFlatSpec with Matchers {

  import monix.execution.Scheduler
  implicit val s       = Scheduler.fixedPool("test-scheduler", 8)
  implicit val metrics = new Metrics.MetricsNOP[Task]

  "DefaultTwoStepLock" should "gate concurrent access to shared resources" in {
    val lock = new ConcurrentTwoStepLockF[Task, String](Metrics.BaseSource)
    var a    = 0
    val t1   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a + 1 })
    val t2   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a - 3 })
    val t3   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a + 5 })
    val t4   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a - 8 })

    val r = Task.parSequenceUnordered(List(t1, t2, t3, t4))
    r.unsafeRunSync
  }

  def acquireLock(
      lock: TwoStepLock[Task, String],
      a: List[String],
      b: List[String],
      update: => Unit
  ): Task[Unit] =
    lock.acquire(a)(() => Task.delay(b))(Task.delay(update))
}
