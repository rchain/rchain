package coop.rchain.rspace.concurrent
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib.TaskContrib._

class TwoStepLockTest extends FlatSpec with Matchers {

  import monix.execution.Scheduler
  implicit val s = Scheduler.fixedPool("test-scheduler", 8)

  "DefaultTwoStepLock" should "gate concurrent access to shared resources" in {
    val lock = new DefaultTwoStepLock[String]
    var a    = 0
    val t1   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a + 1 })
    val t2   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a - 3 })
    val t3   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a + 5 })
    val t4   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a - 8 })

    val r = Task.gatherUnordered(List(t1, t2, t3, t4))
    r.unsafeRunSync
  }

  def acquireLock(
      lock: TwoStepLock[String],
      a: List[String],
      b: List[String],
      update: => Unit
  ): Task[Unit] =
    Task { lock.acquire(a)(() => b)(update) }
}
