package coop.rchain.rspace.concurrent

import org.scalatest._

import monix.eval.Task

class MultiLockTest extends FlatSpec with Matchers {

  import monix.execution.Scheduler
  implicit val s = Scheduler.fixedPool("Maciej", 8)

  implicit class TaskOps[A](task: Task[A])(implicit scheduler: Scheduler) {
    import scala.concurrent.Await
    import scala.concurrent.duration._

    def unsafeRunSync: A =
      Await.result(task.runAsync, Duration.Inf)
  }

  "DefaultMultiLock" should "not allow concurrent modifications of same keys" in {
    val tested = new DefaultMultiLock[String]()

    val m = scala.collection.mutable.Map.empty[String, Int]

    def acquire(seq: Seq[String]) = Task.delay {
      tested.acquire(seq) {
        for {
          k <- seq
          v = m.getOrElse(k, 0) + 1
          _ = m.put(k, v)
        } yield ()
        //Thread.sleep(10000)
      }
    }

    (for {
      _ <- acquire(Seq("a", "b")).fork
      _ <- acquire(Seq("b", "c")).fork
      _ <- acquire(Seq("c", "a")).fork
      _ <- acquire(Seq("a", "c")).fork
      _ <- acquire(Seq("c", "d")).fork
    } yield ()).unsafeRunSync

    m.toList should contain theSameElementsAs (Map("d" -> 1, "b" -> 2, "c" -> 4, "a" -> 3).toList)

  }
}
