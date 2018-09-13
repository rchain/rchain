package coop.rchain.rspace.concurrent

import org.scalatest._

import monix.eval.Task

class MultiLockTest extends FlatSpec with Matchers {

  import monix.execution.Scheduler
  implicit val s = Scheduler.fixedPool("test-scheduler", 8)

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
      f1 <- acquire(Seq("a", "b")).fork
      f2 <- acquire(Seq("d", "c")).fork
      f3 <- acquire(Seq("a", "c")).fork
      f4 <- acquire(Seq("c", "a")).fork
      f5 <- acquire(Seq("a", "c")).fork
      f6 <- acquire(Seq("a", "d")).fork
      _  <- f1.join
      _  <- f2.join
      _  <- f3.join
      _  <- f4.join
      _  <- f5.join
      _  <- f6.join
    } yield ()).unsafeRunSync

    m.toList should contain theSameElementsAs (Map("d" -> 2, "b" -> 1, "c" -> 4, "a" -> 5).toList)

  }

  import cats.effect.{Concurrent, IO}
  "FunctionalMultiLock" should "not allow concurrent modifications of same keys" in {
    val tested = new FunctionalMultiLock[IO, String]()

    val m = scala.collection.mutable.Map.empty[String, Int]

    def acquire(seq: List[String]) =
      tested.acquire(seq) {
        for {
          k <- seq
          v = m.getOrElse(k, 0) + 1
          _ = m.put(k, v)
        } yield ()
        //Thread.sleep(10000)
      }

    (for {
      _ <- acquire(List("a", "b")) //.fork
      _ <- acquire(List("d", "c")) //.fork
      _ <- acquire(List("a", "c")) //.fork
      _ <- acquire(List("c", "a")) //.fork
      _ <- acquire(List("a", "c")) //.fork
      _ <- acquire(List("a", "d")) //.fork
    } yield ()).unsafeRunSync

    m.toList should contain theSameElementsAs (Map("d" -> 2, "b" -> 1, "c" -> 4, "a" -> 5).toList)

  }
}
