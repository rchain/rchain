package coop.rchain.rspace.concurrent

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection._
import scala.collection.immutable.Seq
import coop.rchain.metrics
import coop.rchain.metrics.Metrics

class MultiLockTest extends AnyFlatSpec with Matchers {

  import monix.execution.Scheduler
  implicit val s       = Scheduler.fixedPool("test-scheduler", 8)
  implicit val metrics = new Metrics.MetricsNOP[IO]

  implicit class TaskOps[A](task: IO[A]) {
    import scala.concurrent.Await
    import scala.concurrent.duration._

    def unsafeRunSync: A =
      Await.result(task.unsafeToFuture(), Duration.Inf)
  }

  val tested = new MultiLock[IO, String](Metrics.BaseSource)

  def acquire(m: mutable.Map[String, Int])(seq: Seq[String]) =
    tested.acquire(seq) {
      IO.delay {
        for {
          k <- seq
          v = m.getOrElse(k, 0) + 1
          _ = m.put(k, v)
        } yield ()
      }
    }

  "DefaultMultiLock" should "not allow concurrent modifications of same keys" in {

    val m = mutable.Map.empty[String, Int]

    (for {
      f1 <- acquire(m)(Seq("a", "b")).start
      f2 <- acquire(m)(Seq("d", "c")).start
      f3 <- acquire(m)(Seq("a", "c")).start
      f4 <- acquire(m)(Seq("c", "a")).start
      f5 <- acquire(m)(Seq("a", "c")).start
      f6 <- acquire(m)(Seq("a", "d")).start
      _  <- f1.join
      _  <- f2.join
      _  <- f3.join
      _  <- f4.join
      _  <- f5.join
      _  <- f6.join
    } yield ()).unsafeRunSync

    m.toList should contain theSameElementsAs (Map("d" -> 2, "b" -> 1, "c" -> 4, "a" -> 5).toList)

  }

  it should "release locks upon error in the thunk" in {

    val m = mutable.Map.empty[String, Int]

    (for {
      _ <- acquire(m)(Seq("a", "b"))
      _ <- IO.delay {
            tested.acquire(Seq("a", "c")) { throw new Exception() }
          }.attempt
      _ <- acquire(m)(Seq("a", "c"))
    } yield ()).unsafeRunSync

    m.toList should contain theSameElementsAs (Map("a" -> 2, "b" -> 1, "c" -> 1).toList)
  }

  it should "not try to lock channels with same name twice" in {

    val m = mutable.Map.empty[String, Int]

    (for {
      _ <- acquire(m)(Seq("a", "a"))
    } yield ()).unsafeRunSync

    m.toList should contain theSameElementsAs (Map("a" -> 2).toList)
  }

  "FunctionalMultiLock" should "not allow concurrent modifications of same keys" in {
    import cats.effect.{Async, IO}
    import cats.implicits._

    implicit val metrics: Metrics.MetricsNOP[IO] = new Metrics.MetricsNOP[IO]

    val tested = new MultiLock[IO, String](Metrics.BaseSource)

    val m = scala.collection.mutable.Map.empty[String, Int]

    def acquire(seq: List[String]) =
      tested.acquire(seq) {
        seq.pure[IO].map { keys =>
          for {
            k <- keys
            v = m.getOrElse(k, 0) + 1
            _ = m.put(k, v)
          } yield ()
        }
      }

    (for {
      _ <- acquire(List("a", "b"))
      _ <- acquire(List("d", "c"))
      _ <- acquire(List("a", "c"))
      _ <- acquire(List("c", "a"))
      _ <- acquire(List("a", "c"))
      _ <- acquire(List("a", "d"))
    } yield ()).unsafeRunSync

    m.toList should contain theSameElementsAs Map("d" -> 2, "b" -> 1, "c" -> 4, "a" -> 5).toList

  }
}
