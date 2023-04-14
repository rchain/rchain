package coop.rchain.rspace.concurrent

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import coop.rchain.metrics.Metrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.all._

class TwoStepLockTest extends AnyFlatSpec with Matchers {

  implicit val metrics = new Metrics.MetricsNOP[IO]

  "DefaultTwoStepLock" should "gate concurrent access to shared resources" in {
    val lock = new ConcurrentTwoStepLockF[IO, String](Metrics.BaseSource)
    var a    = 0
    val t1   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a + 1 })
    val t2   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a - 3 })
    val t3   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a + 5 })
    val t4   = acquireLock(lock, List("a", "b"), List("w1", "w2"), { a = a - 8 })

    val r = List(t1, t2, t3, t4).parSequence
    r.unsafeRunSync
  }

  def acquireLock(
      lock: TwoStepLock[IO, String],
      a: List[String],
      b: List[String],
      update: => Unit
  ): IO[Unit] =
    lock.acquire(a)(() => IO.delay(b))(IO.delay(update))
}
