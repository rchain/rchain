package coop.rchain.shared

import cats.Id
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.testkit.TestControl
import cats.effect.{Async, IO, Ref}
import cats.syntax.all._
import coop.rchain.shared.syntax.sharedSyntaxFs2Stream
import fs2.Stream
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class Fs2ExtensionsSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  /**
    * Creates a Stream of 2 elements creating String "11", if timeout occurs it will insert zeroes e.g. "101"
    */
  def test[F[_]: Async](timeout: FiniteDuration): F[String] = Ref.of("") flatMap { st =>
    val addOne  = Stream.eval(st.updateAndGet(_ + "1"))
    val pause   = Stream.sleep[F](1.second).drain
    val addZero = st.update(_ + "0")

    (addOne ++ pause ++ addOne).evalOnIdle(addZero, timeout).compile.lastOrError
  }

  // Helper to construct success result
  def success[A](a: A): Option[Succeeded[Id, Throwable, A]] = Succeeded[Id, Throwable, A](a).some

  "evalOnIdle" should "NOT trigger timeout if element IS produced within timeout period" in {
    for {
      t <- TestControl.execute(test[IO](1001.millis))

      // Sanity check, value should be empty before start
      _ <- t.results.map(_ shouldBe none)

      // Just before the next element produced, still no value
      _ <- t.tickFor(999.millis)
      _ <- t.results.map(_ shouldBe none)

      _ <- t.tickFor(1.millis)
      _ <- t.results.map(_ shouldBe success("11"))
    } yield ()
  }

  it should "trigger timeout if element is NOT produced within timeout" in {
    for {
      t <- TestControl.execute(test[IO](750.millis))

      // Sanity check, value should be empty before start
      _ <- t.results.map(_ shouldBe none)

      // Just before the next element produced, still no value
      _ <- t.tickFor(999.millis)
      _ <- t.results.map(_ shouldBe none)

      _ <- t.tickFor(1.millis)
      _ <- t.results.map(_ shouldBe success("101"))
    } yield ()
  }

  it should "trigger two timeouts if element is NOT produced and timeout is double time shorter" in {
    for {
      t <- TestControl.execute(test[IO](499.millis))

      // Sanity check, value should be empty before start
      _ <- t.results.map(_ shouldBe none)

      // Just before the next element produced, still no value
      _ <- t.tickFor(999.millis)
      _ <- t.results.map(_ shouldBe none)

      _ <- t.tickFor(1.millis)
      _ <- t.results.map(_ shouldBe success("1001"))
    } yield ()
  }

}
