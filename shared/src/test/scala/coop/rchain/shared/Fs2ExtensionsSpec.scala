package coop.rchain.shared

import cats.effect.{Async, IO}
import cats.syntax.all._
import coop.rchain.shared.syntax.sharedSyntaxFs2Stream
import fs2.Stream
import monix.execution.schedulers.TestScheduler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Success
import cats.effect.unsafe.implicits.global
import cats.effect.Ref
import cats.effect.testkit.TestControl

class Fs2ExtensionsSpec extends AnyFlatSpec with Matchers {

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
  def success[A](a: A): Option[Success[A]] = Success(a).some

//  "evalOnIdle" should "NOT trigger timeout if element IS produced within timeout period" in {
//    val t = TestControl.execute(test[IO](1001.millis))
//
//    // Sanity check, value should be empty before start
//    t.unsafeRunSync() shouldBe none
//
//    // Just before the next element produced, still no value
//    t.flatMap(_.advance(999.millis)).unsafeRunSync() shouldBe none
//
//    //
//    t.flatMap(_.advance(1000.millis)).unsafeToFuture() shouldBe success("11")
//  }
//
//  it should "trigger timeout if element is NOT produced within timeout" in {
//    val t = TestControl.execute(test[IO](750.millis))
//
//    // Sanity check, value should be empty before start
//    t.unsafeRunSync() shouldBe none
//
//    // Just before the next element produced, still no value
//    t.flatMap(_.advance(999.millis)).unsafeRunSync() shouldBe none
//
//    t.flatMap(_.advance(1000.millis)).unsafeRunSync() shouldBe success("101")
//  }
//
//  it should "trigger two timeouts if element is NOT produced and timeout is double time shorter" in {
//    val t = TestControl.execute(test[IO](499.millis))
//
//    // Sanity check, value should be empty before start
//    t.unsafeRunSync() shouldBe none
//
//    // Just before the next element produced, still no value
//    t.flatMap(_.advance(999.millis)).unsafeRunSync() shouldBe none
//
//    t.flatMap(_.advance(1000.millis)).unsafeRunSync() shouldBe success("1001")
//  }

}
