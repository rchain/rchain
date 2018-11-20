package coop.rchain.shared

import cats.implicits._
import cats.data.EitherT
import cats.effect.{ExitCase, Sync}
import cats.effect.concurrent.Ref
import coop.rchain.catscontrib.SyncInstances.syncEffect
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.duration._

class SyncInstancesSpec extends FunSpec with Matchers {

  type Effect[A] = EitherT[Task, Throwable, A]
  def mkEffect[A](t: Task[A]): Effect[A] = EitherT[Task, Throwable, A](t.attempt)

  implicit val sync: Sync[Effect] = syncEffect[Throwable](identity, identity)
  
  describe("Sync[Effect].brackedCase") {

    it("should call release in case of error") {
      val ex = new Exception()

      val (result, exitValue, exitCase) =
        runTest[Int](
          1,
          _ => Task.raiseError[Int](ex)
        ).runSyncUnsafe(1.second)

      result should be(Left(ex))
      exitValue should be(Some(1))
      exitCase should be(Some(ExitCase.error(ex)))
    }

    it("should call release with the return value in case of success") {
      val (result, exitValue, exitCase) =
        runTest[Int](1, x => Task.delay(x + 1))
          .runSyncUnsafe(1.second)

      result should be(Right(2))
      exitCase should be(Some(ExitCase.complete))
      exitValue should be(Some(1))
    }

    def runTest[A](
        init: A,
        body: A => Task[A]
    ): Task[(Either[Throwable, A], Option[A], Option[ExitCase[Throwable]])] = {
      val acquireEff = mkEffect(Task.delay(init))

      def bodyEff(a: A) = mkEffect(body(a))

      for {
        exitValueRef <- Ref.of[Task, Option[A]](None)
        exitCaseRef  <- Ref.of[Task, Option[ExitCase[Throwable]]](None)

        r <- Sync[Effect]
              .bracketCase(acquireEff)(bodyEff)(
                (v: A, x: ExitCase[Throwable]) =>
                  mkEffect(for {
                    _ <- exitValueRef.set(Some(v))
                    _ <- exitCaseRef.set(Some(x))
                  } yield ())
              )
              .value
              .attempt

        ev <- exitValueRef.get
        ec <- exitCaseRef.get
      } yield (r.joinRight, ev, ec)
    }
  }
}
