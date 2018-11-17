package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.{Applicative, Functor, Id, Monad, Parallel}
import cats.implicits._
import coop.rchain.catscontrib.effect.implicits._
import monix.eval.Task
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

class ErrorLogSpec extends FlatSpec with PropertyChecks with Matchers {
  private implicit val throwableOrdering: Ordering[Throwable] = Ordering.by(_.toString)

  behavior of "ErrorLog[Id]"

  "Tell" should "store errors in the log" in {
    forAll { errors: Vector[Throwable] =>
      errors.sorted should be {
        testTell[Id, Id](errors).sorted
      }

      errors.sorted should be {
        testTell[Task, Task.Par](errors).runSyncUnsafe(10.seconds).sorted
      }
    }
  }

  "ErrorLog initial vector and after readAndClearErrorVector" should "be empty" in {
    forAll { errors: Vector[Throwable] =>
      (Vector.empty[Throwable], Vector.empty[Throwable]) should be {
        testBeforeAndAfterConditions[Id, Id](errors)
      }

      (Vector.empty[Throwable], Vector.empty[Throwable]) should be {
        testBeforeAndAfterConditions[Task, Task.Par](errors).runSyncUnsafe(10.seconds)
      }
    }
  }

  def testTell[M[_]: Sync, F[_]](errors: Vector[Throwable])(
      implicit parallelMF: Parallel[M, F]
  ): M[Vector[Throwable]] =
    for {
      errorLog <- ErrorLog.create[M]

      _ <- errors.toStream.parTraverse_[M, F, Unit](errorLog.tell)

      storedErrors <- errorLog.readAndClearErrorVector()
    } yield storedErrors.sortBy(_.toString)

  def testBeforeAndAfterConditions[M[_]: Sync, F[_]](errors: Vector[Throwable])(
      implicit parallelMF: Parallel[M, F]
  ): M[(Vector[Throwable], Vector[Throwable])] =
    for {
      errorLog <- ErrorLog.create[M]

      initialErrors <- errorLog.readAndClearErrorVector()

      _           <- errors.toStream.parTraverse_[M, F, Unit](errorLog.tell)
      _           <- errorLog.readAndClearErrorVector()
      finalErrors <- errorLog.readAndClearErrorVector()
    } yield (initialErrors, finalErrors)
}
