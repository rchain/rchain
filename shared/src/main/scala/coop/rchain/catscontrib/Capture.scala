package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import monix.eval.Task

/** Monad with effect-capturing unit.
  *
  * Cribbed from [doobie](http://github.com/tpolecat/doobie)
  */
trait Capture[F[_]] {

  /** Captures the effect of producing `A`, including any exceptions that may
    * be thrown.
    */
  def capture[A](a: => A): F[A]

  /** Alias for `capture`. */
  def apply[A] = capture[A] _

  /** Construct a failed computation described by the given `Throwable`. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def fail[A](t: Throwable): F[A] = capture(throw t)

  def unattempt[A](fa: F[Either[Throwable, A]])(implicit F: Monad[F]): F[A] =
    fa >>= (_.fold(fail, F.pure(_)))
}

object Capture extends CaptureInstances {
  def apply[F[_]](implicit F: Capture[F]): Capture[F] = F
}

trait CaptureInstances extends CaptureInstances0 {
  implicit val taskCapture: Capture[Task] = new Capture[Task] {

    import monix.execution.Scheduler
    import scala.concurrent.Await
    import scala.concurrent.duration._
    import monix.execution.Scheduler.Implicits.global

    def capture[A](a: => A): Task[A] = Task.delay(a)
  }

  /** TEMP REMOVE once comm no longer imperative*/
  implicit def eitherCapture[E]: Capture[Either[E, ?]] = new Capture[Either[E, ?]] {
    def capture[A](a: => A): Either[E, ?][A] = Right(a)
  }

  implicit def idCapture: Capture[Id] = new Capture[Id] {
    def capture[A](a: => A): Id[A] = a
  }
  /** TEMP REMOVE END */
}

sealed trait CaptureInstances0 {
  import eitherT._
  import writerT._
  implicit def eitherTCapture[F[_]: Monad: Capture, E]: Capture[EitherT[F, E, ?]] =
    new TransCapture[F, EitherT[?[_], E, ?]](new TransUnsafeUncapture[EitherT[?[_], E, ?], F] {
      def unsafeUnlift[A]: EitherT[F, E, A] => F[A] = {
        case EitherT(fea) =>
          fea.map {
            case Right(a) => a
            case _        => throw new RuntimeException("dead kitty")
          }
      }
    })
}

private class TransCapture[F[_]: Monad: Capture, T[_[_], _]: MonadTrans](
    tuu: TransUnsafeUncapture[T, F]
) extends Capture[T[F, ?]] {
  def capture[A](a: => A) =
    MonadTrans[T].liftM(Capture[F].capture(a))
}

trait TransUnsafeUncapture[T[_[_], _], F[_]] {
  def unsafeUnlift[A]: T[F, A] => F[A]
}
