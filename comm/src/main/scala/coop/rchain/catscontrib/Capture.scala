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

sealed abstract class CaptureInstances {
  implicit val taskCapture: Capture[Task] = new Capture[Task] {
    def capture[A](a: => A): Task[A] = Task.delay(a)
  }
}
