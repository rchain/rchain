package coop.rchain.sdk.primitive

import scala.util.{Failure, Success, Try}

trait TrySyntax {
  implicit def sdkSyntaxTry[A](tryExp: Try[A]): TryOps[A] = new TryOps[A](tryExp)
}

final class TryOps[A](private val tryExp: Try[A]) extends AnyVal {

  /**
    * Get value or throw exception in case of [[Failure]].
    */
  @SuppressWarnings(Array("org.wartremover.warts.TryPartial"))
  def getUnsafe: A = tryExp.get

  /**
    * Map for failure case (exception).
    */
  def mapFailure(f: Throwable => Throwable): Try[A] =
    tryExp.transform(Success(_), ex => Failure(f(ex)))
}
