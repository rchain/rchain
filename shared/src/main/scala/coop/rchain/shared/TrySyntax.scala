package coop.rchain.shared

import scala.util.{Failure, Success, Try}

object TrySyntax {
  implicit final class TryExt[A](val t: Try[A]) extends AnyVal {

    // Wrap exception in failure
    def mapFailure(f: Throwable => Throwable): Try[A] =
      t.transform(Success(_), ex => Failure(f(ex)))

  }
}
