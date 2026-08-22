package coop.rchain.sdk.error

import coop.rchain.sdk.syntax.all._

/**
  * Represents the error which should not be handled and should cause the main process to exit.
  *
  * It extends [[Throwable]] directly to indicate that is "lower" level error then [[Exception]].
  *
  * Note: it's still caught by [[scala.util.control.NonFatal]] extractor.
  */
final class FatalError(message: String) extends Throwable(message) {
  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause).void()
  }
}

object FatalError {

  /**
    * Creates [[FatalError]] with a message.
    *
    * @param message error description
    */
  def apply(message: => String): FatalError = new FatalError(message)

  /**
    * Creates [[FatalError]] with a message and chain with lower level error.
    *
    * @param message error description
    * @param cause underlying error causing this error
    */
  def apply(message: => String, cause: => Throwable): FatalError = new FatalError(message, cause)
}
