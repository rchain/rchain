package coop.rchain.sdk

trait ThrowableSyntax {
  implicit def syntaxThrowable(x: Throwable): ThrowableOps = new ThrowableOps(x)
}

final class ThrowableOps(private val x: Throwable) extends AnyVal {
  def getMessageSafe: String =
    if (x.getMessage == null) x.toString else x.getMessage
}
