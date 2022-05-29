package coop.rchain.sdk.primitive

trait ThrowableSyntax {
  implicit def sdkSyntaxThrowable(ex: Throwable): ThrowableOps = new ThrowableOps(ex)
}

final class ThrowableOps(private val ex: Throwable) extends AnyVal {

  /**
    * Get exception message or exception type if message is null
    */
  def getMessageSafe: String =
    if (ex.getMessage == null) ex.toString else ex.getMessage
}
