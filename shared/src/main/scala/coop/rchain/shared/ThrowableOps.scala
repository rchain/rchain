package coop.rchain.shared

object ThrowableOps {
  implicit class RichThrowable(th: Throwable) {
    def containsMessageWith(str: String): Boolean =
      (Option(th.getCause), Option(th.getMessage)) match {
        case (_, Some(msg)) if msg.contains(str) => true
        case (Some(cause), _)                    => cause.containsMessageWith(str)
        case _                                   => false
      }
  }
}
