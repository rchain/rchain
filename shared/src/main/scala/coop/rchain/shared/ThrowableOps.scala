package coop.rchain.shared

object ThrowableOps {
  implicit class RichThrowable(th: Throwable) {
    def containsMessageWith(str: String): Boolean =
      if (th.getCause == null) th.getMessage.contains(str)
      else th.getMessage.contains(str) || th.getCause.containsMessageWith(str)
  }
}
