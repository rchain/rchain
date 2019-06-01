package coop.rchain.shared

object Matcher {
  final case class WithPrefix(prefix: String) {
    def unapply(s: String): Option[String] =
      if (s.startsWith(prefix))
        Some(s.substring(prefix.length))
      else None
  }
}
