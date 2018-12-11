package coop.rchain.shared

object TerminalMode {
  def readMode: Boolean =
    if (System.console() != null)
      true
    else
      false
}
