package coop.rchain.shared

object TerminalMode {
  def readMode: Boolean = System.console() != null
}
