package coop.rchain.shared

object StringOps {
  implicit class StringColors(str: String) {
    def green: String     = s"\u001B[32m" + str + "\u001B[0m"
    def red: String       = s"\u001B[31m" + str + "\u001B[0m"
    def blue: String      = s"\u001B[34m" + str + "\u001B[0m"
    def isNumber: Boolean = str.matches("[+-]?\\d+.?\\d+")
  }
}
