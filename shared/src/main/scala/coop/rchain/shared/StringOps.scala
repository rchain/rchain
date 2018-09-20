package coop.rchain.shared

import scala.util.Try

object StringOps {
  implicit class StringColors(str: String) {
    def green: String     = s"\u001B[32m" + str + "\u001B[0m"
    def red: String       = s"\u001B[31m" + str + "\u001B[0m"
    def blue: String      = s"\u001B[34m" + str + "\u001B[0m"
    def isNumber: Boolean = str.matches("[+-]?\\d+.?\\d+")
  }

  implicit class BracesOps(expr: String) {
    // Wrap if it's sth more than just a number
    def wrapWithBraces: String =
      Try(new Integer(expr)).fold(
        _ =>
          if (expr.startsWith("(") && expr.endsWith(")")) {
            expr
          } else {
            s"($expr)"
          },
        _ => expr
      )
  }
}
