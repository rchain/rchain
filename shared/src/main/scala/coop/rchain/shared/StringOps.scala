package coop.rchain.shared

import scala.util.Try

object StringOps {
  final case class ColoredString(str: String, color: String) {
    def colorize: String = color + str + "\u001B[0m"
  }

  implicit class StringColors(str: String) {
    def green: ColoredString = ColoredString(str, "\u001B[32m")
    def red: ColoredString   = ColoredString(str, "\u001B[31m")
    def blue: ColoredString  = ColoredString(str, "\u001B[34m")
    def isNumber: Boolean    = str.matches("[+-]?\\d+.?\\d+")
  }

  implicit class BracesOps(expr: String) {
    // Wrap if it's sth more than just a number
    def wrapWithBraces: String =
      Try(Integer.valueOf(expr)).fold(
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
