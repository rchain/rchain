package coop.rchain.shared

import scala.util.Try

object StringOps {
  implicit class StringColors(str: String) {
    private def conditionalColor(coloring: String)(implicit tty: Boolean): String =
      if (tty)
        coloring + str + "\u001B[0m"
      else
        str

    def green(implicit tty: Boolean): String = conditionalColor("\u001B[32m")
    def red(implicit tty: Boolean): String   = conditionalColor("\u001B[31m")
    def blue(implicit tty: Boolean): String  = conditionalColor("\u001B[34m")
    def isNumber: Boolean                    = str.matches("[+-]?\\d+.?\\d+")
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
