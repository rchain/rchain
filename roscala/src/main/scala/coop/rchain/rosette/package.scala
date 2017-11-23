package coop.rchain

import coop.rchain.rosette.parser.bytecode.ParseError
import reflect.runtime.universe._
import reflect.runtime.currentMirror
import scala.annotation.tailrec

package object rosette {
  type Result = Either[RblError, Ob]

  def suicide(msg: String): Unit = {
    System.err.println(s"*** fatal error: $msg")
    System.exit(1)
  }

  trait Show[A] {
    def show(a: A): String
  }

  object Show {
    def apply[A](implicit sh: Show[A]): Show[A] = sh

    def show[A: Show](a: A): String = Show[A].show(a)

    implicit class ShowOps[A: Show](a: A) {
      def show: String = Show[A].show(a)
    }

    implicit val parseErrorShow: Show[ParseError] =
      _ match {
        case e => e.toString
      }

    implicit val opsShow: Show[Seq[Op]] = { ops =>
      ops
        .map { op =>
          val r = currentMirror.reflect(op)

          val name = op.getClass.getSimpleName
          val args = r.symbol.typeSignature.members.toStream
            .collect { case s: TermSymbol if !s.isMethod => r.reflectField(s) }
            .map { r =>
              r.symbol.name.toString.trim + ":" + r.get
            }

          name + " " + args.mkString(" ")
        }
        .mkString("\n")
    }
  }

  @tailrec
  def recMap[A](count: Int, l: A)(f: A => A): A = {
    if (count == 0) l else recMap(count-1, f(l))(f)
  }
}
