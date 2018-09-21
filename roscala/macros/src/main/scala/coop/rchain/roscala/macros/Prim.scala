package coop.rchain.roscala.macros

import scala.language.experimental.macros
import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.reflect.macros.whitebox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class checkTypeMismatch[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TypeMismatchMacro.impl[A]
}

object TypeMismatchMacro {
  def impl[A](c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val typeParam = c.macroApplication.filter {
      case Ident(_) => true
      case _        => false
    }(1)

    val result =
      annottees.map(_.tree).toList match {
        case q"$mods def fnSimple(ctxt: Ctxt): $returnType = { ..$body }" :: Nil =>
          q"""$mods def fnSimple(ctxt: Ctxt): $returnType =  {
              mismatchType[$typeParam](ctxt) match {
                case Some(typeMismatch) => Left(typeMismatch)
                case None => {..$body}
              }
          }"""

        case _ =>
          c.abort(
            c.enclosingPosition,
            "Annotation @checkTypeMismatch can only be used on Prim.fnSimple"
          )
      }

    c.Expr[Any](result)
  }
}
