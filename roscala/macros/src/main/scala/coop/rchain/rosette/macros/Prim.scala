package coop.rchain.rosette.macros

import scala.language.experimental.macros
import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.reflect.macros.whitebox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class checkArgumentMismatch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ArgumentMismatchMacro.impl
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class checkTypeMismatch[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TypeMismatchMacro.impl[A]
}

object ArgumentMismatchMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val result =
      annottees.map(_.tree).toList match {
        case q"$mods def fn(ctxt: Ctxt): $returnType = { ..$body }" :: Nil =>
          q"""$mods def fn(ctxt: Ctxt): $returnType =  {
              if(ctxt.nargs < minArgs || ctxt.nargs > maxArgs)
                Left(mismatchArgs(ctxt, minArgs, maxArgs))
              else
                {..$body}
          }"""

        case _ =>
          c.abort(
            c.enclosingPosition,
            "Annotation @checkArgumentMismatch can only be used on Prim.fn")
      }

    c.Expr[Any](result)
  }
}

object TypeMismatchMacro {
  def impl[A](c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val typeParam = c.macroApplication.filter {
      case Ident(_) => true
      case _ => false
    }(1)

    val result =
      annottees.map(_.tree).toList match {
        case q"$mods def fn(ctxt: Ctxt): $returnType = { ..$body }" :: Nil =>
          q"""$mods def fn(ctxt: Ctxt): $returnType =  {
              mismatchType[$typeParam](ctxt) match {
                case Some(typeMismatch) => Left(typeMismatch)
                case None => {..$body}
              }
          }"""

        case _ =>
          c.abort(c.enclosingPosition,
                  "Annotation @checkTypeMismatch can only be used on Prim.fn")
      }

    c.Expr[Any](result)
  }
}
