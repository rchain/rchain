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
                Left(PrimErrorWrapper(mismatchArgs(ctxt, minArgs, maxArgs)))
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

    object findTypeParam extends Traverser {
      var idents = List[NameApi]()

      override def traverse(tree: c.universe.Tree): Unit = tree match {
        case Ident(tpe) => idents = idents :+ tpe
        case _ => super.traverse(tree)
      }
    }

    val result =
      annottees.map(_.tree).toList match {
        case q"$mods def fn(ctxt: Ctxt): $returnType = { ..$body }" :: Nil =>
          findTypeParam.traverse(c.macroApplication)
          val param = findTypeParam.idents(1)

          q"""$mods def fn(ctxt: Ctxt): $returnType =  {
              mismatchType[${param.toTypeName}](ctxt) match {
                case Some(typeMismatch) => Left(PrimErrorWrapper(typeMismatch))
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
