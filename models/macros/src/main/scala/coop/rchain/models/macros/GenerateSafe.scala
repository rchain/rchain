package coop.rchain.models.macros

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

class GenerateSafe extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GenerateSafe.impl
}

object GenerateSafe {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    def convertToSafeType(typ: Trees#Tree): c.Tree =
      typ match {
        case Select(qualifier, name) =>
          Select(Select(qualifier, name.toTermName), TypeName("Safe"))
        case _ =>
          c.abort(c.enclosingPosition, "Unsupported type")
      }

    def convertToSafeTerm(typ: Trees#Tree): c.Tree =
      typ match {
        case Select(qualifier, name) =>
          Select(Select(qualifier, name.toTermName), TermName("Safe"))
        case _ =>
          c.abort(c.enclosingPosition, "Unsupported type")
      }

    def generateParams(params: List[ValDef]): List[ValDef] =
      params.map(x => (x, x.tpt)).map {
        case (valDef, tq"scala.Option[$tpt]") =>
          ValDef(valDef.mods, valDef.name, convertToSafeType(tpt), EmptyTree)
        case (valDef, _) =>
          valDef
      }

    def generateCreatorBody(params: List[ValDef]): List[ValDef] =
      params.map(x => (x, x.tpt)).map {
        case (valDef, tq"scala.Option[$tpt]") =>
          val safeTpt  = convertToSafeType(tpt)
          val safeTerm = convertToSafeTerm(tpt)
          ValDef(NoMods,
                 valDef.name,
                 tq"scala.Option[$safeTpt]",
                 q"$safeTerm.create(underlying.${valDef.name}.get)")
        case (valDef, _) =>
          ValDef(NoMods, valDef.name, valDef.tpt, q"underlying.${valDef.name}")
      }

    def isOption(tpt: Tree): Boolean =
      tpt match {
        case tq"scala.Option[$_]" => true
        case _                    => false
      }

    def unsafeParams(params: List[ValDef]): List[Tree] =
      params.collect {
        case valDef if isOption(valDef.tpt) =>
          q"underlying.${valDef.name}"
      }

    def unsafeSecondParams(params: List[ValDef]): List[Tree] =
      params.collect {
        case valDef if isOption(valDef.tpt) =>
          q"${valDef.name}"
      }

    def generateConstructorArguments(params: List[ValDef]): List[Tree] =
      params.map {
        case valDef if isOption(valDef.tpt) =>
          q"${valDef.name}.get"
        case valDef =>
          q"${valDef.name}"
      }

    annottees match {
      case (cl @ q"@..$_ final case class $tpname (..$params) extends { ..$_ } with ..$_ { $_ => ..$_ }")
            :: ModuleDef(mods, tmname, template)
            :: Nil =>
        val generatedParams = generateParams(params)
        val generatedWithMethods = generatedParams.map { param =>
          val capitalizedName = param.name.toString.capitalize
          val value           = if (param.tpt.toString.endsWith("Safe")) q"v.underlying" else q"v"
          val withMethod      = TermName("with" + capitalizedName)
          q"def $withMethod(v: ${param.tpt}): Safe = copy(${param.name} = v, underlying = underlying.$withMethod($value))"
        }
        val safeTp = q"""
          final case class Safe (..$generatedParams, underlying: $tpname) {
            def toByteString: com.google.protobuf.ByteString = underlying.toByteString
            def toByteArray: Array[Byte] = underlying.toByteArray
            ..$generatedWithMethods
          }
          """

        val safeTm = q"""
          object Safe {
            def create(underlying: $tpname): Option[Safe] =
              if (List[Option[Any]](..${unsafeParams(params)}).forall(_.isDefined)) {
                ..${generateCreatorBody(params)}
                if (List[Option[Any]](..${unsafeSecondParams(params)}).forall(_.isDefined)) {
                  Some(Safe(..${generateConstructorArguments(params)}, underlying))
                } else {
                  None
                }
              } else {
                None
              }
          }
          """
        val newObjTemplate =
          Template(template.parents, template.self, template.body :+ safeTp :+ safeTm)
        val newObj = ModuleDef(mods, tmname, newObjTemplate)
        q"""
          $cl
          $newObj
          """
      case _ =>
        c.abort(
          c.enclosingPosition,
          "Can only generate a safe version of a final case class with companion"
        )
    }
  }
}
