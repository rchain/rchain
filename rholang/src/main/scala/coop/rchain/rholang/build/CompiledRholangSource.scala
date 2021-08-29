package coop.rchain.rholang.build
import coop.rchain.models.NormalizerEnv.ToEnvMap
import coop.rchain.models.{NormalizerEnv, Par}
import coop.rchain.rholang.interpreter.builder.ADTBuilder
import monix.eval.Coeval
import shapeless.HNil

import scala.io.Source

abstract class CompiledRholangSource[Env](val code: String, val normalizerEnv: NormalizerEnv[Env])(
    implicit ev: ToEnvMap[Env]
) {
  val path: String
  val term: Par = ADTBuilder[Coeval, String, Par].buildWithEnv(code, normalizerEnv.toEnv).value()
  final def env = normalizerEnv.toEnv
}

object CompiledRholangSource {
  def loadSource(classpath: String) = {
    val fileContent = Source.fromResource(classpath).mkString
    s"""//Loaded from resource file <<$classpath>>
       #$fileContent
       #""".stripMargin('#')
  }

  def apply(classpath: String): CompiledRholangSource[HNil] = apply(classpath, NormalizerEnv.Empty)

  def apply[Env](classpath: String, env: NormalizerEnv[Env])(
      implicit ev: ToEnvMap[Env]
  ): CompiledRholangSource[Env] = new CompiledRholangSource[Env](loadSource(classpath), env) {
    override val path: String = classpath
  }
}

/**
  * Loads code from a resource file while doing macro substitution with the provided values.
  * The macros have the format $$macro$$
  * @param classpath
  * @param env a sequence of pairs macro -> value
  * @return
  */
abstract class CompiledRholangTemplate[Env](
    classpath: String,
    normalizerEnv0: NormalizerEnv[Env],
    env: (String, Any)*
)(implicit ev: ToEnvMap[Env])
    extends CompiledRholangSource[Env](
      CompiledRholangTemplate.loadTemplate(classpath, env),
      normalizerEnv0
    ) {
  override val path: String = classpath
}

object CompiledRholangTemplate {
  def loadTemplate(classpath: String, macros: Seq[(String, Any)]) = {
    val originalContent = Source.fromResource(classpath).mkString
    val finalContent = macros.foldLeft(originalContent) {
      case (content, (name, value)) => content.replace(s"""$$$$$name$$$$""", value.toString)
    }

    s"""//Loaded from resource file <<$classpath>>
       #$finalContent
       #""".stripMargin('#')
  }
}
