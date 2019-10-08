package coop.rchain.rholang.build
import coop.rchain.models.NormalizerEnv.NormalizerEnv
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.ParBuilder
import monix.eval.Coeval

import scala.io.Source

trait CompiledRholangSource {
  val path: String
  val term: Par
  val code: String
  val normalizerEnv: NormalizerEnv
}

object CompiledRholangSource {

  def apply(classpath: String, env: NormalizerEnv): CompiledRholangSource =
    new CompiledRholangSource {
      override val path: String = classpath
      override val code: String = {
        val fileContent = Source.fromResource(classpath).mkString

        s"""//Loaded from resource file <<$classpath>>
         #$fileContent
         #""".stripMargin('#')
      }
      override val term: Par =
        ParBuilder[Coeval].buildNormalizedTerm(code, env).value()
      override val normalizerEnv: NormalizerEnv = env
    }
}

/**
  * Loads code from a resource file while doing macro substitution with the provided values.
  * The macros have the format $$macro$$
  * @param classpath
  * @param env a sequence of pairs macro -> value
  * @return
  */
abstract class CompiledRholangTemplate(
    classpath: String,
    normalizerEnv0: NormalizerEnv,
    env: (String, Any)*
) extends CompiledRholangSource {

  val originalContent = Source.fromResource(classpath).mkString

  val finalContent = env.foldLeft(originalContent) {
    case (content, (name, value)) => content.replace(s"""$$$$$name$$$$""", value.toString)
  }

  override val path: String = classpath
  override val code: String =
    s"""//Loaded from resource file <<$classpath>>
        #$finalContent
        #""".stripMargin('#')

  override val term: Par                    = ParBuilder[Coeval].buildNormalizedTerm(code, normalizerEnv0).value()
  override val normalizerEnv: NormalizerEnv = normalizerEnv0
}
