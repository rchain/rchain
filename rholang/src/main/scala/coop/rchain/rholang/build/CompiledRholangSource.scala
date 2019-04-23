package coop.rchain.rholang.build
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.ParBuilder
import monix.eval.Coeval

import scala.io.Source

trait CompiledRholangSource {
  val path: String
  val term: Par
  val code: String
}

object CompiledRholangSource {

  def apply(classpath: String): CompiledRholangSource = new CompiledRholangSource {
    override val path: String = classpath
    override val code: String = Source.fromResource(classpath).mkString
    override val term: Par    = ParBuilder[Coeval].buildNormalizedTerm(code).value()
  }

}

/**
  * Loads code from a resource file while doing macro substitution with the provided values.
  * The macros have the format $$macro$$
  * @param classpath
  * @param env a sequence of pairs macro -> value
  * @return
  */
abstract class CompiledRholangTemplate(classpath: String, env: (String, Any)*)
    extends CompiledRholangSource {

  val originalContent = Source.fromResource(classpath).mkString

  val finalContent = env.foldLeft(originalContent) {
    case (content, (name, value)) => content.replace(s"""$$$$$name$$$$""", value.toString)
  }

  override val path: String = classpath
  override val code: String =
    s"""//Loaded from resource file <<$classpath>>
        #$finalContent
        #""".stripMargin('#')

  override val term: Par = ParBuilder[Coeval].buildNormalizedTerm(code).value()
}
