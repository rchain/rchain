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
