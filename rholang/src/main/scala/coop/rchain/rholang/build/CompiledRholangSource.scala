package coop.rchain.rholang.build
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import monix.eval.Coeval

import scala.io.Source

trait CompiledRholangSource {
  val term: Par
  val code: String
}

object CompiledRholangSource {

  def apply(classpath: String): CompiledRholangSource = new CompiledRholangSource {
    override val code: String = Source.fromResource(classpath).mkString
    override val term: Par    = Interpreter[Coeval].buildNormalizedTerm(code).value()
  }

}
