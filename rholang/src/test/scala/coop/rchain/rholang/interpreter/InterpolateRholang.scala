package coop.rchain.rholang.interpreter

import coop.rchain.models.{Interpolate, Par}
import monix.eval.Coeval

object InterpolateRholang {
  def interpolate(term: String, interpolateMap: Map[String, Par]): Coeval[Par] =
    Interpreter.buildNormalizedTerm(term).map(Interpolate.interpolate(_, interpolateMap))
}
