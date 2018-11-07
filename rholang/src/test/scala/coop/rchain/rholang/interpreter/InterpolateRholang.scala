package coop.rchain.rholang.interpreter

import coop.rchain.models.{Interpolate, Par}
import monix.eval.Coeval

object InterpolateRholang {

  /**
    * Method for interpolating rholang source code with Par instances.
    * Values to be replaced have to be strings starting with `#`
    *
    * Examples:
    *
    * #1 Interpolate channels:
    * {{{
    *   val privateChannel = GPrivateBuilder()
    *   val send = interpolate("\"@#x\"!(10)", Map[String, Par]("#x" -> privateChannel))
    *   val receive = interpolate("\"for(@x <- @\"#y\") { @Nil!(x) }", Map[String, Par]("#x" -> privateChannel))
    * }}}
    *
    * #2 Interpolate data:
    * {{{
    *   val privateChannel = GPrivateBuilder()
    *   val send = interpolate("@Nil!(\"#key1\", 10)", Map[String, Par]("#key1" -> privateChannel))
    * }}}
    *
    * #3 Interpolate inside new body:
    * {{{
    *   val privateChannel = GPrivateBuilder()
    *   val neu = interpolate("new x in { \"@#key1\"!(10) }", Map[String, Par]("#key1" -> privateChannel))
    * }}}
    *
    * #4 Interpolate lists and tuples:
    * {{{
    *   val privateChannel = GPrivateBuilder()
    *   val tuple = interpolate("[\"#key1\"]", Map[String, Par]("#key1" -> privateChannel))
    *   val list = interpolate("(\"#key1\")", Map[String, Par]("#key1" -> privateChannel))
    * }}}
    *
    * @param term rholang code that is to be parsed and interpolated
    * @param interpolateMap interpolation keys to values mappings
    * @return term after interpolation
    */
  def interpolate(term: String, interpolateMap: Map[String, Par]): Coeval[Par] =
    Interpreter.buildNormalizedTerm(term).map(Interpolate.interpolate(_, interpolateMap))
}
