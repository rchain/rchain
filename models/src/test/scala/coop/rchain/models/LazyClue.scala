package coop.rchain.models

/**
  * A workaround around scalatest's `assert` and `withClue` taking strict (non-lazy) parameters.
  */
class LazyClue(string: => String) {
  override def toString: String = string
}

object LazyClue {
  def apply(string: => String) = new LazyClue(string)
}
