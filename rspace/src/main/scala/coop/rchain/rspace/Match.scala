package coop.rchain.rspace

/**
  * Type class for matching patterns with data.
  *
  * @tparam P A type representing patterns
  * @tparam A A type representing data
  */
trait Match[P, A] {

  def get(p: P, a: A): Option[A]
}
