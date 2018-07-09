package coop.rchain.rspace

/**
  * Type class for matching patterns with data.
  *
  * @tparam P A type representing patterns
  * @tparam A A type representing data
  * @tparam R A type representing a match result
  */
trait Match[P, A, R] {

  def get(p: P, a: A): Option[R]
}
