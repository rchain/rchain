package coop.rchain.rspace

/**
  * Type class for matching patterns with data.
  *
  * @tparam P A type representing patterns
  * @tparam E A type representing illegal state
  * @tparam A A type representing data
  * @tparam R A type representing a match result
  */
trait Match[P, E, A, R] {

  def get(p: P, a: A): Either[E, Option[R]]
}
