package coop.rchain.rspace

/**
  * Type class for matching patterns with data.
  *
  * @tparam P A type representing patterns
  * @tparam A A type representing data and match result
  */
trait Match[F[_], P, A] {

  def get(p: P, a: A): F[Option[A]]
}
