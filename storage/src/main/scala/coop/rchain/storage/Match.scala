package coop.rchain.storage

/**
  * Type class for matching patterns with data.
  *
  * In Rholang, `P` is `List[Channel]`, and `A` is `List[Quote]`.
  *
  * @tparam P The type of Patterns
  * @tparam A The type of Data
  */
trait Match[P, A] {

  def get(p: P, a: A): Option[A]
}
