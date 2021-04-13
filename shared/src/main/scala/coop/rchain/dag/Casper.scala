package coop.rchain.dag

object Casper {
  final case class Justification[M, V](message: M, creator: V)
}
