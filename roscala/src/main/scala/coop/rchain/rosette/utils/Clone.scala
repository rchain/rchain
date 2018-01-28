package coop.rchain.rosette.utils

trait Clone[T] {
  def clone(t: T): T
}
