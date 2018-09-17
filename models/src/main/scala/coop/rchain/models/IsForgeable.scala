package coop.rchain.models

trait IsForgeable[T] {
  def isForgeable(subject: T): Boolean
  def apply(subject: T) = isForgeable(subject)
}
