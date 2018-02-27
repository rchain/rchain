package coop.rchain.storage

trait Transaction[T] {

  def commit(t: T): Unit

  def abort(t: T): Unit

  def close(t: T): Unit
}
