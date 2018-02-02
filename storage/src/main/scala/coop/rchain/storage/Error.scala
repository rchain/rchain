package coop.rchain.storage

/**
  * Represents an error that can occur in the storage layer
  */
sealed trait Error
final case class SerializeError(msg: String)        extends Error
final case class StorageError(throwable: Throwable) extends Error
case object NotFound                                extends Error
