package coop.rchain.storage

/**
  * Interface for the Storage Layer
  */
trait IStorage[K, V] {

  /**
    * Puts a given key-value pair in LMDB
    */
  def put(key: K, value: V): Either[Error, Unit]

  /**
    * Retrieves the value for a given key from LMDB
    */
  def get(key: K): Either[Error, V]

  /**
    * Removes a the key-value pair for a given key from LMDB
    */
  def remove(key: K): Either[Error, Boolean]
}
