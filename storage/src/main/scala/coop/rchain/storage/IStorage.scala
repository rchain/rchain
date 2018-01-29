package coop.rchain.storage

/**
  * Interface for the Storage Layer
  */
trait IStorage {

  /**
    * Puts a given key-value pair in LMDB
    */
  def put[A](key: Key, value: A)(implicit s: Serialize[A]): Either[Error, Unit]

  /**
    * Retrieves the value for a given key from LMDB
    */
  def get[A](key: Key)(implicit s: Serialize[A]): Either[Error, A]

  /**
    * Removes a the key-value pair for a given key from LMDB
    */
  def remove(key: Key): Either[Error, Boolean]
}
