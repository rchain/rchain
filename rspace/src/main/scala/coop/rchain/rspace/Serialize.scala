package coop.rchain.rspace

/**
  * Type class for serializing and deserializing values
  *
  * @tparam A The type to serialize and deserialize.
  */
trait Serialize[A] {

  def encode(a: A): Array[Byte]

  def decode(bytes: Array[Byte]): Either[Throwable, A]
}
