package coop.rchain.models

/**
  * Type class for serializing/deserializing values
  */
trait Serialize[A] {

  def encode(a: A): Array[Byte]

  def decode(bytes: Array[Byte]): Either[Throwable, A]
}
