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

object Serialize {
  def apply[A](implicit ev: Serialize[A]): Serialize[A] = ev
}
