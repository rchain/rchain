package coop.rchain.storage.models

import java.nio.charset.StandardCharsets

import cats.syntax.either._
import coop.rchain.models.Serialize

trait SerializeInstances {
  implicit object stringInstance extends Serialize[String] {

    def encode(s: String): Array[Byte] =
      s.getBytes(StandardCharsets.UTF_8)

    def decode(bytes: Array[Byte]): Either[Throwable, String] =
      Either
        .catchNonFatal(new String(bytes, StandardCharsets.UTF_8))
  }

  implicit object bytesInstance extends Serialize[Array[Byte]] {

    def encode(a: Array[Byte]): Array[Byte] = a

    def decode(bytes: Array[Byte]): Either[Throwable, Array[Byte]] = Right(bytes)
  }
}
