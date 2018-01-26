package coop.rchain.storage.models

import cats.syntax.either._
import coop.rchain.storage.{Serialize, SerializeError}

trait SerializeInstances {

  implicit object blockInstance extends Serialize[Block] {

    def encode(a: Block): Array[Byte] =
      a.toByteArray

    def decode(bytes: Array[Byte]): Either[SerializeError, Block] =
      Either
        .catchNonFatal(Block.parseFrom(bytes))
        .leftMap(SerializeError.apply)
  }
}
