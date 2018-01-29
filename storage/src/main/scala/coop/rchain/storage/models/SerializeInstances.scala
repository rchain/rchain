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

  implicit object contractInstance extends Serialize[Contract] {

    def encode(a: Contract): Array[Byte] =
      a.toByteArray

    def decode(bytes: Array[Byte]): Either[SerializeError, Contract] =
      Either
        .catchNonFatal(Contract.parseFrom(bytes))
        .leftMap(SerializeError.apply)
  }

  implicit object systemContractInstance extends Serialize[SystemContract] {

    def encode(a: SystemContract): Array[Byte] =
      a.toByteArray

    def decode(bytes: Array[Byte]): Either[SerializeError, SystemContract] =
      Either
        .catchNonFatal(SystemContract.parseFrom(bytes))
        .leftMap(SerializeError.apply)
  }
}
