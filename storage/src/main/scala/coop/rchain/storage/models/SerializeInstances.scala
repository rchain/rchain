package coop.rchain.storage.models

import cats.syntax.either._
import coop.rchain.storage.models.StorableData.Value
import coop.rchain.storage.{Serialize, SerializeError}

trait SerializeInstances {

  implicit object blockInstance extends Serialize[Block] {

    def encode(a: Block): Array[Byte] =
      StorableData(value = Value.Block(a)).toByteArray

    def decode(bytes: Array[Byte]): Either[SerializeError, Block] =
      StorableData
        .parseFrom(bytes)
        .value
        .block
        .toRight(SerializeError("decode: could not parse Block"))
  }

  implicit object contractInstance extends Serialize[Contract] {

    def encode(a: Contract): Array[Byte] =
      StorableData(value = Value.Contract(a)).toByteArray

    def decode(bytes: Array[Byte]): Either[SerializeError, Contract] =
      StorableData
        .parseFrom(bytes)
        .value
        .contract
        .toRight(SerializeError("decode: could not parse Contract"))
  }

  implicit object systemContractInstance extends Serialize[SystemContract] {

    def encode(a: SystemContract): Array[Byte] =
      StorableData(value = Value.SystemContract(a)).toByteArray

    def decode(bytes: Array[Byte]): Either[SerializeError, SystemContract] =
      StorableData
        .parseFrom(bytes)
        .value
        .systemContract
        .toRight(SerializeError("decode: could not parse SystemContract"))
  }
}
