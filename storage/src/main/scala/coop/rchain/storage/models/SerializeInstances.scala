package coop.rchain.storage.models

import cats.syntax.either._
import com.trueaccord.scalapb.{GeneratedMessage, GeneratedMessageCompanion}
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

  case class rhoInstanceWrapper[T <: GeneratedMessage with com.trueaccord.scalapb.Message[T]](
      companion: GeneratedMessageCompanion[T])
      extends Serialize[T] {
    override def encode(a: T): Array[Byte] = companion.toByteArray(a)

    override def decode(bytes: Array[Byte]): Either[SerializeError, T] =
      Either
        .catchNonFatal(companion.parseFrom(bytes))
        .leftMap(SerializeError.apply)
  }

  implicit object parInstance      extends rhoInstanceWrapper(Par)
  implicit object boundVarInstance extends rhoInstanceWrapper(BoundVar)
  implicit object varInstance      extends rhoInstanceWrapper(Var)
}
