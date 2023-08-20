package coop.rchain.models.rholangn.parmanager.protobuf

import cats.Eval
import com.google.protobuf.CodedInputStream
import coop.rchain.models.rholangn.parmanager.primitive.PrimitiveReader

object ProtoPrimitiveReader {

  /** Wrapper for protobuf de-serialization of primitive types. */
  def apply(input: CodedInputStream) = new PrimitiveReader[Eval] {
    // NOTE: Eval.always is used to ensure correct deserialization and read from input stream
    def readByte: Eval[Byte] = Eval.always(input.readRawByte())

    def readBytes: Eval[Array[Byte]] = Eval.always(input.readByteArray())

    def readBool: Eval[Boolean] = Eval.always(input.readBool())

    def readInt: Eval[Int] = Eval.always(input.readUInt32())

    def readLong: Eval[Long] = Eval.always(input.readUInt64())

    def readString: Eval[String] = Eval.always(input.readString())
  }
}
