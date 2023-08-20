package coop.rchain.models.rholangn.parmanager.protobuf

import cats.Eval
import com.google.protobuf.CodedInputStream
import coop.rchain.models.rholangn.parmanager.primitive.PrimitiveReader

import java.io.InputStream

object ProtoPrimitiveReader {

  /** Wrapper for protobuf de-serialization of primitive types. */
  def apply(input: InputStream) = {
    val cis = CodedInputStream.newInstance(input)

    new PrimitiveReader[Eval] {
      // NOTE: Eval.always is used to ensure correct deserialization and read from input stream
      def readByte: Eval[Byte] = Eval.always(cis.readRawByte())

      def readBytes: Eval[Array[Byte]] = Eval.always(cis.readByteArray())

      def readBool: Eval[Boolean] = Eval.always(cis.readBool())

      def readInt: Eval[Int] = Eval.always(cis.readUInt32())

      def readLong: Eval[Long] = Eval.always(cis.readUInt64())

      def readString: Eval[String] = Eval.always(cis.readString())
    }
  }
}
