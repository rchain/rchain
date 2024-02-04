package coop.rchain.models.rholangn.parmanager.protobuf

import cats.Eval
import com.google.protobuf.CodedOutputStream
import coop.rchain.models.rholangn.parmanager.primitive.PrimitiveWriter

/** Wrapper for protobuf serialization of primitive types. */
object ProtoPrimitiveWriter {
  def apply(output: CodedOutputStream): PrimitiveWriter[Eval] =
    new PrimitiveWriter[Eval] {
      def write(x: Byte): Eval[Unit] = Eval.later(output.writeRawByte(x))

      /** Writes raw bytes without size prefix */
      def writeRaw(x: Array[Byte]): Eval[Unit] = Eval.later(output.writeRawBytes(x))

      /** Writes bytes with size prefix */
      def write(x: Array[Byte]): Eval[Unit] = Eval.later(output.writeByteArrayNoTag(x))

      def write(x: Boolean): Eval[Unit] = Eval.later(output.writeBoolNoTag(x))

      def write(x: Int): Eval[Unit] = Eval.later(output.writeUInt32NoTag(x))

      def write(x: Long): Eval[Unit] = Eval.later(output.writeUInt64NoTag(x))

      def write(x: String): Eval[Unit] = Eval.later(output.writeStringNoTag(x))
    }

  def encodeWith(write: PrimitiveWriter[Eval] => Eval[Unit]): Eval[Array[Byte]] =
    ProtoCodec.encode(payloadSize = 256, ProtoPrimitiveWriter.apply _ andThen write)
}
