package coop.rchain.models

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.all._
import com.google.protobuf.Descriptors.FieldDescriptor
import com.google.protobuf.WireFormat.FieldType
import com.google.protobuf.{ByteString, CodedOutputStream, Descriptors, MessageLite}
import cats.Eval
import scalapb.WireType
import scalapb.compiler.Types
import coop.rchain.catscontrib.effect.implicits.sEval

import scala.jdk.CollectionConverters._

object ProtoM {

  def toByteArray(message: StacksafeMessage[_]): Eval[Array[Byte]] =
    for {
      size  <- message.serializedSizeM.get
      array = new Array[Byte](size)
      out   = CodedOutputStream.newInstance(array)
      _     <- ProtoM.writeTo(out, message)
      _     <- Sync[Eval].catchNonFatal { out.checkNoSpaceLeft() }
    } yield array

  def writeTo(
      out: CodedOutputStream,
      message: StacksafeMessage[_]
  ): Eval[Unit] = {
    val companion       = message.companion
    val descriptor      = companion.javaDescriptor
    val defaultInstance = companion.defaultInstance.asInstanceOf[StacksafeMessage[_]]
    for {
      _ <- descriptor.getFields.asScala.toList
            .sortBy(_.getNumber)
            .traverse(f => {
              val fieldValue = message.getFieldByNumber(f.getNumber)
              val default    = defaultInstance.getFieldByNumber(f.getNumber)
              if (fieldValue != default)
                writeField(out, fieldValue, f)
              else ().pure[Eval]
            })
    } yield ()
  }

  private def writeField(
      out: CodedOutputStream,
      value: Any,
      field: FieldDescriptor
  ): Eval[Unit] =
    if (field.isRepeated) {
      writeRepeatedField(out, value, field)
    } else {
      writeSingleField(out, value, field)
    }

  private def writeRepeatedField(
      out: CodedOutputStream,
      value: Any,
      field: FieldDescriptor
  ): Eval[Unit] =
    for {
      _         <- raiseUnsupportedIf[Eval](field.isPacked, "Packed fields are unsupported")
      container = value.asInstanceOf[Seq[Any]].toList
      _         <- container.traverse(writeSingleField(out, _, field))
    } yield ()

  private def writeSingleField(
      out: CodedOutputStream,
      value: Any,
      field: Descriptors.FieldDescriptor
  ): Eval[Unit] =
    if (field.getLiteType == FieldType.MESSAGE) {
      for {
        _         <- writeTag(out, field, WireType.WIRETYPE_LENGTH_DELIMITED)
        valueSize <- value.asInstanceOf[StacksafeMessage[_]].serializedSizeM.get
        _         <- writeUInt32NoTag(out, valueSize)
        _         <- writeTo(out, value.asInstanceOf[StacksafeMessage[_]])
      } yield ()
    } else if (field.getLiteType == FieldType.ENUM)
      Sync[Eval].raiseError(
        new UnsupportedOperationException(
          s"Enums are not supported, got $value of type ${value.getClass}"
        )
      )
    else {
      writeScalarValue(value, field, out)
    }

  private def writeTag(
      out: CodedOutputStream,
      field: FieldDescriptor,
      wireType: Int
  ): Eval[Unit] =
    Sync[Eval].delay { out.writeTag(field.getNumber, wireType) }

  private def writeUInt32NoTag(out: CodedOutputStream, valueSize: Int): Eval[Unit] =
    Sync[Eval].delay { out.writeUInt32NoTag(valueSize) }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def writeScalarValue(
      value: Any,
      field: FieldDescriptor,
      out: CodedOutputStream
  ): Eval[Unit] =
    Sync[Eval].catchNonFatal {

      import FieldDescriptor.Type._

      // format: off
      field.getType match {
        case DOUBLE   => out.writeDouble    (field.getNumber, value.asInstanceOf[Double])
        case FLOAT    => out.writeFloat     (field.getNumber, value.asInstanceOf[Float])
        case INT64    => out.writeInt64     (field.getNumber, value.asInstanceOf[Long])
        case UINT64   => out.writeUInt64    (field.getNumber, value.asInstanceOf[Long])
        case INT32    => out.writeInt32     (field.getNumber, value.asInstanceOf[Int])
        case FIXED64  => out.writeFixed64   (field.getNumber, value.asInstanceOf[Long])
        case FIXED32  => out.writeFixed32   (field.getNumber, value.asInstanceOf[Int])
        case BOOL     => out.writeBool      (field.getNumber, value.asInstanceOf[Boolean])
        case STRING   => out.writeString    (field.getNumber, value.asInstanceOf[String])
        case GROUP    => throw new UnsupportedOperationException(
          "Groups are not supported, because they're deprecated in protobuf"
        )
        case MESSAGE  => out.writeMessage   (field.getNumber, value.asInstanceOf[MessageLite])
        case BYTES    => out.writeBytes     (field.getNumber, value.asInstanceOf[ByteString])
        case UINT32   => out.writeUInt32    (field.getNumber, value.asInstanceOf[Int])
        case ENUM     => throw new UnsupportedOperationException(
          s"Enums are not supported, got $value of type ${value.getClass}"
        )
        case SFIXED32 => out.writeSFixed32  (field.getNumber, value.asInstanceOf[Int])
        case SFIXED64 => out.writeSFixed64  (field.getNumber, value.asInstanceOf[Long])
        case SINT32   => out.writeSInt32    (field.getNumber, value.asInstanceOf[Int])
        case SINT64   => out.writeSInt64    (field.getNumber, value.asInstanceOf[Long])
      }
      // format: on
    }

  def serializedSize(
      message: StacksafeMessage[_]
  ): Eval[Int] = Eval.defer {
    val companion       = message.companion
    val descriptor      = companion.javaDescriptor
    val defaultInstance = companion.defaultInstance.asInstanceOf[StacksafeMessage[_]]
    for {
      fieldSizes <- descriptor.getFields.asScala.toList.traverse(f => {
                     val fieldValue = message.getFieldByNumber(f.getNumber)
                     val default    = defaultInstance.getFieldByNumber(f.getNumber)
                     if (fieldValue != default)
                       fieldSize(fieldValue, f)
                     else 0.pure[Eval]
                   })
    } yield fieldSizes.sum
  }

  private def fieldSize(value: Any, field: Descriptors.FieldDescriptor): Eval[Int] =
    if (field.isRepeated) {
      repeatedSize(value, field)
    } else {
      singleFieldSize(value, field)
    }

  private def repeatedSize(value: Any, field: Descriptors.FieldDescriptor): Eval[Int] =
    for {
      _ <- raiseUnsupportedIf[Eval](field.isPacked, "Packed fields are unsupported")

      container = value.asInstanceOf[Seq[Any]].toList
      containerSize <- Types.fixedSize(field.getType) match {
                        case Some(size) =>
                          val tagSize = CodedOutputStream.computeTagSize(field.getNumber)
                          ((size + tagSize) * container.size).pure[Eval]
                        case None =>
                          val elementSizes = container.traverse(singleFieldSize(_, field))
                          elementSizes.map(_.sum)
                      }
    } yield containerSize

  private def singleFieldSize(value: Any, field: Descriptors.FieldDescriptor): Eval[Int] =
    if (field.getLiteType == FieldType.MESSAGE) {
      for {
        valueSize     <- value.asInstanceOf[StacksafeMessage[_]].serializedSizeM.get
        valueSizeSize = CodedOutputStream.computeUInt32SizeNoTag(valueSize)
        tagSize       = CodedOutputStream.computeTagSize(field.getNumber)
      } yield tagSize + valueSizeSize + valueSize
    } else if (field.getLiteType == FieldType.ENUM)
      Sync[Eval].raiseError(
        new UnsupportedOperationException(
          s"Enums are not supported, got $value of type ${value.getClass}"
        )
      )
    else {
      scalarValueSize(value, field)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def scalarValueSize(value: Any, field: FieldDescriptor): Eval[Int] =
    Sync[Eval].catchNonFatal {

      import FieldDescriptor.Type._
      import com.google.protobuf.CodedOutputStream._

      // format: off
      field.getType match {
        case DOUBLE   => computeDoubleSize     (field.getNumber, value.asInstanceOf[Double])
        case FLOAT    => computeFloatSize      (field.getNumber, value.asInstanceOf[Float])
        case INT64    => computeInt64Size      (field.getNumber, value.asInstanceOf[Long])
        case UINT64   => computeUInt64Size     (field.getNumber, value.asInstanceOf[Long])
        case INT32    => computeInt32Size      (field.getNumber, value.asInstanceOf[Int])
        case FIXED64  => computeFixed64Size    (field.getNumber, value.asInstanceOf[Long])
        case FIXED32  => computeFixed32Size    (field.getNumber, value.asInstanceOf[Int])
        case BOOL     => computeBoolSize       (field.getNumber, value.asInstanceOf[Boolean])
        case STRING   => computeStringSize     (field.getNumber, value.asInstanceOf[String])
        case GROUP    => throw new UnsupportedOperationException(
          "Groups are not supported, because they're deprecated in protobuf"
        )
        case MESSAGE  => computeMessageSize    (field.getNumber, value.asInstanceOf[MessageLite])
        case BYTES    => computeBytesSize      (field.getNumber, value.asInstanceOf[ByteString])
        case UINT32   => computeUInt32Size     (field.getNumber, value.asInstanceOf[Int])
        case ENUM     => throw new UnsupportedOperationException(
          s"Enums are not supported, got $value of type ${value.getClass}"
        )
        case SFIXED32 => computeSFixed32Size   (field.getNumber, value.asInstanceOf[Int])
        case SFIXED64 => computeSFixed64Size   (field.getNumber, value.asInstanceOf[Long])
        case SINT32   => computeSInt32Size     (field.getNumber, value.asInstanceOf[Int])
        case SINT64   => computeSInt64Size     (field.getNumber, value.asInstanceOf[Long])
      }
      // format: on
    }

  private def raiseUnsupportedIf[M[_]: Sync](condition: => Boolean, message: String): M[Unit] =
    if (condition) {
      Sync[M].raiseError(new UnsupportedOperationException(message))
    } else {
      ().pure[M]
    }
}
