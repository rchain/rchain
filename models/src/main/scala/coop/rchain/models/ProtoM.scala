package coop.rchain.models
import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.Descriptors.FieldDescriptor
import com.google.protobuf.{ByteString, CodedOutputStream, Descriptors, MessageLite}
import scalapb.GeneratedMessage
import scalapb.compiler.{DescriptorPimps, GeneratorParams, Types}
import scalapb.{GeneratedMessage, WireType}

object ProtoM extends DescriptorPimps {

  def params: GeneratorParams = ??? //required by DescriptorPimps, but we don't use it transitively

  def toByteArray[M[_]: Sync](message: GeneratedMessage): M[Array[Byte]] =
    for {
      size  <- ProtoM.serializedSize[M](message)
      array = new Array[Byte](size)
      out   = CodedOutputStream.newInstance(array)
      _     <- ProtoM.writeTo[M](out, message)
      _     <- Sync[M].catchNonFatal { out.checkNoSpaceLeft() }
    } yield array

  def writeTo[M[_]: Sync](
      out: CodedOutputStream,
      message: GeneratedMessage
  ): M[Unit] = {
    val companion       = message.companion
    val descriptor      = companion.javaDescriptor
    val defaultInstance = companion.defaultInstance.asInstanceOf[GeneratedMessage]
    for {
      _ <- raiseUnsupportedIf(descriptor.preservesUnknownFields, "Unknown fields are not supported")
      _ <- descriptor.fields
            .sortBy(_.getNumber)
            .toList
            .traverse[M, Unit](f => {
              val fieldValue = message.getField(f)
              val default    = defaultInstance.getField(f)
              if (fieldValue != default)
                writeField(out, fieldValue, f)
              else Monad[M].pure(())
            })
    } yield ()
  }

  private def writeField[M[_]: Sync](
      out: CodedOutputStream,
      value: Any,
      field: FieldDescriptor
  ): M[Unit] =
    if (field.isRepeated) {
      writeRepeatedField(out, value, field)
    } else if (field.isRequired || field.isSingular || field.isOptional) {
      writeSingleField(out, value, field)
    } else {
      Sync[M].raiseError(new RuntimeException("This cannot be!"))
    }

  private def writeRepeatedField[M[_]: Sync](
      out: CodedOutputStream,
      value: Any,
      field: FieldDescriptor
  ): M[Unit] =
    for {
      _ <- raiseUnsupportedIf[M](field.isPacked, "Packed fields are unsupported")

      container = value.asInstanceOf[Seq[Any]].toList
      _         <- container.traverse(writeSingleField(out, _, field))
    } yield ()

  private def writeSingleField[M[_]: Sync](
      out: CodedOutputStream,
      value: Any,
      field: Descriptors.FieldDescriptor
  ): M[Unit] =
    if (field.isMessage) {
      for {
        _         <- writeTag(out, field, WireType.WIRETYPE_LENGTH_DELIMITED)
        valueSize <- serializedSize(value.asInstanceOf[GeneratedMessage])
        _         <- writeUInt32NoTag(out, valueSize)
        _         <- writeTo(out, value.asInstanceOf[GeneratedMessage])
      } yield ()
    } else if (field.isEnum)
      Sync[M].raiseError(
        new UnsupportedOperationException(
          s"Enums are not supported, got $value of type ${value.getClass}"
        )
      )
    else {
      writeScalarValue(value, field, out)
    }

  private def writeTag[M[_]: Sync](
      out: CodedOutputStream,
      field: FieldDescriptor,
      wireType: Int
  ): M[Unit] =
    Sync[M].delay { out.writeTag(field.getNumber, wireType) }

  private def writeUInt32NoTag[M[_]: Sync](out: CodedOutputStream, valueSize: Int): M[Unit] =
    Sync[M].delay { out.writeUInt32NoTag(valueSize) }

  private def writeScalarValue[M[_]: Sync](
      value: Any,
      field: FieldDescriptor,
      out: CodedOutputStream
  ): M[Unit] =
    Sync[M].catchNonFatal {

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
        case GROUP    => out.writeGroup     (field.getNumber, value.asInstanceOf[MessageLite])
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

  def serializedSize[M[_]: Sync](
      message: GeneratedMessage
  ): M[Int] = {
    val companion       = message.companion
    val descriptor      = companion.javaDescriptor
    val defaultInstance = companion.defaultInstance.asInstanceOf[GeneratedMessage]
    for {
      _ <- raiseUnsupportedIf(descriptor.preservesUnknownFields, "Unknown fields are not supported")
      fieldSizes <- descriptor.fields.toList.traverse[M, Int](f => {
                     val fieldValue = message.getField(f)
                     val default    = defaultInstance.getField(f)
                     if (fieldValue != default)
                       fieldSize(fieldValue, f)
                     else Monad[M].pure(0)
                   })
    } yield fieldSizes.sum
  }

  private def fieldSize[M[_]: Sync](value: Any, field: Descriptors.FieldDescriptor): M[Int] =
    if (field.isRepeated) {
      repeatedSize(value, field)
    } else if (field.isRequired || field.isSingular || field.isOptional) {
      singleFieldSize(value, field)
    } else {
      Sync[M].raiseError(new RuntimeException("This cannot be!"))
    }

  private def repeatedSize[M[_]: Sync](value: Any, field: Descriptors.FieldDescriptor): M[Int] =
    for {
      _ <- raiseUnsupportedIf[M](field.isPacked, "Packed fields are unsupported")

      container = value.asInstanceOf[Seq[Any]].toList
      // format: off
      containerSize <- Types.fixedSize(field.getType) match {
        case Some(size) =>
          val tagSize = CodedOutputStream.computeTagSize(field.getNumber)
          Applicative[M].pure(((size + tagSize) * container.size))
        case None =>
          val elementSizes: M[List[Int]] = container.traverse(singleFieldSize(_, field))
          elementSizes.map(_.sum)
      }
      // format: on
    } yield containerSize

  private def singleFieldSize[M[_]: Sync](value: Any, field: Descriptors.FieldDescriptor): M[Int] =
    if (field.isMessage) {
      for {
        valueSize     <- serializedSize(value.asInstanceOf[GeneratedMessage])
        valueSizeSize = CodedOutputStream.computeUInt32SizeNoTag(valueSize)
        tagSize       = CodedOutputStream.computeTagSize(field.getNumber)
      } yield tagSize + valueSizeSize + valueSize
    } else if (field.isEnum)
      Sync[M].raiseError(
        new UnsupportedOperationException(
          s"Enums are not supported, got $value of type ${value.getClass}"
        )
      )
    else {
      scalarValueSize(value, field)
    }

  private def scalarValueSize[M[_]: Sync](value: Any, field: FieldDescriptor): M[Int] =
    Sync[M].catchNonFatal {

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
        case GROUP    => computeGroupSize      (field.getNumber, value.asInstanceOf[MessageLite])
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
      Applicative[M].pure(())
    }
}
