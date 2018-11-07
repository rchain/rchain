package coop.rchain.models
import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.Descriptors.FieldDescriptor
import com.google.protobuf.{ByteString, CodedOutputStream, Descriptors, MessageLite}
import monix.eval.Coeval
import scalapb.compiler.{DescriptorPimps, GeneratorParams, Types}
import scalapb.WireType

object ProtoM extends DescriptorPimps {

  def params: GeneratorParams = ??? //required by DescriptorPimps, but we don't use it transitively

  def toByteArray(message: StacksafeMessage[_]): Coeval[Array[Byte]] =
    for {
      size  <- message.serializedSizeM.get
      array = new Array[Byte](size)
      out   = CodedOutputStream.newInstance(array)
      _     <- ProtoM.writeTo(out, message)
      _     <- Sync[Coeval].catchNonFatal { out.checkNoSpaceLeft() }
    } yield array

  def writeTo(
      out: CodedOutputStream,
      message: StacksafeMessage[_]
  ): Coeval[Unit] = {
    val companion       = message.companion
    val descriptor      = companion.javaDescriptor
    val defaultInstance = companion.defaultInstance.asInstanceOf[StacksafeMessage[_]]
    for {
      _ <- raiseUnsupportedIf[Coeval](
            descriptor.preservesUnknownFields,
            "Unknown fields are not supported"
          )
      _ <- descriptor.fields
            .sortBy(_.getNumber)
            .toList
            .traverse[Coeval, Unit](f => {
              val fieldValue = message.getField(f)
              val default    = defaultInstance.getField(f)
              if (fieldValue != default)
                writeField(out, fieldValue, f)
              else Monad[Coeval].pure(())
            })
    } yield ()
  }

  private def writeField(
      out: CodedOutputStream,
      value: Any,
      field: FieldDescriptor
  ): Coeval[Unit] =
    if (field.isRepeated) {
      writeRepeatedField(out, value, field)
    } else if (field.isRequired || field.isSingular || field.isOptional) {
      writeSingleField(out, value, field)
    } else {
      Sync[Coeval].raiseError(
        new RuntimeException(
          "This cannot be! A field that's none of: repeated, required, singluar, optional. Did protobuf spec change?"
        )
      )
    }

  private def writeRepeatedField(
      out: CodedOutputStream,
      value: Any,
      field: FieldDescriptor
  ): Coeval[Unit] =
    for {
      _         <- raiseUnsupportedIf[Coeval](field.isPacked, "Packed fields are unsupported")
      container = value.asInstanceOf[Seq[Any]].toList
      _         <- container.traverse(writeSingleField(out, _, field))
    } yield ()

  private def writeSingleField(
      out: CodedOutputStream,
      value: Any,
      field: Descriptors.FieldDescriptor
  ): Coeval[Unit] =
    if (field.isMessage) {
      for {
        _         <- writeTag(out, field, WireType.WIRETYPE_LENGTH_DELIMITED)
        valueSize <- value.asInstanceOf[StacksafeMessage[_]].serializedSizeM.get
        _         <- writeUInt32NoTag(out, valueSize)
        _         <- writeTo(out, value.asInstanceOf[StacksafeMessage[_]])
      } yield ()
    } else if (field.isEnum)
      Sync[Coeval].raiseError(
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
  ): Coeval[Unit] =
    Sync[Coeval].delay { out.writeTag(field.getNumber, wireType) }

  private def writeUInt32NoTag(out: CodedOutputStream, valueSize: Int): Coeval[Unit] =
    Sync[Coeval].delay { out.writeUInt32NoTag(valueSize) }

  private def writeScalarValue(
      value: Any,
      field: FieldDescriptor,
      out: CodedOutputStream
  ): Coeval[Unit] =
    Sync[Coeval].catchNonFatal {

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

  def serializedSize(
      message: StacksafeMessage[_]
  ): Coeval[Int] = Coeval.defer {
    val companion       = message.companion
    val descriptor      = companion.javaDescriptor
    val defaultInstance = companion.defaultInstance.asInstanceOf[StacksafeMessage[_]]
    for {
      _ <- raiseUnsupportedIf[Coeval](
            descriptor.preservesUnknownFields,
            "Unknown fields are not supported"
          )
      fieldSizes <- descriptor.fields.toList.traverse[Coeval, Int](f => {
                     val fieldValue = message.getField(f)
                     val default    = defaultInstance.getField(f)
                     if (fieldValue != default)
                       fieldSize(fieldValue, f)
                     else Monad[Coeval].pure(0)
                   })
    } yield fieldSizes.sum
  }

  private def fieldSize(value: Any, field: Descriptors.FieldDescriptor): Coeval[Int] =
    if (field.isRepeated) {
      repeatedSize(value, field)
    } else if (field.isRequired || field.isSingular || field.isOptional) {
      singleFieldSize(value, field)
    } else {
      Sync[Coeval].raiseError(
        new RuntimeException(
          "This cannot be! A field that's none of: repeated, required, singluar, optional. Did protobuf spec change?"
        )
      )
    }

  private def repeatedSize(value: Any, field: Descriptors.FieldDescriptor): Coeval[Int] =
    for {
      _ <- raiseUnsupportedIf[Coeval](field.isPacked, "Packed fields are unsupported")

      container = value.asInstanceOf[Seq[Any]].toList
      // format: off
      containerSize <- Types.fixedSize(field.getType) match {
        case Some(size) =>
          val tagSize = CodedOutputStream.computeTagSize(field.getNumber)
          Applicative[Coeval].pure(((size + tagSize) * container.size))
        case None =>
          val elementSizes: Coeval[List[Int]] = container.traverse(singleFieldSize(_, field))
          elementSizes.map(_.sum)
      }
      // format: on
    } yield containerSize

  private def singleFieldSize(value: Any, field: Descriptors.FieldDescriptor): Coeval[Int] =
    if (field.isMessage) {
      for {
        valueSize     <- value.asInstanceOf[StacksafeMessage[_]].serializedSizeM.get
        valueSizeSize = CodedOutputStream.computeUInt32SizeNoTag(valueSize)
        tagSize       = CodedOutputStream.computeTagSize(field.getNumber)
      } yield tagSize + valueSizeSize + valueSize
    } else if (field.isEnum)
      Sync[Coeval].raiseError(
        new UnsupportedOperationException(
          s"Enums are not supported, got $value of type ${value.getClass}"
        )
      )
    else {
      scalarValueSize(value, field)
    }

  private def scalarValueSize(value: Any, field: FieldDescriptor): Coeval[Int] =
    Sync[Coeval].catchNonFatal {

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
