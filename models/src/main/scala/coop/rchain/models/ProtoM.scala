package coop.rchain.models
import cats.{Applicative, Monad}
import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.Descriptors.FieldDescriptor
import com.google.protobuf.{ByteString, CodedOutputStream, Descriptors, MessageLite}
import scalapb.GeneratedMessage
import scalapb.compiler.{DescriptorPimps, GeneratorParams, Types}

object ProtoM extends DescriptorPimps {

  def params: GeneratorParams = ??? //required by DescriptorPimps, but we don't use it transitively

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
