package coop.rchain.models

import java.nio.ByteBuffer

import scala.reflect.ClassTag

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io._
import coop.rchain.models.Var.VarInstance
import org.objenesis.strategy.StdInstantiatorStrategy

trait Serialize2ByteBuffer[A] {
  def encode(a: A): ByteBuffer
  def decode(bytes: ByteBuffer): A

}

class DefaultSerializer[T](implicit tag: ClassTag[T]) extends Serializer[T] {

  def defaultSerializer(kryo: Kryo): Serializer[T] =
    kryo
      .getDefaultSerializer(tag.runtimeClass)
      .asInstanceOf[Serializer[T]]

  override def write(kryo: Kryo, output: Output, e: T): Unit =
    defaultSerializer(kryo).write(kryo, output, e)

  override def read(
      kryo: Kryo,
      input: Input,
      `type`: Class[T]
  ): T = defaultSerializer(kryo).read(kryo, input, `type`)

}

object KryoSerializers {

  object ParMapSerializer extends Serializer[ParMap] {
    import ParMapTypeMapper._

    override def write(kryo: Kryo, output: Output, parMap: ParMap): Unit =
      kryo.writeObject(output, parMapToEMap(parMap))

    override def read(kryo: Kryo, input: Input, `type`: Class[ParMap]): ParMap =
      emapToParMap(kryo.readObject(input, classOf[EMap]))
  }

  object ParSetSerializer extends Serializer[ParSet] {
    import ParSetTypeMapper._

    override def write(kryo: Kryo, output: Output, parSet: ParSet): Unit =
      kryo.writeObject(output, parSetToESet(parSet))

    override def read(kryo: Kryo, input: Input, `type`: Class[ParSet]): ParSet =
      esetToParSet(kryo.readObject(input, classOf[ESet]))
  }

  def emptyReplacingSerializer[T](thunk: T => Boolean, replaceWith: T)(implicit tag: ClassTag[T]) =
    new DefaultSerializer[T] {
      override def read(
          kryo: Kryo,
          input: Input,
          `type`: Class[T]
      ): T = {
        val read = super.read(kryo, input, `type`)
        if (thunk(read))
          replaceWith
        else read
      }
    }

  val TaggedContinuationSerializer =
    emptyReplacingSerializer[TaggedContinuation](_.taggedCont.isEmpty, TaggedContinuation())

  val VarSerializer =
    emptyReplacingSerializer[Var](_.varInstance.isEmpty, Var())

  val ExprSerializer =
    emptyReplacingSerializer[Expr](_.exprInstance.isEmpty, Expr())

  val ConnectiveSerializer =
    emptyReplacingSerializer[Connective](_.connectiveInstance.isEmpty, Connective())

  val NoneSerializer: DefaultSerializer[None.type] =
    emptyReplacingSerializer[None.type](_.isEmpty, None)

  val kryo = new Kryo()
  kryo.register(classOf[ParMap], ParMapSerializer)
  kryo.register(classOf[ParSet], ParSetSerializer)
  kryo.register(classOf[TaggedContinuation], TaggedContinuationSerializer)
  kryo.register(classOf[Var], VarSerializer)
  kryo.register(classOf[Expr], ExprSerializer)
  kryo.register(classOf[Connective], ConnectiveSerializer)
  kryo.register(None.getClass, NoneSerializer)

  kryo.setRegistrationRequired(false)
  // Support deserialization of classes without no-arg constructors
  kryo.setInstantiatorStrategy(new StdInstantiatorStrategy())

  def serializer[A](of: Class[A]): Serialize2ByteBuffer[A] = new Serialize2ByteBuffer[A] {

    private[this] val noSizeLimit = -1

    override def encode(gnat: A): ByteBuffer = {
      val output = new ByteBufferOutput(1024, noSizeLimit)
      kryo.writeObject(output, gnat)
      output.close()

      val buf = output.getByteBuffer
      buf.flip()
      buf
    }

    override def decode(bytes: ByteBuffer): A = {
      val input = new ByteBufferInput(bytes)
      val res   = kryo.readObject(input, of)
      input.close()
      res
    }

  }
}
