package coop.rchain.models

import java.nio.ByteBuffer

trait Serialize2ByteBuffer[A] {
  def encode(a: A): ByteBuffer
  def decode(bytes: ByteBuffer): A
}

object KryoSerializers {

  import com.esotericsoftware.kryo.{Kryo, Serializer}
  import com.esotericsoftware.kryo.io._

  import com.esotericsoftware.kryo.util.MapReferenceResolver
  import org.objenesis.strategy.StdInstantiatorStrategy

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

  object TaggedContEmptySerializer extends Serializer[TaggedContinuation.TaggedCont] {

    override def write(kryo: Kryo, output: Output, tc: TaggedContinuation.TaggedCont): Unit =
      kryo.writeObject(output, tc)

    override def read(
        kryo: Kryo,
        input: Input,
        `type`: Class[TaggedContinuation.TaggedCont]
    ): TaggedContinuation.TaggedCont = {
      val read = kryo.readObject(input, `type`)
      if (read.isEmpty) TaggedContinuation.TaggedCont.Empty
      else read
    }
  }

  object TaggedContinuationSerializer extends Serializer[TaggedContinuation] {

    def defaultSerializer(kryo: Kryo): Serializer[TaggedContinuation] =
      kryo
        .getDefaultSerializer(classOf[TaggedContinuation])
        .asInstanceOf[Serializer[TaggedContinuation]]

    override def write(kryo: Kryo, output: Output, tc: TaggedContinuation): Unit =
      defaultSerializer(kryo).write(kryo, output, tc)

    override def read(
        kryo: Kryo,
        input: Input,
        `type`: Class[TaggedContinuation]
    ): TaggedContinuation = {
      val read = defaultSerializer(kryo).read(kryo, input, `type`)
      if (read.taggedCont.isEmpty)
        TaggedContinuation()
      else read
    }
  }

  val kryo = new Kryo()
  kryo.register(classOf[ParMap], ParMapSerializer)
  kryo.register(classOf[ParSet], ParSetSerializer)
  kryo.register(classOf[TaggedContinuation], TaggedContinuationSerializer)

  kryo.setRegistrationRequired(false)
  // Support deserialization of classes without no-arg constructors
  kryo.setInstantiatorStrategy(new StdInstantiatorStrategy())

  implicit def serializer[A](of: Class[A]): Serialize2ByteBuffer[A] = new Serialize2ByteBuffer[A] {

    override def encode(gnat: A): ByteBuffer = {
      val output = new ByteBufferOutput(1024, -1)
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
