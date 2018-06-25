package coop.rchain.rspace

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import cats.syntax.either._
import coop.rchain.shared.Resources.withResource
import scodec.bits.ByteVector

package object examples {

  def makeSerializeFromSerializable[T <: Serializable]: Serialize[T] =
    new Serialize[T] {

      def encode(a: T): ByteVector =
        withResource(new ByteArrayOutputStream()) { baos =>
          withResource(new ObjectOutputStream(baos)) { (oos: ObjectOutputStream) =>
            oos.writeObject(a)
          }
          ByteVector.view(baos.toByteArray)
        }

      def decode(bytes: ByteVector): Either[Throwable, T] =
        Either.catchNonFatal {
          withResource(new ByteArrayInputStream(bytes.toArray)) { bais =>
            withResource(new ObjectInputStream(bais)) { ois =>
              ois.readObject.asInstanceOf[T]
            }
          }
        }
    }
}
