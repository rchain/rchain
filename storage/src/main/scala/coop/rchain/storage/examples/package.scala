package coop.rchain.storage

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import cats.syntax.either._
import coop.rchain.storage.util.withResource

package object examples {

  private[examples] def makeSerializeFromSerializable[T <: Serializable]: Serialize[T] =
    new Serialize[T] {

      def encode(a: T): Array[Byte] =
        withResource(new ByteArrayOutputStream()) { baos =>
          withResource(new ObjectOutputStream(baos)) { (oos: ObjectOutputStream) =>
            oos.writeObject(a)
          }
          baos.toByteArray
        }

      def decode(bytes: Array[Byte]): Either[Throwable, T] =
        Either.catchNonFatal {
          withResource(new ByteArrayInputStream(bytes)) { bais =>
            withResource(new ObjectInputStream(bais)) { ois =>
              ois.readObject.asInstanceOf[T]
            }
          }
        }
    }
}
