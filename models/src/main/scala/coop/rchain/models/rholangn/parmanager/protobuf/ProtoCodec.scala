package coop.rchain.models.rholangn.parmanager.protobuf

import cats.Applicative
import cats.syntax.all._
import com.google.protobuf.CodedOutputStream

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import scala.util.Using

object ProtoCodec {

  // TODO: make these functions more usable and elegant with cats.effect Resource and error handling

  // TODO: Properly handle errors
  @SuppressWarnings(Array("org.wartremover.warts.TryPartial"))
  def decode[F[_], T](bv: Array[Byte], read: InputStream => F[T]): F[T] =
    Using(new ByteArrayInputStream(bv))(read).get

  // TODO: Properly handle errors
  @SuppressWarnings(Array("org.wartremover.warts.TryPartial"))
  def encode[F[_]: Applicative](
      payloadSize: Int,
      write: CodedOutputStream => F[Unit]
  ): F[Array[Byte]] =
    Using(new ByteArrayOutputStream(payloadSize)) { baos =>
      val cos = CodedOutputStream.newInstance(baos)
      write(cos).map { _ =>
        cos.flush()
        baos.flush()
        baos.toByteArray
      }
    }.get
}
