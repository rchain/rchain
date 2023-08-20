package coop.rchain.models.rholangn.parmanager.protobuf

import cats.Applicative
import cats.syntax.all._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import scala.util.Using

object ProtoCodec {

  // TODO: Properly handle errors
  @SuppressWarnings(Array("org.wartremover.warts.TryPartial"))
  def decode[F[_], T](bv: Array[Byte], read: CodedInputStream => F[T]): F[T] =
    Using(new ByteArrayInputStream(bv)) { input =>
      val cis = CodedInputStream.newInstance(input)
      read(cis)
    }.get

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
