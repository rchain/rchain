package coop.rchain.models.rholangn.parmanager.primitive.syntax

import cats.Applicative
import cats.syntax.all._
import coop.rchain.models.rholangn.parmanager.primitive.PrimitiveWriter

final class PrimitiveWriterOps[F[_]](val writer: PrimitiveWriter[F]) extends AnyVal {
  def writeBigInt(x: BigInt): F[Unit] = writer.write(x.toByteArray)

  def writeOpt[T](pOpt: Option[T], writeT: T => F[Unit])(
      implicit applicativeF: Applicative[F]
  ): F[Unit] =
    pOpt.map(writer.write(true) *> writeT(_)).getOrElse(writer.write(false))

  def writeSeq[T](seq: Seq[T], writeT: T => F[Unit])(
      implicit applicativeF: Applicative[F]
  ): F[Unit] =
    writer.write(seq.size) *> seq.traverse_(writeT)

  def writeTuple[T](kv: (T, T), writeT: T => F[Unit])(
      implicit applicativeF: Applicative[F]
  ): F[Unit] = writeT(kv._1) *> writeT(kv._2)

  def writeTupleStringT[T](kv: (String, T), writeT: T => F[Unit])(
      implicit applicativeF: Applicative[F]
  ): F[Unit] = writer.write(kv._1) *> writeT(kv._2)
}

object PrimitiveWriterSyntax {
  implicit final def primitiveWriterSyntax[F[_]: Applicative](
      writer: PrimitiveWriter[F]
  ): PrimitiveWriterOps[F] = new PrimitiveWriterOps[F](writer)
}
