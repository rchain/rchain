package coop.rchain.models.rholangn.parmanager

import cats.Applicative
import cats.syntax.all._
import coop.rchain.models.rholangn.RhoTypeN
import coop.rchain.models.rholangn.parmanager.primitive.PrimitiveWriter

object RhoRecWriter {
  def apply[F[_]: Applicative](
      writer: PrimitiveWriter[F],
      rec: RhoTypeN => F[Unit]
  ): RhoRecWriter[F] = new RhoRecWriter(writer, rec)
}

/** The extension of primitive types serialization with recursive function. */
class RhoRecWriter[F[_]: Applicative] private (
    writer: PrimitiveWriter[F],
    rec: RhoTypeN => F[Unit]
) {
  def writeBigInt(x: BigInt): F[Unit] = writer.write(x.toByteArray)

  // Recursive traversal
  def writePar(x: RhoTypeN): F[Unit] = rec(x)

  // Recursive traversal of a sequence
  def writeSeq(seq: Seq[RhoTypeN]): F[Unit] = writeSeq[RhoTypeN](seq, writePar)

  def writeOpt(pOpt: Option[RhoTypeN]): F[Unit] =
    pOpt.map(writer.write(true) *> writePar(_)).getOrElse(writer.write(false))

  def writeTuplePar(kv: (RhoTypeN, RhoTypeN)): F[Unit] =
    writePar(kv._1) *> writePar(kv._2)

  def writeTupleStringPar(kv: (String, RhoTypeN)): F[Unit] =
    writer.write(kv._1) *> writePar(kv._2)

  // Writes serialized value of a sequence
  def writeSeq[T](seq: Seq[T], f: T => F[Unit]): F[Unit] =
    writer.write(seq.size) *> seq.traverse_(f)
}
