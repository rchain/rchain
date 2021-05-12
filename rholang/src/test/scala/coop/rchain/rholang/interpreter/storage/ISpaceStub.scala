package coop.rchain.rholang.interpreter.storage

import cats.implicits._
import cats.Applicative
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.{internal, Checkpoint, ContResult, ISpace, Match, Result, SoftCheckpoint}

import scala.collection.SortedSet

class ISpaceStub[F[_]: Applicative, C, P, A, K] extends ISpace[F, C, P, A, K] {

  implicit val m: Match[F, P, A] = (_: P, _: A) => Applicative[F].pure(none)

  override def getJoins(channel: C): F[Seq[Seq[C]]] = ???

  override def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int]
  ): F[Option[(ContResult[C, P, K], Seq[Result[C, A]])]] = ???

  override def install(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K
  ): F[Option[(K, Seq[A])]] = ???

  override def produce(
      channel: C,
      data: A,
      persist: Boolean
  ): F[Option[(ContResult[C, P, K], Seq[Result[C, A]])]] = ???

  override def createCheckpoint(): F[Checkpoint] = ???

  override def reset(root: Blake2b256Hash): F[Unit] = ???

  override def getData(channel: C): F[Seq[internal.Datum[A]]] = ???

  override def getWaitingContinuations(
      channels: Seq[C]
  ): F[Seq[internal.WaitingContinuation[P, K]]] = ???

  override def clear(): F[Unit] = ???

  override def toMap: F[Map[Seq[C], internal.Row[P, A, K]]] = ???

  override def createSoftCheckpoint(): F[SoftCheckpoint[C, P, A, K]] = ???

  override def revertToSoftCheckpoint(checkpoint: SoftCheckpoint[C, P, A, K]): F[Unit] = ???
}
