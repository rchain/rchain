package coop.rchain.casper

import cats.Applicative, cats.implicits._

import coop.rchain.casper.protocol.{BlockMessage, Resource}

trait Casper[F[_], A] {
  def addBlock(b: BlockMessage): F[Unit]
  def contains(b: BlockMessage): F[Boolean]
  def deploy(r: Resource): F[Unit]
  def estimator: F[A]
  def proposeBlock: F[Option[BlockMessage]]
}

trait MultiParentCasper[F[_]] extends Casper[F, IndexedSeq[BlockMessage]]

object MultiParentCasper extends MultiParentCasperInstances {
  def apply[F[_]](implicit instance: MultiParentCasper[F]): MultiParentCasper[F] = instance
}

sealed abstract class MultiParentCasperInstances {
  def noCasper[F[_]: Applicative]: MultiParentCasper[F] = new MultiParentCasper[F] {
    def addBlock(b: BlockMessage): F[Unit]    = ().pure[F]
    def contains(b: BlockMessage): F[Boolean] = false.pure[F]
    def deploy(r: Resource): F[Unit]          = ().pure[F]
    def estimator: F[IndexedSeq[BlockMessage]] =
      Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
    def proposeBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
  }
}
