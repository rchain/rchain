package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._
import coop.rchain.casper.protocol.Resource.ResourceClass.ProduceResource
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.catscontrib.{Capture, IOUtil}
import coop.rchain.p2p.Network.{ErrorHandler, KeysStore}
import coop.rchain.p2p.effects._

trait Casper[F[_], A] {
  def addBlock(b: BlockMessage): F[Unit]
  def contains(b: BlockMessage): F[Boolean]
  def deploy(r: Resource): F[Unit]
  def estimator: F[A]
  def proposeBlock: F[Option[BlockMessage]]
  def shouldSendBlock: F[Unit]
}

trait MultiParentCasper[F[_]] extends Casper[F, IndexedSeq[BlockMessage]]

object MultiParentCasper extends MultiParentCasperInstances {
  def apply[F[_]](implicit instance: MultiParentCasper[F]): MultiParentCasper[F] = instance
}

sealed abstract class MultiParentCasperInstances {
  def noCasper[F[_]: Applicative]: MultiParentCasper[F] =
    new MultiParentCasper[F] {
      def addBlock(b: BlockMessage): F[Unit]    = ().pure[F]
      def contains(b: BlockMessage): F[Boolean] = false.pure[F]
      def deploy(r: Resource): F[Unit]          = ().pure[F]
      def estimator: F[IndexedSeq[BlockMessage]] =
        Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
      def proposeBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
      def shouldSendBlock: F[Unit]              = ().pure[F]
    }

  def simpleCasper[
      F[_]: Monad: Capture: Applicative: NodeDiscovery: TransportLayer: Log: Time: Encryption: KeysStore: ErrorHandler]
    : MultiParentCasper[F] = new MultiParentCasper[F] {
    def addBlock(b: BlockMessage): F[Unit]    = ().pure[F]
    def contains(b: BlockMessage): F[Boolean] = false.pure[F]
    def deploy(r: Resource): F[Unit]          = ().pure[F]
    def estimator: F[IndexedSeq[BlockMessage]] =
      Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
    def proposeBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
    def shouldSendBlock: F[Unit] =
      for {
        _           <- IOUtil.sleep[F](5000L)
        currentTime <- Time[F].currentMillis
        postState = RChainState().withResources(
          Seq(Resource(ProduceResource(Produce(currentTime.toInt)))))
        block = BlockMessage().withBody(Body().withPostState(postState))
        _     <- CommUtil.sendBlock[F](block)
      } yield ()
  }
}
