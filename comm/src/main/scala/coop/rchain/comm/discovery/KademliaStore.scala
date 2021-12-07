package coop.rchain.comm.discovery

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm._
import coop.rchain.metrics.Metrics

trait KademliaStore[F[_]] {
  def peers: F[Seq[PeerNode]]
  def sparseness: F[Seq[Int]]
  def updateLastSeen(peerNode: PeerNode): F[Unit]
  def lookup(key: Seq[Byte]): F[Seq[PeerNode]]
  def find(key: Seq[Byte]): F[Option[PeerNode]]
  def remove(key: Seq[Byte]): F[Unit]
}

object KademliaStore extends KademliaStoreInstances {
  def apply[F[_]](implicit ev: KademliaStore[F]): KademliaStore[F] = ev
}

sealed abstract class KademliaStoreInstances {

  def table[F[_]: Sync: KademliaRPC: Metrics](id: NodeIdentifier): KademliaStore[F] =
    new KademliaStore[F] {
      implicit private val metricsSource: Metrics.Source = DiscoveryMetricsSource
      private val table                                  = PeerTable[PeerNode, F](id.key)

      def peers: F[Seq[PeerNode]]                   = table.peers
      def sparseness: F[Seq[Int]]                   = table.sparseness
      def lookup(key: Seq[Byte]): F[Seq[PeerNode]]  = table.lookup(key)
      def find(key: Seq[Byte]): F[Option[PeerNode]] = table.find(key)

      def remove(key: Seq[Byte]): F[Unit] =
        for {
          _     <- table.remove(key)
          peers <- table.peers
          _     <- Metrics[F].setGauge("peers", peers.length.toLong)
        } yield ()

      def updateLastSeen(peerNode: PeerNode): F[Unit] =
        for {
          _     <- table.updateLastSeen(peerNode)
          peers <- table.peers
          _     <- Metrics[F].setGauge("peers", peers.length.toLong)
        } yield ()
    }
}
