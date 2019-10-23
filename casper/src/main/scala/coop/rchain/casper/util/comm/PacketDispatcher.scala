package coop.rchain.casper.util.comm

import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.Semaphore
import cats.syntax.all._
import cats.Show

import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.shared.Log
import FairRoundRobinDispatcher._

trait PacketDispatcher[F[_]] {
  def dispatch(peer: PeerNode, packet: Packet): F[Unit]
}

object PacketDispatcher {
  def apply[F[_]](ev: PacketDispatcher[F]): PacketDispatcher[F] = ev

  implicit private val showPeerNode: Show[PeerNode] = _.toString

  def fairRoundRobin[F[_]: Sync: Log: PacketHandler](
      filter: Packet => F[Dispatch],
      maxPeerQueueSize: Int,
      giveUpAfterSkipped: Int,
      dropPeerAfterRetries: Int
  ): F[PacketDispatcher[F]] =
    FairRoundRobinDispatcher[F, PeerNode, Packet](
      filter,
      PacketHandler[F].handlePacket,
      maxPeerQueueSize,
      giveUpAfterSkipped,
      dropPeerAfterRetries
    ).map(dispatcher => (peer: PeerNode, packet: Packet) => dispatcher.dispatch(peer, packet))

  def concurrentFairRoundRobin[F[_]: Concurrent: Log: PacketHandler](
      filter: Packet => F[Dispatch],
      maxPeerQueueSize: Int,
      giveUpAfterSkipped: Int,
      dropPeerAfterRetries: Int
  ): F[PacketDispatcher[F]] =
    for {
      lock <- Semaphore(1)
      dispatcher <- fairRoundRobin(
                     filter,
                     maxPeerQueueSize,
                     giveUpAfterSkipped,
                     dropPeerAfterRetries
                   )
    } yield new PacketDispatcher[F] {
      def dispatch(peer: PeerNode, packet: Packet): F[Unit] =
        lock.withPermit(
          dispatcher.dispatch(peer, packet)
        )
    }
}
