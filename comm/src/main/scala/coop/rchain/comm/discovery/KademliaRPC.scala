package coop.rchain.comm.discovery

import scala.concurrent.duration.Duration

import cats._, cats.data._

import coop.rchain.catscontrib.{MonadTrans, _}
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing._

trait KademliaRPC[F[_]] {
  def ping(node: PeerNode): F[Boolean]
  def lookup(key: Seq[Byte], peer: PeerNode): F[Seq[PeerNode]]
  def receive(
      pingHandler: PeerNode => F[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => F[Seq[PeerNode]]
  ): F[Unit]
}

object KademliaRPC {
  def apply[F[_]](implicit P: KademliaRPC[F]): KademliaRPC[F] = P
}
