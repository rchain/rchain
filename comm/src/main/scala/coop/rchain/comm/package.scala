package coop.rchain

import cats.mtl.ApplicativeAsk

package object comm {
  trait TcpConnTag
  trait KademliaConnTag

  type PeerNodeAsk[F[_]] = ApplicativeAsk[F, PeerNode]

  object PeerNodeAsk {
    def apply[F[_]](implicit ev: ApplicativeAsk[F, PeerNode]): ApplicativeAsk[F, PeerNode] = ev
  }

}
