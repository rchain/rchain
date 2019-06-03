package coop.rchain

import java.net.InetAddress

import cats.effect.Sync
import cats.implicits._
import cats.mtl.ApplicativeAsk

import coop.rchain.catscontrib.ski.kp
import coop.rchain.metrics.Metrics

package object comm {
  type PeerNodeAsk[F[_]] = ApplicativeAsk[F, PeerNode]

  object PeerNodeAsk {
    def apply[F[_]](implicit ev: ApplicativeAsk[F, PeerNode]): ApplicativeAsk[F, PeerNode] = ev
  }

  val CommMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "comm")

  private def validateInetAddress[F[_]: Sync](host: String, p: InetAddress => Boolean): F[Boolean] =
    Sync[F].delay(InetAddress.getByName(host)).attempt.map(_.fold(kp(false), p))

  def isValidInetAddress[F[_]: Sync](host: String): F[Boolean] =
    validateInetAddress(host, !_.isAnyLocalAddress)

  def isValidPublicInetAddress[F[_]: Sync](host: String): F[Boolean] =
    validateInetAddress(
      host,
      a =>
        !(a.isAnyLocalAddress ||
          a.isLinkLocalAddress ||
          a.isLoopbackAddress ||
          a.isMulticastAddress ||
          a.isSiteLocalAddress)
    )
}
