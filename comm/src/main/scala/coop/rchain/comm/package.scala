package coop.rchain

import java.net.InetAddress

import scala.util.Try

import cats.mtl.ApplicativeAsk

import coop.rchain.catscontrib.ski.kp
import coop.rchain.metrics.Metrics

package object comm {
  type PeerNodeAsk[F[_]] = ApplicativeAsk[F, PeerNode]

  object PeerNodeAsk {
    def apply[F[_]](implicit ev: ApplicativeAsk[F, PeerNode]): ApplicativeAsk[F, PeerNode] = ev
  }

  val CommMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "comm")

  def isValidInetAddress(host: String): Boolean =
    Try(InetAddress.getByName(host))
      .fold(kp(false), !_.isAnyLocalAddress)

  def isValidPublicInetAddress(host: String): Boolean =
    Try(InetAddress.getByName(host))
      .fold(
        kp(false),
        a =>
          !(a.isAnyLocalAddress ||
            a.isLinkLocalAddress ||
            a.isLoopbackAddress ||
            a.isMulticastAddress ||
            a.isSiteLocalAddress)
      )
}
