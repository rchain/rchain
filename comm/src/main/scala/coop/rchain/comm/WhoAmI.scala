package coop.rchain.comm

import java.io.{BufferedReader, InputStreamReader}
import java.net.{InetAddress, URL}

import scala.util.Try

import cats.effect.Sync
import cats.syntax.all._
import cats.Applicative

import coop.rchain.catscontrib.ski.kp
import coop.rchain.shared.Log

object WhoAmI {

  def fetchLocalPeerNode[F[_]: Sync: Log](
      host: Option[String],
      protocolPort: Int,
      discoveryPort: Int,
      noUpnp: Boolean,
      id: NodeIdentifier
  ): F[PeerNode] =
    for {
      externalAddress <- retrieveExternalAddress(noUpnp, List(protocolPort, discoveryPort))
      host            <- fetchHost(host, externalAddress)
      peerNode        = PeerNode.from(id, host, protocolPort, discoveryPort)
    } yield peerNode

  def checkLocalPeerNode[F[_]: Sync: Log](
      protocolPort: Int,
      discoveryPort: Int,
      peerNode: PeerNode
  ): F[Option[PeerNode]] =
    for {
      r      <- checkAll()
      (_, a) = r
      host <- if (a == peerNode.endpoint.host) Option.empty[String].pure[F]
             else Log[F].info(s"external IP address has changed to $a").map(kp(Option(a)))
    } yield host.map(h => PeerNode.from(peerNode.id, h, protocolPort, discoveryPort))

  private def fetchHost[F[_]: Sync: Log](
      host: Option[String],
      externalAddress: Option[String]
  ): F[String] =
    host match {
      case Some(h) => h.pure[F]
      case None    => whoAmI(externalAddress)
    }

  private def retrieveExternalAddress[F[_]: Sync: Log](
      noUpnp: Boolean,
      ports: List[Int]
  ): F[Option[String]] =
    if (noUpnp) Option.empty[String].pure[F]
    else UPnP.assurePortForwarding[F](ports)

  private def check[F[_]: Sync](source: String, from: String): F[(String, Option[String])] =
    checkFrom(from).map((source, _))

  private def checkFrom[F[_]: Sync](from: String): F[Option[String]] =
    Sync[F].delay {
      Try {
        val whatismyip         = new URL(from)
        val in: BufferedReader = new BufferedReader(new InputStreamReader(whatismyip.openStream()))
        InetAddress.getByName(in.readLine()).getHostAddress
      }.toOption
    }

  private def checkNext[F[_]: Applicative](
      prev: (String, Option[String]),
      next: => F[(String, Option[String])]
  ): F[(String, Option[String])] =
    prev._2.fold(next)(_ => prev.pure[F])

  private def upnpIpCheck[F[_]: Sync](
      externalAddress: Option[String]
  ): F[(String, Option[String])] =
    Sync[F].delay(("UPnP", externalAddress.map(InetAddress.getByName(_).getHostAddress)))

  private def checkAll[F[_]: Sync](externalAddress: Option[String] = None): F[(String, String)] =
    for {
      r1 <- check("AmazonAWS service", "http://checkip.amazonaws.com")
      r2 <- checkNext(r1, check("WhatIsMyIP service", "http://bot.whatismyipaddress.com"))
      r3 <- checkNext(r2, upnpIpCheck(externalAddress))
      r4 <- checkNext(r3, ("failed to guess", Option("localhost")).pure[F])
    } yield {
      val (s, aOpt) = r4
      (s, aOpt.get)
    }

  private def whoAmI[F[_]: Sync: Log](externalAddress: Option[String]): F[String] =
    for {
      _      <- Log[F].info("flag --host was not provided, guessing your external IP address")
      r      <- checkAll(externalAddress)
      (s, a) = r
      _      <- Log[F].info(s"guessed $a from source: $s")
    } yield a
}
