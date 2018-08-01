package coop.rchain.node

import java.io._
import java.net._

import scala.util.Try

import coop.rchain.catscontrib.Capture

object IpChecker {
  def checkFrom[F[_]: Capture](from: String): F[Option[String]] =
    Capture[F].capture {
      Try {
        val whatismyip         = new URL(from)
        val in: BufferedReader = new BufferedReader(new InputStreamReader(whatismyip.openStream()))
        InetAddress.getByName(in.readLine()).getHostAddress
      }.toOption
    }
}
