package coop.rchain.node

import java.net._
import java.io._
import scala.util.Try

object IpChecker {
  def checkFrom(from: String): Option[String] =
    Try {
      val whatismyip         = new URL(from);
      val in: BufferedReader = new BufferedReader(new InputStreamReader(whatismyip.openStream()))
      InetAddress.getByName(in.readLine()).getHostAddress
    }.toOption
}
