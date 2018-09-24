package coop.rchain.node

import cats.effect.IO
import org.http4s._
import org.http4s.dsl.io._

object VersionInfo {
  val get: String =
    s"RChain Node ${BuildInfo.version} (${BuildInfo.gitHeadCommit.getOrElse("commit # unknown")})"

  def service = HttpService[IO] {
    case GET -> Root => Ok(get)
  }
}
