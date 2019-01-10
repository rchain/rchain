package coop.rchain.node

import Http4sDsl.Ok

import monix.eval.Task
import org.http4s._
import org.http4s.dsl.io._

object VersionInfo {
  val get: String =
    s"RChain Node ${BuildInfo.version} (${BuildInfo.gitHeadCommit.getOrElse("commit # unknown")})"

  def service: HttpRoutes[Task] =
    HttpRoutes.of[Task] {
      case GET -> Root => Ok(get)
    }
}
