package coop.rchain.node

import org.http4s._
import org.http4s.dsl.io._
import monix.eval.Task

object VersionInfo {
  val get: String =
    s"RChain Node ${BuildInfo.version} (${BuildInfo.gitHeadCommit.getOrElse("commit # unknown")})"

  def service = HttpRoutes.of[Task] {
    case GET -> Root => Ok(get)
  }
}
