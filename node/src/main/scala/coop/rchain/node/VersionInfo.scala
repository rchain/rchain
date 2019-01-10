package coop.rchain.node

import cats.effect.Sync
import cats.implicits._

import org.http4s.HttpRoutes

object VersionInfo {
  val get: String =
    s"RChain Node ${BuildInfo.version} (${BuildInfo.gitHeadCommit.getOrElse("commit # unknown")})"

  def service[F[_]: Sync]: HttpRoutes[F] = {
    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root => Ok(get.pure[F])
    }
  }
}
