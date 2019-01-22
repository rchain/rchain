package coop.rchain.node

import cats.effect.Sync
import cats.implicits._

import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.ConnectionsCell

import org.http4s.HttpRoutes

object StatusInfo {

  final case class Status(
      version: String,
      peers: Int,
      nodes: Int
  )

  def status[F[_]: Sync: ConnectionsCell: NodeDiscovery]: F[Status] =
    for {
      version <- Sync[F].delay(VersionInfo.get)
      peers   <- ConnectionsCell[F].read
      nodes   <- NodeDiscovery[F].peers
    } yield Status(version, peers.length, nodes.length)

  def service[F[_]: Sync: ConnectionsCell: NodeDiscovery]: HttpRoutes[F] = {
    import io.circe.generic.auto._
    import io.circe.syntax._
    import org.http4s.circe.CirceEntityEncoder._

    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root => Ok(status.map(_.asJson))
    }
  }
}
