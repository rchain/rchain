package coop.rchain.node.web

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import org.http4s.HttpRoutes

object StatusInfo {

  final case class Status(
      address: String,
      version: String,
      peers: Int,
      nodes: Int
  )

  def status[F[_]: Sync: ConnectionsCell: NodeDiscovery: RPConfAsk]: F[Status] =
    for {
      version <- Sync[F].delay(VersionInfo.get)
      peers   <- ConnectionsCell[F].get
      nodes   <- NodeDiscovery[F].peers
      rpConf  <- RPConfAsk[F].ask
    } yield Status(rpConf.local.toAddress, version, peers.length, nodes.length)

  def service[F[_]: Sync: ConnectionsCell: NodeDiscovery: RPConfAsk]: HttpRoutes[F] = {
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
