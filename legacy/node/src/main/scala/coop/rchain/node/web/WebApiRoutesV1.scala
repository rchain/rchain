package coop.rchain.node.web

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.node.api.json.JsonEntitiesCirceFromSchema
import coop.rchain.node.api.v1.{WebApiAdminEndpoints, WebApiEndpoints}
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.shared.Log
import endpoints4s.http4s.server.Endpoints
import org.http4s.HttpRoutes

import scala.Function.const

/**
  * Definition for RNode Web API v1 HTTP routes (https4s).
  */
object WebApiRoutesV1 {

  /**
    * Creates routes for Web API v1.
    *
    * @param webApi Web API implementation
    * @return http4s routes (including OpenAPI schema _openapi.json_)
    */
  def create[F[_]: Concurrent: Log](webApi: WebApi[F]): HttpRoutes[F] = {
    // RNode WebApi v1 routes
    val apiRoutes = HttpRoutes.of[F](WebApiRoutesV1(webApi).publicRoutes)
    // OpenAPI schema route
    val docRoutes = HttpRoutes.of[F](WebApiDocServer[F]().publicRoutes)

    apiRoutes <+> docRoutes
  }

  /**
    * Creates routes for Admin Web API v1 (includes the whole public Web API).
    *
    * @param adminWebApi Admin Web API implementation
    * @return http4s routes (including OpenAPI schema _openapi.json_)
    */
  def createAdmin[F[_]: Concurrent: Log](
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F]
  ): HttpRoutes[F] = {
    // RNode WebApi v1 routes
    val publicRoutes = HttpRoutes.of[F](WebApiRoutesV1(webApi).publicRoutes)
    // RNode Admin WebApi v1 routes
    val adminRoutes = HttpRoutes.of[F](AdminWebApiRoutesV1(adminWebApi).adminRoutes)
    // OpenAPI schema route
    val docRoutes = HttpRoutes.of[F](WebApiDocServer[F]().adminRoutes)

    publicRoutes <+> adminRoutes <+> docRoutes
  }
}

/**
  * Defines implementation (interpreter) for Web API endpoints.
  */
final case class WebApiRoutesV1[F[_]: Concurrent: Log](
    webApi: WebApi[F]
) extends Endpoints[F]
    with JsonEntitiesCirceFromSchema
    with WebApiEndpoints {

  val publicRoutes = routesFromEndpoints(
    // Status
    status.implementedByEffect(const(webApi.status)),
    // Get data
    exploreDeploy.implementedByEffect(
      webApi.exploratoryDeploy(_, blockHash = none, usePreStateHash = false)
    ),
    // Deploy
    deploy.implementedByEffect(webApi.deploy),
    deployStatus.implementedByEffect(webApi.deployStatus),
    // Blocks
    getBlocks.implementedByEffect(const(webApi.getBlocks(1))),
    getBlock.implementedByEffect(webApi.getBlock)
  )
}

/**
  * Defines implementation (interpreter) for Admin Web API endpoints.
  */
final case class AdminWebApiRoutesV1[F[_]: Sync](
    adminWebApi: AdminWebApi[F]
) extends Endpoints[F]
    with JsonEntitiesCirceFromSchema
    with WebApiAdminEndpoints {

  val adminRoutes = routesFromEndpoints(
    // Propose
    propose.implementedByEffect(const(adminWebApi.propose))
  )
}
