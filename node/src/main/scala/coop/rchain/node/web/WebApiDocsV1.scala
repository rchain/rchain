package coop.rchain.node.web

import cats.effect.kernel.Concurrent
import coop.rchain.node.api.v1.{WebApiAdminEndpoints, WebApiEndpoints}
import endpoints4s.http4s.server
import endpoints4s.http4s.server.Endpoints
import endpoints4s.openapi
import endpoints4s.openapi.model.{Info, OpenApi}

import scala.Function.const

/**
  * OpenAPI schema definition for RNode Web API v1.
  */
object WebApiDocs
    extends WebApiEndpoints
    with WebApiAdminEndpoints
    with openapi.Endpoints
    with openapi.JsonEntitiesFromSchemas {

  val public =
    Seq(
      status,
      deploy,
      deployStatus,
      exploreDeploy,
      exploreDeployByBlockHash,
      dataAtName,
      getBlocks,
      getBlock
    )

  val admin = Seq(propose)

  // Public API Open API schema
  val publicApi: OpenApi = openApi(Info(title = "RNode API", version = "1.0"))(public: _*)

  // Admin API includes the whole public API schema
  val adminApi: OpenApi =
    openApi(Info(title = "RNode API (admin)", version = "1.0"))(public ++ admin: _*)
}

/**
  * OpenAPI endpoint definition (GET /openapi.json).
  */
final case class WebApiDocServer[F[_]: Concurrent]()
    extends Endpoints[F]
    with server.JsonEntitiesFromEncodersAndDecoders {
  implicit val jCodec: endpoints4s.Encoder[OpenApi, String] = OpenApi.stringEncoder

  val publicRoutes = endpoint(get(path / "openapi.json"), ok(jsonResponse[OpenApi]))
    .implementedBy(const(WebApiDocs.publicApi))

  val adminRoutes = endpoint(get(path / "openapi.json"), ok(jsonResponse[OpenApi]))
    .implementedBy(const(WebApiDocs.adminApi))
}
