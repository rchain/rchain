package coop.rchain.node.api.v1

import cats.syntax.all._
import coop.rchain.node.api.json.JsonSchemaDerivations
import endpoints4s.algebra

/**
  * Defines the HTTP endpoints description of Admin Web API v1.
  */
trait WebApiAdminEndpoints
    extends algebra.Endpoints
    with algebra.JsonEntitiesFromSchemas
    with JsonSchemaDerivations {

  val propose: Endpoint[Unit, String] = endpoint(
    get(path / "propose"),
    ok(jsonResponse[String]),
    docs = EndpointDocs().withDescription("Create and propose block".some)
  )
}
