package coop.rchain.node.api.v1

import coop.rchain.casper.protocol.{BlockInfo, LightBlockInfo}
import coop.rchain.node.api.WebApi.{
  ApiStatus,
  DataAtNameByBlockHashRequest,
  DeployRequest,
  ExploreDeployRequest,
  RhoDataResponse
}
import coop.rchain.node.api.json.JsonSchemaDerivations
import endpoints4s.algebra
import cats.syntax.all._
import coop.rchain.node.web.TransactionResponse

/**
  * Defines the HTTP endpoints description of Web API v1.
  */
trait WebApiEndpoints
    extends algebra.Endpoints
    with algebra.JsonEntitiesFromSchemas
    with JsonSchemaDerivations {

  val status: Endpoint[Unit, ApiStatus] = endpoint(
    get(path / "status"),
    ok(jsonResponse[ApiStatus]),
    docs = EndpointDocs().withDescription("API status data".some)
  )

  // Prepare deploy

  // Deploy

  val deploy: Endpoint[DeployRequest, String] = endpoint(
    post(path / "deploy", jsonRequest[DeployRequest]),
    ok(jsonResponse[String])
  )

  val exploreDeploy: Endpoint[String, RhoDataResponse] = endpoint(
    post(path / "explore-deploy", jsonRequest[String]),
    ok(jsonResponse[RhoDataResponse]),
    docs = EndpointDocs().withDescription("Exploratory deploy on last finalized state".some)
  )

  val exploreDeployByBlockHash: Endpoint[ExploreDeployRequest, RhoDataResponse] = endpoint(
    post(path / "explore-deploy-by-block-hash", jsonRequest[ExploreDeployRequest]),
    ok(jsonResponse[RhoDataResponse]),
    docs = EndpointDocs().withDescription("Exploratory deploy".some)
  )

  // Get data

  val dataAtName: Endpoint[DataAtNameByBlockHashRequest, RhoDataResponse] = endpoint(
    post(path / "data-at-name-by-block-hash", jsonRequest[DataAtNameByBlockHashRequest]),
    ok(jsonResponse[RhoDataResponse])
  )

  // Blocks

  val getBlocks: Endpoint[Unit, List[LightBlockInfo]] = endpoint(
    get(path / "blocks"),
    ok(jsonResponse[List[LightBlockInfo]])
  )

  val getBlock: Endpoint[String, BlockInfo] = endpoint(
    get(path / "block" / hashString),
    ok(jsonResponse[BlockInfo])
  )

  val getTransaction: Endpoint[String, TransactionResponse] = endpoint(
    get(path / "transactions" / hashString),
    ok(jsonResponse[TransactionResponse])
  )

  // Segments

  lazy val hashString = segment[String](name = "hash", docs = "Hex encoded string".some)
}
