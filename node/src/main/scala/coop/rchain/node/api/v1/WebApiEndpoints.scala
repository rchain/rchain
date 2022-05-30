package coop.rchain.node.api.v1

import cats.syntax.all._
import coop.rchain.casper.protocol.{BlockInfo, LightBlockInfo}
import coop.rchain.node.api.WebApi._
import coop.rchain.node.api.json.JsonSchemaDerivations
import coop.rchain.node.web.TransactionResponse
import endpoints4s.algebra

/**
  * Defines the HTTP endpoints description of Web API v1.
  */
trait WebApiEndpoints
    extends algebra.Endpoints
    with algebra.JsonEntitiesFromSchemas
    with JsonSchemaDerivations {

  // Status

  val status: Endpoint[Unit, ApiStatus] = endpoint(
    get(path / "status"),
    ok(jsonResponse[ApiStatus]),
    docs = EndpointDocs().withDescription("API status data".some)
  )

  // Prepare deploy

  val prepareDeployGet: Endpoint[Unit, PrepareResponse] = endpoint(
    get(path / "prepare-deploy"),
    ok(jsonResponse[PrepareResponse]),
    docs = EndpointDocs().withDescription("Get data to create deploy".some)
  )

  val prepareDeployPost: Endpoint[PrepareRequest, PrepareResponse] = endpoint(
    post(path / "prepare-deploy", jsonRequest[PrepareRequest]),
    ok(jsonResponse[PrepareResponse]),
    docs = EndpointDocs().withDescription("Get data to create deploy".some)
  )

  // Deploy

  val deploy: Endpoint[DeployRequest, String] = endpoint(
    post(path / "deploy", jsonRequest[DeployRequest]),
    ok(jsonResponse[String]),
    docs =
      EndpointDocs().withDescription("Queue deployment of Rholang code (or fail to parse)".some)
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

  val dataAtName: Endpoint[DataAtNameRequest, DataAtNameResponse] = endpoint(
    post(path / "data-at-name", jsonRequest[DataAtNameRequest]),
    ok(jsonResponse[DataAtNameResponse]),
    docs = EndpointDocs().withDescription("Data at name".some)
  )

  val dataAtNameByBlockHash: Endpoint[DataAtNameByBlockHashRequest, RhoDataResponse] = endpoint(
    post(path / "data-at-name-by-block-hash", jsonRequest[DataAtNameByBlockHashRequest]),
    ok(jsonResponse[RhoDataResponse]),
    docs = EndpointDocs().withDescription("Data at name by block hash".some)
  )

  // Blocks

  val lastFinalizedBlock: Endpoint[Unit, BlockInfo] = endpoint(
    get(path / "last-finalized-block"),
    ok(jsonResponse[BlockInfo]),
    docs = EndpointDocs().withDescription("Get details about a particular block".some)
  )

  val getBlock: Endpoint[String, BlockInfo] = endpoint(
    get(path / "block" / hashString),
    ok(jsonResponse[BlockInfo]),
    docs = EndpointDocs().withDescription("Get details about a particular block".some)
  )

  val getBlocks: Endpoint[Unit, List[LightBlockInfo]] = endpoint(
    get(path / "blocks"),
    ok(jsonResponse[List[LightBlockInfo]]),
    docs = EndpointDocs().withDescription("Get a summary of blocks on the blockchain".some)
  )

  val getBlocksByHeights: Endpoint[(Long, Long), List[LightBlockInfo]] = endpoint(
    get(path / "blocks" / startBlockNumber / endBlockNumber),
    ok(jsonResponse[List[LightBlockInfo]]),
    docs = EndpointDocs().withDescription("Get blocks by block height".some)
  )

  val getBlocksByDepth: Endpoint[Int, List[LightBlockInfo]] = endpoint(
    get(path / "blocks" / depth),
    ok(jsonResponse[List[LightBlockInfo]]),
    docs = EndpointDocs().withDescription("Get a summary of blocks on the blockchain".some)
  )

  val findDeploy: Endpoint[String, LightBlockInfo] = endpoint(
    get(path / "deploy" / deployId),
    ok(jsonResponse[LightBlockInfo]),
    docs = EndpointDocs().withDescription("Find block containing a deploy".some)
  )

  val isFinalized: Endpoint[String, Boolean] = endpoint(
    get(path / "is-finalized" / hashString),
    ok(jsonResponse[Boolean]),
    docs = EndpointDocs().withDescription("Check if a given block is finalized".some)
  )

  val getTransaction: Endpoint[String, TransactionResponse] = endpoint(
    get(path / "transactions" / hashString),
    ok(jsonResponse[TransactionResponse])
  )

  // Segments

  lazy val hashString: Path[String] =
    segment[String](name = "hash", docs = "Hex encoded string".some)
  lazy val startBlockNumber: Path[Long] =
    segment[Long](name = "startBlockNumber", docs = "Start block number".some)
  lazy val endBlockNumber: Path[Long] =
    segment[Long](name = "endBlockNumber", docs = "End block number".some)
  lazy val depth: Path[Int]       = segment[Int](name = "depth", docs = "Request depth".some)
  lazy val deployId: Path[String] = segment[String](name = "deployId", docs = "Deploy ID".some)
}
