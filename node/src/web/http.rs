//! HTTP server routes (port of the http4s `service` methods in the `web` package and
//! `NewPrometheusReporter.service`).
//!
//! The public server mounts `/version`, `/metrics`, `/status`, the `/api` + `/api/v1` JSON routes,
//! the `/api/v1/openapi.json` OpenAPI document, and the reporting routes (`/reporting/trace` +
//! `/api/trace`). The admin server mounts the `/api`/`/api/v1` admin routes (propose) and its own
//! OpenAPI document. CORS and a per-request timeout are applied to both servers.

use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use axum::extract::{Path, Query, State};
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use tower_http::cors::CorsLayer;
use tower_http::timeout::TimeoutLayer;

use rchain_casper::api::block_report_api::BlockReportApi;
use rchain_casper::protocol::comm_util::ConnectionsCell;
use rchain_comm::discovery::NodeDiscovery;
use rchain_comm::rp::rp_conf::RPConf;
use rchain_models::block_hash::BlockHash;
use rchain_shared::rate_limiter::RateLimiter;
use rchain_shared::refined::Port;

use crate::api::admin_web_api::AdminWebApi;
use crate::api::grpc::DEFAULT_API_RATE_LIMIT_PER_SEC;
use crate::api::dto::{
    BlockApiException, DataAtNameByBlockHashRequest, DataAtNameRequest, DeployRequest,
    ExploreDeployRequest,
};
use crate::api::web_api::WebApi;
use crate::diagnostics::NewPrometheusReporter;
use crate::web::reporting::transform_result;
use crate::web::status_info;
use crate::web::version_info;

/// Comm state needed by `GET /status` (port of the `ConnectionsCell`/`NodeDiscovery`/`RPConfAsk`
/// arguments of `StatusInfo.service`).
#[derive(Clone)]
pub struct StatusProvider {
    pub connections: ConnectionsCell,
    pub rp_conf: RPConf,
    pub discovery: Arc<dyn NodeDiscovery>,
}

/// State shared by the public HTTP server (port of the `webApi` + `prometheusReporter` +
/// `blockReportAPI` arguments of `acquireHttpServer`).
#[derive(Clone)]
pub struct HttpState {
    pub reporter: Arc<NewPrometheusReporter>,
    pub web_api: Arc<dyn WebApi>,
    pub block_report_api: Arc<BlockReportApi>,
    pub status_provider: Option<StatusProvider>,
    pub enable_reporting: bool,
    /// Rate limiter for the unauthenticated deploy/explore-deploy routes (documented Scala
    /// deviation: the Scala HTTP deploy routes are unlimited).
    pub deploy_rate_limiter: Arc<RateLimiter>,
}

/// State shared by the admin HTTP server (port of the `adminWebApiRoutes` argument of
/// `acquireAdminHttpServer`).
#[derive(Clone)]
pub struct AdminState {
    pub admin_web_api: Arc<dyn AdminWebApi>,
}

/// `GET /version` (port of `VersionInfo.service`): the node version string.
pub async fn version() -> String {
    version_info::get(env!("CARGO_PKG_VERSION"), None)
}

/// `GET /metrics` (port of `NewPrometheusReporter.service`): the Prometheus scrape data.
pub async fn metrics(State(state): State<HttpState>) -> String {
    state.reporter.scrape_data()
}

/// `GET /status` (port of `StatusInfo.service`): the node address, version, and peer/node counts.
pub async fn status(State(state): State<HttpState>) -> Response {
    match &state.status_provider {
        Some(provider) => {
            let connections = provider.connections.read().await;
            let discovered = provider.discovery.peers();
            let version = version_info::get(env!("CARGO_PKG_VERSION"), None);
            let status = status_info::status(&version, &connections, &discovered, &provider.rp_conf);
            (StatusCode::OK, Json(status)).into_response()
        }
        None => (StatusCode::NOT_FOUND, ()).into_response(),
    }
}

/// Map a `WebApi`/`AdminWebApi` result to an HTTP response: `200` with a JSON body on success,
/// `400` with a JSON error string on `BlockApiException` (port of the `handleResponseError`
/// handler in `WebApiRoutes`).
fn json_result<T: Serialize>(result: Result<T, BlockApiException>) -> Response {
    match result {
        Ok(value) => (StatusCode::OK, Json(value)).into_response(),
        Err(err) => (StatusCode::BAD_REQUEST, Json(err.0)).into_response(),
    }
}

// --- Web API routes (port of `WebApiRoutes.service`) ---

async fn api_status(State(state): State<HttpState>) -> Response {
    json_result(state.web_api.status().await)
}

async fn api_deploy(State(state): State<HttpState>, Json(req): Json<DeployRequest>) -> Response {
    if !state.deploy_rate_limiter.allow() {
        return (StatusCode::TOO_MANY_REQUESTS, Json("deploy rate limit exceeded".to_string()))
            .into_response();
    }
    json_result(state.web_api.deploy(&req).await)
}

async fn api_explore_deploy(State(state): State<HttpState>, Json(term): Json<String>) -> Response {
    if !state.deploy_rate_limiter.allow() {
        return (StatusCode::TOO_MANY_REQUESTS, Json("deploy rate limit exceeded".to_string()))
            .into_response();
    }
    json_result(state.web_api.exploratory_deploy(&term, None, false).await)
}

async fn api_explore_deploy_by_block_hash(
    State(state): State<HttpState>,
    Json(req): Json<ExploreDeployRequest>,
) -> Response {
    if !state.deploy_rate_limiter.allow() {
        return (StatusCode::TOO_MANY_REQUESTS, Json("deploy rate limit exceeded".to_string()))
            .into_response();
    }
    let block_hash = if req.block_hash.is_empty() {
        None
    } else {
        Some(req.block_hash.as_str())
    };
    json_result(
        state
            .web_api
            .exploratory_deploy(&req.term, block_hash, req.use_pre_state_hash)
            .await,
    )
}

async fn api_data_at_name(
    State(state): State<HttpState>,
    Json(req): Json<DataAtNameRequest>,
) -> Response {
    json_result(state.web_api.listen_for_data_at_name(&req).await)
}

async fn api_data_at_name_by_block_hash(
    State(state): State<HttpState>,
    Json(req): Json<DataAtNameByBlockHashRequest>,
) -> Response {
    json_result(state.web_api.get_data_at_par(&req).await)
}

async fn api_last_finalized_block(State(state): State<HttpState>) -> Response {
    json_result(state.web_api.last_finalized_block().await)
}

async fn api_get_block(State(state): State<HttpState>, Path(hash): Path<String>) -> Response {
    json_result(state.web_api.get_block(&hash).await)
}

async fn api_get_blocks(State(state): State<HttpState>) -> Response {
    json_result(state.web_api.get_blocks(1).await)
}

async fn api_get_blocks_by_heights(
    State(state): State<HttpState>,
    Path((start, end)): Path<(i64, i64)>,
) -> Response {
    json_result(state.web_api.get_blocks_by_heights(start, end).await)
}

async fn api_get_blocks_by_depth(
    State(state): State<HttpState>,
    Path(depth): Path<i32>,
) -> Response {
    json_result(state.web_api.get_blocks(depth).await)
}

async fn api_find_deploy(
    State(state): State<HttpState>,
    Path(deploy_id): Path<String>,
) -> Response {
    json_result(state.web_api.find_deploy(&deploy_id).await)
}

async fn api_is_finalized(State(state): State<HttpState>, Path(hash): Path<String>) -> Response {
    json_result(state.web_api.is_finalized(&hash).await)
}

async fn api_get_transaction(
    State(state): State<HttpState>,
    Path(hash): Path<String>,
) -> Response {
    json_result(state.web_api.get_transaction(&hash).await)
}

// --- Web API v1 routes (port of `WebApiRoutesV1`; the OpenAPI schema is served below) ---

async fn api_v1_deploy_status(
    State(state): State<HttpState>,
    Path(deploy_signature): Path<String>,
) -> Response {
    json_result(state.web_api.deploy_status(&deploy_signature).await)
}

/// `GET /api/v1/openapi.json` — the OpenAPI 3.0 document describing the v1 API. Hand-written from the
/// endpoint DTOs (the Scala derives the same schema from its endpoints4s algebra).
const OPENAPI_JSON: &str = r##"{
  "openapi": "3.0.0",
  "info": { "title": "RNode API", "version": "1.0" },
  "paths": {
    "/status": {
      "get": {
        "summary": "Node status",
        "responses": {
          "200": {
            "description": "Node version, address and peer/node counts",
            "content": { "application/json": { "schema": { "$ref": "#/components/schemas/ApiStatus" } } }
          }
        }
      }
    },
    "/deploy": {
      "post": {
        "summary": "Deploy a signed rholang term",
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "$ref": "#/components/schemas/DeployRequest" } } } },
        "responses": {
          "200": { "description": "Deploy accepted", "content": { "application/json": { "schema": { "type": "string" } } } },
          "400": { "description": "Invalid deploy" }
        }
      }
    },
    "/deploy-status/{deploySignature}": {
      "get": {
        "summary": "Deploy execution status",
        "parameters": [ { "name": "deploySignature", "in": "path", "required": true, "schema": { "type": "string" } } ],
        "responses": {
          "200": { "description": "Deploy execution status", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/DeployExecStatus" } } } },
          "400": { "description": "Invalid deploy signature" }
        }
      }
    },
    "/explore-deploy": {
      "post": {
        "summary": "Run an exploratory deploy",
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "$ref": "#/components/schemas/ExploreDeployRequest" } } } },
        "responses": {
          "200": { "description": "Result expression", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/ExploratoryDeployResponse" } } } },
          "400": { "description": "Deploy failed" }
        }
      }
    },
    "/explore-deploy-by-block-hash": {
      "post": {
        "summary": "Exploratory deploy at a block hash",
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "$ref": "#/components/schemas/ExploreDeployRequest" } } } },
        "responses": {
          "200": { "description": "Result expression", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/ExploratoryDeployResponse" } } } },
          "400": { "description": "Deploy failed" }
        }
      }
    },
    "/data-at-name-by-block-hash": {
      "post": {
        "summary": "Data at a name, at a block hash",
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "$ref": "#/components/schemas/DataAtNameByBlockHashRequest" } } } },
        "responses": {
          "200": { "description": "Data at the name", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/RhoDataResponse" } } } },
          "400": { "description": "Invalid request" }
        }
      }
    },
    "/blocks": {
      "get": {
        "summary": "Recent blocks",
        "responses": {
          "200": { "description": "Recent lightweight block info", "content": { "application/json": { "schema": { "type": "array", "items": { "$ref": "#/components/schemas/LightBlockInfo" } } } } }
        }
      }
    },
    "/block/{hash}": {
      "get": {
        "summary": "A block by hash",
        "parameters": [ { "name": "hash", "in": "path", "required": true, "schema": { "type": "string" } } ],
        "responses": {
          "200": { "description": "Block and its deploys", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/BlockInfo" } } } },
          "400": { "description": "Invalid block hash" }
        }
      }
    },
    "/propose": {
      "post": {
        "summary": "Propose a block",
        "responses": {
          "200": { "description": "Proposal result", "content": { "application/json": { "schema": { "type": "string" } } } }
        }
      }
    }
  },
  "components": {
    "schemas": {
      "VersionInfo": {
        "type": "object",
        "properties": {
          "api": { "type": "string" },
          "node": { "type": "string" }
        }
      },
      "ApiStatus": {
        "type": "object",
        "properties": {
          "version": { "$ref": "#/components/schemas/VersionInfo" },
          "address": { "type": "string" },
          "networkId": { "type": "string" },
          "shardId": { "type": "string" },
          "peers": { "type": "integer", "format": "int32" },
          "nodes": { "type": "integer", "format": "int32" },
          "minPhloPrice": { "type": "integer", "format": "int64" },
          "latestBlockNumber": { "type": "integer", "format": "int64" }
        }
      },
      "DeployData": {
        "type": "object",
        "properties": {
          "term": { "type": "string" },
          "timestamp": { "type": "integer", "format": "int64" },
          "phloPrice": { "type": "integer", "format": "int64" },
          "phloLimit": { "type": "integer", "format": "int64" },
          "validAfterBlockNumber": { "type": "integer", "format": "int64" },
          "shardId": { "type": "string" }
        }
      },
      "DeployRequest": {
        "type": "object",
        "properties": {
          "data": { "$ref": "#/components/schemas/DeployData" },
          "deployer": { "type": "string" },
          "signature": { "type": "string" },
          "sigAlgorithm": { "type": "string" }
        }
      },
      "BondInfo": {
        "type": "object",
        "properties": {
          "validator": { "type": "string" },
          "stake": { "type": "integer", "format": "int64" }
        }
      },
      "LightBlockInfo": {
        "type": "object",
        "properties": {
          "version": { "type": "integer", "format": "int32" },
          "shardId": { "type": "string" },
          "blockHash": { "type": "string" },
          "blockNumber": { "type": "integer", "format": "int64" },
          "sender": { "type": "string" },
          "seqNum": { "type": "integer", "format": "int64" },
          "preStateHash": { "type": "string" },
          "postStateHash": { "type": "string" },
          "justifications": { "type": "array", "items": { "type": "string" } },
          "bonds": { "type": "array", "items": { "$ref": "#/components/schemas/BondInfo" } },
          "sigAlgorithm": { "type": "string" },
          "sig": { "type": "string" },
          "blockSize": { "type": "string" },
          "deployCount": { "type": "integer", "format": "int32" },
          "rejectedDeploys": { "type": "array", "items": { "type": "string" } }
        }
      },
      "DeployInfo": {
        "type": "object",
        "properties": {
          "deployer": { "type": "string" },
          "term": { "type": "string" },
          "timestamp": { "type": "integer", "format": "int64" },
          "sig": { "type": "string" },
          "sigAlgorithm": { "type": "string" },
          "phloPrice": { "type": "integer", "format": "int64" },
          "phloLimit": { "type": "integer", "format": "int64" },
          "validAfterBlockNumber": { "type": "integer", "format": "int64" },
          "cost": { "type": "integer", "format": "int64" },
          "errored": { "type": "boolean" },
          "systemDeployError": { "type": "string" }
        }
      },
      "BlockInfo": {
        "type": "object",
        "properties": {
          "blockInfo": { "$ref": "#/components/schemas/LightBlockInfo" },
          "deploys": { "type": "array", "items": { "$ref": "#/components/schemas/DeployInfo" } }
        }
      },
      "ExploreDeployRequest": {
        "type": "object",
        "properties": {
          "term": { "type": "string" },
          "blockHash": { "type": "string" },
          "usePreStateHash": { "type": "boolean" }
        }
      },
      "DataAtNameByBlockHashRequest": {
        "type": "object",
        "properties": {
          "name": { "type": "object", "description": "A rholang expression" },
          "blockHash": { "type": "string" },
          "usePreStateHash": { "type": "boolean" }
        }
      },
      "RhoDataResponse": {
        "type": "object",
        "properties": {
          "expr": { "type": "array", "items": { "type": "object", "description": "A rholang expression" } },
          "block": { "$ref": "#/components/schemas/LightBlockInfo" }
        }
      },
      "ExploratoryDeployResponse": {
        "type": "object",
        "properties": {
          "expr": { "type": "array", "items": { "type": "object", "description": "A rholang expression" } },
          "block": { "$ref": "#/components/schemas/LightBlockInfo" }
        }
      },
      "DeployExecStatus": {
        "oneOf": [
          { "type": "object", "properties": { "deployResult": { "type": "array", "items": { "type": "object" } }, "block": { "$ref": "#/components/schemas/LightBlockInfo" } } },
          { "type": "object", "properties": { "deployError": { "type": "string" }, "block": { "$ref": "#/components/schemas/LightBlockInfo" } } },
          { "type": "object", "properties": { "status": { "type": "string" } } }
        ]
      }
    }
  }
}"##;

async fn api_v1_openapi() -> Response {
    let doc: serde_json::Value = serde_json::from_str(OPENAPI_JSON)
        .unwrap_or_else(|_| serde_json::Value::Null);
    (StatusCode::OK, Json(doc)).into_response()
}

// --- Reporting routes (port of `ReportingRoutes.service`) ---

/// `GET /reporting/trace` query params (`blockHash` + optional `forceReplay`).
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct ReportingQuery {
    block_hash: String,
    force_replay: Option<bool>,
}

async fn reporting_trace(
    State(state): State<HttpState>,
    Query(query): Query<ReportingQuery>,
) -> Response {
    // Reporting is disabled by default (`api-server.enable-reporting = false`); the route answers
    // 404 unless explicitly enabled (M6 — the flag was read but never enforced).
    if !state.enable_reporting {
        return (StatusCode::NOT_FOUND, ()).into_response();
    }
    // Validate-on-ingress: a malformed block hash (non-hex / wrong length) must be a 400, not a
    // panic in `BlockHash::from_hex`.
    let hash = match BlockHash::try_from_hex(&query.block_hash) {
        Ok(h) => h,
        Err(_) => {
            return (
                StatusCode::BAD_REQUEST,
                Json("invalid block hash".to_string()),
            )
                .into_response()
        }
    };
    let result = state
        .block_report_api
        .block_report(&hash, query.force_replay.unwrap_or(false))
        .await;
    (StatusCode::OK, Json(transform_result(result))).into_response()
}

// --- Admin Web API routes (port of `AdminWebApiRoutes.service`) ---

async fn admin_propose(State(state): State<AdminState>) -> Response {
    json_result(state.admin_web_api.propose().await)
}

/// Build the public HTTP routes (port of `acquireHttpServer`'s route map: `/version`, `/metrics`,
/// `/status`, the `/api` JSON routes, `/reporting` + `/api/trace`, the `/api/v1` routes, and the
/// `/api/v1/openapi.json` OpenAPI document).
pub fn router(state: HttpState) -> Router {
    Router::new()
        .route("/version", get(version))
        .route("/metrics", get(metrics))
        .route("/status", get(status))
        .route("/reporting/trace", get(reporting_trace))
        .route("/api/trace", get(reporting_trace))
        .route("/api/status", get(api_status))
        .route("/api/deploy", post(api_deploy))
        .route("/api/explore-deploy", post(api_explore_deploy))
        .route(
            "/api/explore-deploy-by-block-hash",
            post(api_explore_deploy_by_block_hash),
        )
        .route("/api/data-at-name", post(api_data_at_name))
        .route(
            "/api/data-at-name-by-block-hash",
            post(api_data_at_name_by_block_hash),
        )
        .route("/api/last-finalized-block", get(api_last_finalized_block))
        .route("/api/block/:hash", get(api_get_block))
        .route("/api/blocks", get(api_get_blocks))
        .route("/api/blocks/:start/:end", get(api_get_blocks_by_heights))
        .route("/api/blocks/:depth", get(api_get_blocks_by_depth))
        .route("/api/deploy/:deploy_id", get(api_find_deploy))
        .route("/api/is-finalized/:hash", get(api_is_finalized))
        .route("/api/transactions/:hash", get(api_get_transaction))
        .route("/api/v1/status", get(api_status))
        .route("/api/v1/deploy", post(api_deploy))
        .route(
            "/api/v1/deploy-status/:deploy_signature",
            get(api_v1_deploy_status),
        )
        .route("/api/v1/explore-deploy", post(api_explore_deploy))
        .route(
            "/api/v1/explore-deploy-by-block-hash",
            post(api_explore_deploy_by_block_hash),
        )
        .route(
            "/api/v1/data-at-name-by-block-hash",
            post(api_data_at_name_by_block_hash),
        )
        .route("/api/v1/blocks", get(api_get_blocks))
        .route("/api/v1/block/:hash", get(api_get_block))
        .route("/api/v1/openapi.json", get(api_v1_openapi))
        .layer(CorsLayer::permissive())
        .with_state(state)
}

/// Build the admin HTTP routes (port of `acquireAdminHttpServer`'s `/api` + `/api/v1` admin routes).
pub fn admin_router(state: AdminState) -> Router {
    Router::new()
        .route("/api/propose", post(admin_propose))
        .route("/api/v1/propose", get(admin_propose))
        .route("/api/v1/openapi.json", get(api_v1_openapi))
        .layer(CorsLayer::permissive())
        .with_state(state)
}

/// Bind and serve the public HTTP routes (port of `web/acquireHttpServer`), with a CORS layer and a
/// per-request timeout (`api-server.max-connection-idle`).
pub async fn acquire_http_server(
    host: &str,
    port: Port,
    reporter: Arc<NewPrometheusReporter>,
    web_api: Arc<dyn WebApi>,
    block_report_api: Arc<BlockReportApi>,
    status_provider: Option<StatusProvider>,
    max_connection_idle: Duration,
    enable_reporting: bool,
) -> Result<(), String> {
    let port = u16::from(port); // single discharge at the bind boundary
    let addr: SocketAddr = format!("{host}:{port}")
        .parse()
        .map_err(|e| format!("invalid bind address {host}:{port}: {e}"))?;
    let listener = tokio::net::TcpListener::bind(addr)
        .await
        .map_err(|e| e.to_string())?;
    let app = router(HttpState {
        reporter,
        web_api,
        block_report_api,
        status_provider,
        enable_reporting,
        deploy_rate_limiter: Arc::new(RateLimiter::new(DEFAULT_API_RATE_LIMIT_PER_SEC)),
    })
    .layer(TimeoutLayer::new(max_connection_idle));
    axum::serve(listener, app).await.map_err(|e| e.to_string())
}

/// Bind and serve the admin HTTP routes (port of `web/acquireAdminHttpServer`).
pub async fn acquire_admin_http_server(
    host: &str,
    port: Port,
    admin_web_api: Arc<dyn AdminWebApi>,
    max_connection_idle: Duration,
) -> Result<(), String> {
    let port = u16::from(port); // single discharge at the bind boundary
    let addr: SocketAddr = format!("{host}:{port}")
        .parse()
        .map_err(|e| format!("invalid bind address {host}:{port}: {e}"))?;
    let listener = tokio::net::TcpListener::bind(addr)
        .await
        .map_err(|e| e.to_string())?;
    let app = admin_router(AdminState { admin_web_api }).layer(TimeoutLayer::new(max_connection_idle));
    axum::serve(listener, app).await.map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::dto::{
        ApiStatus, DataAtNameResponse, DeployExecStatus, RhoDataResponse, VersionInfo,
    };
    use crate::diagnostics::scrape_data_builder::Configuration;
    use crate::web::transaction::TransactionResponse;
    use async_trait::async_trait;
    use axum::body::to_bytes;
    use rchain_block_storage::dag::codecs::{BlockHashCodec, BlockMessageCodec};
    use rchain_casper::reporting::noop;
    use rchain_comm::peer_node::{NodeIdentifier, PeerNode};
    use rchain_comm::rp::rp_conf::ClearConnectionsConf;
    use rchain_models::casper::protocol::deploy_service::{BlockInfo, LightBlockInfo};
    use rchain_models::casper::protocol::report::BlockEventInfo;
    use rchain_shared::store::InMemoryKeyValueStore;
    use rchain_shared::typed_store::{Codec, KeyValueTypedStoreCodec, SharedStore};
    use std::marker::PhantomData;

    struct JsonCodec<T>(PhantomData<T>);

    struct NoopDiscovery;
    #[async_trait]
    impl NodeDiscovery for NoopDiscovery {
        async fn discover(&self) {}
        fn peers(&self) -> Vec<PeerNode> {
            Vec::new()
        }
    }

    impl<T: Serialize + serde::de::DeserializeOwned + Send + Sync> Codec<T> for JsonCodec<T> {
        fn encode(&self, value: &T) -> Vec<u8> {
            serde_json::to_vec(value).expect("json encode")
        }

        fn decode(&self, bytes: &[u8]) -> Result<T, String> {
            serde_json::from_slice(bytes).map_err(|e| e.to_string())
        }
    }

    fn test_block_report_api() -> Arc<BlockReportApi> {
        let store: SharedStore = Arc::new(tokio::sync::Mutex::new(
            Box::new(InMemoryKeyValueStore::default()),
        ));
        let block_store = Arc::new(KeyValueTypedStoreCodec::new(
            store.clone(),
            Arc::new(BlockHashCodec),
            Arc::new(BlockMessageCodec),
        ));
        let report_store = Arc::new(KeyValueTypedStoreCodec::new(
            store,
            Arc::new(BlockHashCodec),
            Arc::new(JsonCodec::<BlockEventInfo>(PhantomData)),
        ));
        Arc::new(BlockReportApi::new(
            block_store,
            Arc::new(noop()),
            report_store,
            None,
        ))
    }

    fn test_status() -> ApiStatus {
        ApiStatus {
            version: VersionInfo {
                api: "1.0".to_string(),
                node: "2.0".to_string(),
            },
            address: "addr".to_string(),
            network_id: "testnet".to_string(),
            shard_id: "root".to_string(),
            peers: 1,
            nodes: 2,
            min_phlo_price: 3,
            latest_block_number: 4,
        }
    }

    struct MockWebApi {
        status: ApiStatus,
    }

    #[async_trait]
    impl WebApi for MockWebApi {
        async fn status(&self) -> Result<ApiStatus, BlockApiException> {
            Ok(self.status.clone())
        }

        async fn deploy(&self, _: &DeployRequest) -> Result<String, BlockApiException> {
            unimplemented!()
        }

        async fn deploy_status(&self, _: &str) -> Result<DeployExecStatus, BlockApiException> {
            unimplemented!()
        }

        async fn listen_for_data_at_name(
            &self,
            _: &DataAtNameRequest,
        ) -> Result<DataAtNameResponse, BlockApiException> {
            unimplemented!()
        }

        async fn get_data_at_par(
            &self,
            _: &DataAtNameByBlockHashRequest,
        ) -> Result<RhoDataResponse, BlockApiException> {
            unimplemented!()
        }

        async fn last_finalized_block(&self) -> Result<BlockInfo, BlockApiException> {
            unimplemented!()
        }

        async fn get_block(&self, _: &str) -> Result<BlockInfo, BlockApiException> {
            unimplemented!()
        }

        async fn get_blocks(&self, _: i32) -> Result<Vec<LightBlockInfo>, BlockApiException> {
            unimplemented!()
        }

        async fn find_deploy(&self, _: &str) -> Result<LightBlockInfo, BlockApiException> {
            unimplemented!()
        }

        async fn exploratory_deploy(
            &self,
            _: &str,
            _: Option<&str>,
            _: bool,
        ) -> Result<RhoDataResponse, BlockApiException> {
            unimplemented!()
        }

        async fn get_blocks_by_heights(
            &self,
            _: i64,
            _: i64,
        ) -> Result<Vec<LightBlockInfo>, BlockApiException> {
            unimplemented!()
        }

        async fn is_finalized(&self, _: &str) -> Result<bool, BlockApiException> {
            unimplemented!()
        }

        async fn get_transaction(&self, _: &str) -> Result<TransactionResponse, BlockApiException> {
            unimplemented!()
        }
    }

    fn state() -> HttpState {
        HttpState {
            reporter: Arc::new(NewPrometheusReporter::new(Configuration::default())),
            web_api: Arc::new(MockWebApi {
                status: test_status(),
            }),
            block_report_api: test_block_report_api(),
            status_provider: None,
            enable_reporting: true,
            deploy_rate_limiter: Arc::new(RateLimiter::new(DEFAULT_API_RATE_LIMIT_PER_SEC)),
        }
    }

    #[tokio::test]
    async fn version_returns_node_version() {
        assert!(version().await.starts_with("RChain Node "));
    }

    #[tokio::test]
    async fn metrics_returns_scrape_data() {
        let out = metrics(State(state())).await;
        assert_eq!(
            out,
            "# The kamon-prometheus module didn't receive any data just yet.\n"
        );
    }

    #[tokio::test]
    async fn api_status_returns_json() {
        let response = api_status(State(state())).await;
        assert_eq!(response.status(), StatusCode::OK);
        let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(json["address"], "addr");
        assert_eq!(json["version"]["api"], "1.0");
    }

    #[test]
    fn openapi_json_is_valid() {
        let doc: serde_json::Value = serde_json::from_str(OPENAPI_JSON).expect("OPENAPI_JSON parses");
        assert_eq!(doc["openapi"], "3.0.0");
        assert!(doc["paths"].is_object());
        assert!(doc["components"]["schemas"].is_object());
    }

    fn status_provider() -> StatusProvider {
        let local = PeerNode::from(
            NodeIdentifier::new(vec![1]),
            "localhost".to_string(),
            rchain_shared::refined::Port::new(40400),
            rchain_shared::refined::Port::new(40404),
        );
        StatusProvider {
            connections: Arc::new(tokio::sync::RwLock::new(vec![local])),
            rp_conf: RPConf {
                local: PeerNode::from(
                    NodeIdentifier::new(vec![2]),
                    "localhost".to_string(),
                    rchain_shared::refined::Port::new(40400),
                    rchain_shared::refined::Port::new(40404),
                ),
                network_id: "testnet".to_string(),
                bootstrap: None,
                default_timeout: std::time::Duration::from_secs(10),
                max_num_of_connections: 100,
                clear_connections: ClearConnectionsConf {
                    num_of_connections_pinged: 10,
                },
            },
            discovery: Arc::new(NoopDiscovery),
        }
    }

    #[tokio::test]
    async fn status_returns_comm_state() {
        let mut s = state();
        s.status_provider = Some(status_provider());
        let response = status(State(s)).await;
        assert_eq!(response.status(), StatusCode::OK);
        let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(json["peers"], 1);
        assert_eq!(json["nodes"], 0);
    }
}
