//! Web API request/response data types (port of the DTOs in `api/WebApi.scala`).

use std::fmt;

use rchain_models::casper::protocol::casper_message::DeployData;
use rchain_models::casper::protocol::deploy_service::LightBlockInfo;
use serde::{Deserialize, Serialize};

use super::rho_expr::{RhoExpr, RhoUnforg};

/// A deploy request (port of `DeployRequest`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DeployRequest {
    pub data: DeployData,
    pub deployer: String,
    pub signature: String,
    pub sig_algorithm: String,
}

/// An exploratory-deploy request (port of `ExploreDeployRequest`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExploreDeployRequest {
    pub term: String,
    pub block_hash: String,
    pub use_pre_state_hash: bool,
}

/// A data-at-name request (port of `DataAtNameRequest`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DataAtNameRequest {
    pub name: RhoUnforg,
    pub depth: i32,
}

/// A data-at-name-by-block-hash request (port of `DataAtNameByBlockHashRequest`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DataAtNameByBlockHashRequest {
    pub name: RhoExpr,
    pub block_hash: String,
    pub use_pre_state_hash: bool,
}

/// API/node version info (port of `VersionInfo`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VersionInfo {
    pub api: String,
    pub node: String,
}

/// Node status (port of `ApiStatus`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ApiStatus {
    pub version: VersionInfo,
    pub address: String,
    pub network_id: String,
    pub shard_id: String,
    pub peers: i32,
    pub nodes: i32,
    pub min_phlo_price: i64,
    pub latest_block_number: i64,
}

/// Exception thrown by the Block API (port of `BlockApiException`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockApiException(pub String);

impl fmt::Display for BlockApiException {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for BlockApiException {}

/// A deploy-signature error (port of `SignatureException`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SignatureException(pub String);

impl fmt::Display for SignatureException {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for SignatureException {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::rho_expr::{RhoExpr, RhoUnforg};

    #[test]
    fn dto_fields_are_accessible() {
        let deploy = DeployRequest {
            data: DeployData {
                term: "Nil".to_string(),
                timestamp: 0,
                phlo_price: 1,
                phlo_limit: 1,
                valid_after_block_number: 0,
                shard_id: "root".to_string(),
            },
            deployer: "de".to_string(),
            signature: "si".to_string(),
            sig_algorithm: "secp256k1".to_string(),
        };
        assert_eq!(deploy.sig_algorithm, "secp256k1");

        let data_at_name = DataAtNameRequest {
            name: RhoUnforg::UnforgPrivate("ab".to_string()),
            depth: 1,
        };
        assert_eq!(data_at_name.depth, 1);

        let by_hash = DataAtNameByBlockHashRequest {
            name: RhoExpr::ExprString("x".to_string()),
            block_hash: "h".to_string(),
            use_pre_state_hash: false,
        };
        assert!(!by_hash.use_pre_state_hash);

        let status = ApiStatus {
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
        };
        assert_eq!(status.min_phlo_price, 3);
        assert_eq!(status.latest_block_number, 4);
    }

    #[test]
    fn exceptions_carry_messages() {
        let e = BlockApiException("boom".to_string());
        assert_eq!(e.to_string(), "boom");
        let e = SignatureException("bad sig".to_string());
        assert_eq!(e.to_string(), "bad sig");
    }
}

/// A deploy execution status (port of the `DeployExecStatus` ADT in `WebApi.scala`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all_fields = "camelCase")]
pub enum DeployExecStatus {
    ProcessedWithSuccess {
        deploy_result: Vec<RhoExpr>,
        block: LightBlockInfo,
    },
    ProcessedWithError {
        deploy_error: String,
        block: LightBlockInfo,
    },
    NotProcessed {
        status: String,
    },
}

/// A rholang expression plus the block it was found in (port of `RhoExprWithBlock`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RhoExprWithBlock {
    pub expr: RhoExpr,
    pub block: LightBlockInfo,
}

/// A data-at-name response (port of `DataAtNameResponse`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DataAtNameResponse {
    pub exprs: Vec<RhoExprWithBlock>,
    pub length: i32,
}

/// An exploratory-deploy response (port of `ExploratoryDeployResponse`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExploratoryDeployResponse {
    pub expr: Vec<RhoExpr>,
    pub block: LightBlockInfo,
}

/// A rho data response (port of `RhoDataResponse`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RhoDataResponse {
    pub expr: Vec<RhoExpr>,
    pub block: LightBlockInfo,
}
