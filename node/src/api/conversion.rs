//! Web API protobuf conversion functions (port of the conversion fns in `api/WebApi.scala`).

use rchain_crypto::public_key::PublicKey;
use rchain_crypto::signatures::signed::Signed;
use rchain_crypto::signatures::signatures_alg::from_algorithm;
use rchain_models::ast::Par;
use rchain_models::casper::protocol::casper_message::DeployData;
use rchain_models::casper::protocol::deploy_service::{
    DataWithBlockInfo, DeployExecStatus as CasperDeployExecStatus, LightBlockInfo, Status,
};
use rchain_shared::base16;

use super::dto::{
    ApiStatus, DataAtNameResponse, DeployExecStatus as ApiDeployExecStatus, DeployRequest,
    RhoDataResponse, RhoExprWithBlock, SignatureException, VersionInfo,
};
use super::rho_expr::{expr_from_par, RhoExpr};

/// Map a casper `Status` to an `ApiStatus` (port of `toApiStatus`).
pub fn to_api_status(status: &Status) -> ApiStatus {
    ApiStatus {
        version: VersionInfo {
            api: status.version.api.clone(),
            node: status.version.node.clone(),
        },
        address: status.address.clone(),
        network_id: status.network_id.clone(),
        shard_id: status.shard_id.clone(),
        peers: status.peers,
        nodes: status.nodes,
        min_phlo_price: status.min_phlo_price,
        latest_block_number: status.latest_block_number,
    }
}

/// Map a casper `DeployExecStatus` to the API `DeployExecStatus` (port of `toDeployExecStatus`).
pub fn to_deploy_exec_status(status: &CasperDeployExecStatus) -> Option<ApiDeployExecStatus> {
    match status {
        CasperDeployExecStatus::ProcessedWithSuccess {
            deploy_result,
            block,
        } => {
            let result: Vec<RhoExpr> = deploy_result.iter().filter_map(expr_from_par).collect();
            Some(ApiDeployExecStatus::ProcessedWithSuccess {
                deploy_result: result,
                block: block.clone(),
            })
        }
        CasperDeployExecStatus::ProcessedWithError {
            deploy_error,
            block,
        } => Some(ApiDeployExecStatus::ProcessedWithError {
            deploy_error: deploy_error.clone(),
            block: block.clone(),
        }),
        CasperDeployExecStatus::NotProcessed { status } => {
            Some(ApiDeployExecStatus::NotProcessed {
                status: status.clone(),
            })
        }
    }
}

/// Map post-block `Par`s plus a block to a `RhoDataResponse` (port of `toRhoDataResponse`).
pub fn to_rho_data_response(pars: &[Par], block: &LightBlockInfo) -> RhoDataResponse {
    RhoDataResponse {
        expr: pars.iter().filter_map(expr_from_par).collect(),
        block: block.clone(),
    }
}

/// Map post-block data plus a length to a `DataAtNameResponse` (port of `toDataAtNameResponse`).
pub fn to_data_at_name_response(dbs: &[DataWithBlockInfo], length: i32) -> DataAtNameResponse {
    let mut exprs_with_block = Vec::new();
    for data in dbs {
        let exprs: Vec<RhoExpr> = data
            .post_block_data
            .iter()
            .filter_map(expr_from_par)
            .collect();
        // Implements the semantic of Par with Unit: P | Nil ==> P.
        let expr = if let [single] = exprs.as_slice() {
            single.clone()
        } else {
            RhoExpr::ExprPar(exprs)
        };
        exprs_with_block.insert(
            0,
            RhoExprWithBlock {
                expr,
                block: data.block.clone(),
            },
        );
    }
    DataAtNameResponse {
        exprs: exprs_with_block,
        length,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_models::ast::Expr;
    use rchain_models::casper::protocol::deploy_service::{
        BondInfo, Status, VersionInfo as CasperVersionInfo,
    };

    fn light_block_info() -> LightBlockInfo {
        LightBlockInfo {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: "h".to_string(),
            block_number: 1,
            sender: "s".to_string(),
            seq_num: 1,
            pre_state_hash: "pre".to_string(),
            post_state_hash: "post".to_string(),
            justifications: vec![],
            bonds: vec![BondInfo {
                validator: "v".to_string(),
                stake: 100,
            }],
            sig_algorithm: "secp256k1".to_string(),
            sig: "sig".to_string(),
            block_size: "0".to_string(),
            deploy_count: 0,
            rejected_deploys: vec![],
        }
    }

    fn par_int(n: i64) -> Par {
        Par {
            exprs: vec![Expr::GInt(n)],
            ..Default::default()
        }
    }

    #[test]
    fn to_api_status_maps_fields() {
        let status = Status {
            version: CasperVersionInfo {
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
        let api = to_api_status(&status);
        assert_eq!(api.version.api, "1.0");
        assert_eq!(api.address, "addr");
        assert_eq!(api.min_phlo_price, 3);
    }

    #[test]
    fn to_deploy_exec_status_maps_oneof() {
        let not_processed = to_deploy_exec_status(&CasperDeployExecStatus::NotProcessed {
            status: "pending".to_string(),
        })
        .unwrap();
        assert_eq!(
            not_processed,
            ApiDeployExecStatus::NotProcessed {
                status: "pending".to_string()
            }
        );

        let err = to_deploy_exec_status(&CasperDeployExecStatus::ProcessedWithError {
            deploy_error: "boom".to_string(),
            block: light_block_info(),
        })
        .unwrap();
        assert!(matches!(err, ApiDeployExecStatus::ProcessedWithError { .. }));
    }

    #[test]
    fn to_rho_data_response_maps_pars() {
        let resp = to_rho_data_response(&[par_int(42)], &light_block_info());
        assert_eq!(resp.expr, vec![RhoExpr::ExprInt(42)]);
        assert_eq!(resp.block.block_hash, "h");
    }

    #[test]
    fn to_data_at_name_response_maps_data() {
        let db = DataWithBlockInfo {
            post_block_data: vec![par_int(1)],
            block: light_block_info(),
        };
        let resp = to_data_at_name_response(&[db], 5);
        assert_eq!(resp.length, 5);
        assert_eq!(resp.exprs.len(), 1);
        assert_eq!(resp.exprs[0].expr, RhoExpr::ExprInt(1));
    }
}

/// Build a signed deploy from a deploy request (port of `toSignedDeploy`).
pub fn to_signed_deploy(sd: &DeployRequest) -> Result<Signed<DeployData>, SignatureException> {
    let pk_bytes = base16::decode(&sd.deployer).ok_or_else(|| {
        SignatureException("Public key is not valid base16 format.".to_string())
    })?;
    let sig_bytes = base16::decode(&sd.signature).ok_or_else(|| {
        SignatureException("Signature is not valid base16 format.".to_string())
    })?;
    let pk = PublicKey::new(pk_bytes);
    let sig_alg = from_algorithm(&sd.sig_algorithm)
        .ok_or_else(|| SignatureException("Signature algorithm not supported.".to_string()))?;
    Signed::from_signed_data(sd.data.clone(), pk, sig_bytes, sig_alg)
        .ok_or_else(|| SignatureException("Invalid signature.".to_string()))
}

#[cfg(test)]
mod to_signed_deploy_tests {
    use super::*;

    fn deploy_request(deployer: &str, sig_algorithm: &str) -> DeployRequest {
        DeployRequest {
            data: DeployData {
                term: "Nil".to_string(),
                timestamp: 0,
                phlo_price: 1,
                phlo_limit: 1,
                valid_after_block_number: 0,
                shard_id: "root".to_string(),
            },
            deployer: deployer.to_string(),
            signature: "00".to_string(),
            sig_algorithm: sig_algorithm.to_string(),
        }
    }

    #[test]
    fn rejects_invalid_deployer_hex() {
        let err = to_signed_deploy(&deploy_request("zz", "secp256k1")).err().unwrap();
        assert_eq!(err.to_string(), "Public key is not valid base16 format.");
    }

    #[test]
    fn rejects_unsupported_algorithm() {
        let err = to_signed_deploy(&deploy_request("00", "unknown")).err().unwrap();
        assert_eq!(err.to_string(), "Signature algorithm not supported.");
    }
}
