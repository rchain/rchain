//! The propose gRPC service (port of `ProposeGrpcServiceV1.scala`).

use std::sync::Arc;

use rchain_casper::api::block_api::BlockApi;
use rchain_models::casper::protocol::deploy_service::ServiceError;
use rchain_models::casper::protocol::propose_service::{ProposeQuery, ProposeResultQuery};

/// The propose service (port of `ProposeGrpcServiceV1`).
pub struct ProposeGrpcServiceV1 {
    block_api: Arc<dyn BlockApi>,
}

impl ProposeGrpcServiceV1 {
    pub fn new(block_api: Arc<dyn BlockApi>) -> Self {
        ProposeGrpcServiceV1 { block_api }
    }

    /// Trigger a proposal; returns immediately when async (port of `propose`).
    pub async fn propose(&self, request: &ProposeQuery) -> Result<String, ServiceError> {
        self.block_api
            .create_block(request.is_async)
            .await
            .map_err(ServiceError::new)
    }

    /// Wait for/read the latest proposal result (port of `proposeResult`).
    pub async fn propose_result(
        &self,
        _request: &ProposeResultQuery,
    ) -> Result<String, ServiceError> {
        self.block_api
            .get_propose_result()
            .await
            .map_err(ServiceError::new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use rchain_casper::api::block_api::{ApiErr, BlockApi};
    use rchain_models::ast::Par;
    use rchain_models::block_metadata::BlockMetadata;
    use rchain_models::casper::protocol::casper_message::SignedDeployData;
    use rchain_models::casper::protocol::deploy_service::{
        BlockInfo, ContinuationsWithBlockInfo, DataWithBlockInfo, DeployExecStatus, LightBlockInfo,
        Status,
    };

    struct StubBlockApi {
        create_result: ApiErr<String>,
        propose_result: ApiErr<String>,
    }

    #[async_trait]
    impl BlockApi for StubBlockApi {
        async fn status(&self) -> Status {
            unimplemented!()
        }
        async fn deploy(&self, _deploy: &SignedDeployData) -> ApiErr<String> {
            unimplemented!()
        }
        async fn deploy_status(&self, _deploy_id: &Vec<u8>) -> ApiErr<DeployExecStatus> {
            unimplemented!()
        }
        async fn create_block(&self, _is_async: bool) -> ApiErr<String> {
            self.create_result.clone()
        }
        async fn get_propose_result(&self) -> ApiErr<String> {
            self.propose_result.clone()
        }
        async fn get_listening_name_data_response(
            &self,
            _depth: i32,
            _listening_name: &Par,
        ) -> ApiErr<(Vec<DataWithBlockInfo>, i32)> {
            unimplemented!()
        }
        async fn get_listening_name_continuation_response(
            &self,
            _depth: i32,
            _listening_names: &[Par],
        ) -> ApiErr<(Vec<ContinuationsWithBlockInfo>, i32)> {
            unimplemented!()
        }
        async fn get_blocks_by_heights(
            &self,
            _start_block_number: i64,
            _end_block_number: i64,
        ) -> ApiErr<Vec<LightBlockInfo>> {
            unimplemented!()
        }
        async fn visualize_dag(
            &self,
            _depth: i32,
            _start_block_number: i32,
            _show_justification_lines: bool,
        ) -> ApiErr<Vec<String>> {
            unimplemented!()
        }
        async fn machine_verifiable_dag(&self, _depth: i32) -> ApiErr<String> {
            unimplemented!()
        }
        async fn get_blocks(&self, _depth: i32) -> ApiErr<Vec<LightBlockInfo>> {
            unimplemented!()
        }
        async fn find_deploy(&self, _id: &Vec<u8>) -> ApiErr<LightBlockInfo> {
            unimplemented!()
        }
        async fn get_block(&self, _hash: &str) -> ApiErr<BlockInfo> {
            unimplemented!()
        }
        async fn bond_status(&self, _public_key: &[u8]) -> ApiErr<bool> {
            unimplemented!()
        }
        async fn exploratory_deploy(
            &self,
            _term: &str,
            _block_hash: Option<&str>,
            _use_pre_state_hash: bool,
        ) -> ApiErr<(Vec<Par>, LightBlockInfo)> {
            unimplemented!()
        }
        async fn get_data_at_par(
            &self,
            _par: &Par,
            _block_hash: &str,
            _use_pre_state_hash: bool,
        ) -> ApiErr<(Vec<Par>, LightBlockInfo)> {
            unimplemented!()
        }
        async fn last_finalized_block(&self) -> ApiErr<BlockInfo> {
            unimplemented!()
        }
        async fn is_finalized(&self, _hash: &str) -> ApiErr<bool> {
            unimplemented!()
        }
        async fn get_latest_message(&self) -> ApiErr<BlockMetadata> {
            unimplemented!()
        }
    }

    #[tokio::test]
    async fn propose_maps_api_errors_to_service_errors() {
        let api = Arc::new(StubBlockApi {
            create_result: Err("read-only node".to_string()),
            propose_result: Ok("Success! Block created.".to_string()),
        });
        let svc = ProposeGrpcServiceV1::new(api);

        let r = svc.propose(&ProposeQuery { is_async: false }).await;
        assert_eq!(r, Err(ServiceError::new("read-only node")));

        let r = svc.propose_result(&ProposeResultQuery).await;
        assert_eq!(r, Ok("Success! Block created.".to_string()));
    }
}
