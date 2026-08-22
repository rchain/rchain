//! Admin web API implementation (port of `AdminWebApi.AdminWebApiImpl`).

use std::sync::Arc;

use async_trait::async_trait;

use rchain_casper::api::block_api::BlockApi;

use super::admin_web_api::AdminWebApi;
use super::dto::BlockApiException;

/// The admin web API implementation (port of `AdminWebApi.AdminWebApiImpl`).
pub struct AdminWebApiImpl {
    block_api: Arc<dyn BlockApi>,
}

impl AdminWebApiImpl {
    pub fn new(block_api: Arc<dyn BlockApi>) -> Self {
        AdminWebApiImpl { block_api }
    }
}

#[async_trait]
impl AdminWebApi for AdminWebApiImpl {
    async fn propose(&self) -> Result<String, BlockApiException> {
        self.block_api
            .create_block(false)
            .await
            .map_err(BlockApiException)
    }

    async fn propose_result(&self) -> Result<String, BlockApiException> {
        self.block_api
            .get_propose_result()
            .await
            .map_err(BlockApiException)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use rchain_casper::api::block_api::ApiErr;
    use rchain_models::ast::Par;
    use rchain_models::block_metadata::BlockMetadata;
    use rchain_models::casper::protocol::casper_message::SignedDeployData;
    use rchain_models::casper::protocol::deploy_service::{
        BlockInfo, ContinuationsWithBlockInfo, DataWithBlockInfo, DeployExecStatus, LightBlockInfo,
        Status,
    };

    struct StubBlockApi;
    #[async_trait]
    impl BlockApi for StubBlockApi {
        async fn status(&self) -> Status {
            unimplemented!()
        }
        async fn deploy(&self, _: &SignedDeployData) -> ApiErr<String> {
            unimplemented!()
        }
        async fn deploy_status(&self, _: &Vec<u8>) -> ApiErr<DeployExecStatus> {
            unimplemented!()
        }
        async fn create_block(&self, _: bool) -> ApiErr<String> {
            Ok("Success! Block created.".to_string())
        }
        async fn get_propose_result(&self) -> ApiErr<String> {
            Ok("Success! Block created.".to_string())
        }
        async fn get_listening_name_data_response(
            &self,
            _: i32,
            _: &Par,
        ) -> ApiErr<(Vec<DataWithBlockInfo>, i32)> {
            unimplemented!()
        }
        async fn get_listening_name_continuation_response(
            &self,
            _: i32,
            _: &[Par],
        ) -> ApiErr<(Vec<ContinuationsWithBlockInfo>, i32)> {
            unimplemented!()
        }
        async fn get_blocks_by_heights(&self, _: i64, _: i64) -> ApiErr<Vec<LightBlockInfo>> {
            unimplemented!()
        }
        async fn visualize_dag(&self, _: i32, _: i32, _: bool) -> ApiErr<Vec<String>> {
            unimplemented!()
        }
        async fn machine_verifiable_dag(&self, _: i32) -> ApiErr<String> {
            unimplemented!()
        }
        async fn get_blocks(&self, _: i32) -> ApiErr<Vec<LightBlockInfo>> {
            unimplemented!()
        }
        async fn find_deploy(&self, _: &Vec<u8>) -> ApiErr<LightBlockInfo> {
            unimplemented!()
        }
        async fn get_block(&self, _: &str) -> ApiErr<BlockInfo> {
            unimplemented!()
        }
        async fn bond_status(&self, _: &[u8]) -> ApiErr<bool> {
            unimplemented!()
        }
        async fn exploratory_deploy(
            &self,
            _: &str,
            _: Option<&str>,
            _: bool,
        ) -> ApiErr<(Vec<Par>, LightBlockInfo)> {
            unimplemented!()
        }
        async fn get_data_at_par(
            &self,
            _: &Par,
            _: &str,
            _: bool,
        ) -> ApiErr<(Vec<Par>, LightBlockInfo)> {
            unimplemented!()
        }
        async fn last_finalized_block(&self) -> ApiErr<BlockInfo> {
            unimplemented!()
        }
        async fn is_finalized(&self, _: &str) -> ApiErr<bool> {
            unimplemented!()
        }
        async fn get_latest_message(&self) -> ApiErr<BlockMetadata> {
            unimplemented!()
        }
    }

    #[tokio::test]
    async fn propose_and_result_delegate_to_block_api() {
        let api = AdminWebApiImpl::new(Arc::new(StubBlockApi));
        assert_eq!(api.propose().await.unwrap(), "Success! Block created.");
        assert_eq!(api.propose_result().await.unwrap(), "Success! Block created.");
    }
}
