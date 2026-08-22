//! Web API interface (port of the `WebApi[F]` trait in `api/WebApi.scala`).

use async_trait::async_trait;

use rchain_models::casper::protocol::deploy_service::{BlockInfo, LightBlockInfo};

use super::dto::{
    ApiStatus, BlockApiException, DataAtNameByBlockHashRequest, DataAtNameRequest,
    DataAtNameResponse, DeployExecStatus, DeployRequest, RhoDataResponse,
};
use crate::web::transaction::TransactionResponse;

/// The web API contract (port of `WebApi[F]`; the `F[_]` effect becomes `async` + `Result`).
#[async_trait]
pub trait WebApi: Send + Sync {
    async fn status(&self) -> Result<ApiStatus, BlockApiException>;

    async fn deploy(&self, request: &DeployRequest) -> Result<String, BlockApiException>;

    async fn deploy_status(&self, deploy_id: &str) -> Result<DeployExecStatus, BlockApiException>;

    async fn listen_for_data_at_name(
        &self,
        request: &DataAtNameRequest,
    ) -> Result<DataAtNameResponse, BlockApiException>;

    async fn get_data_at_par(
        &self,
        request: &DataAtNameByBlockHashRequest,
    ) -> Result<RhoDataResponse, BlockApiException>;

    async fn last_finalized_block(&self) -> Result<BlockInfo, BlockApiException>;

    async fn get_block(&self, hash: &str) -> Result<BlockInfo, BlockApiException>;

    async fn get_blocks(&self, depth: i32) -> Result<Vec<LightBlockInfo>, BlockApiException>;

    async fn find_deploy(&self, deploy_id: &str) -> Result<LightBlockInfo, BlockApiException>;

    async fn exploratory_deploy(
        &self,
        term: &str,
        block_hash: Option<&str>,
        use_pre_state_hash: bool,
    ) -> Result<RhoDataResponse, BlockApiException>;

    async fn get_blocks_by_heights(
        &self,
        start_block_number: i64,
        end_block_number: i64,
    ) -> Result<Vec<LightBlockInfo>, BlockApiException>;

    async fn is_finalized(&self, hash: &str) -> Result<bool, BlockApiException>;

    async fn get_transaction(&self, hash: &str) -> Result<TransactionResponse, BlockApiException>;
}
