//! The deploy gRPC service (port of `DeployGrpcServiceV1.scala`).
//!
//! Streaming responses (`visualizeDag`/`getBlocks`/`getBlocksByHeights`) collapse to `Vec<_>`
//! (the monix `Observable` layer is transport).

use std::sync::Arc;

use rchain_casper::api::block_api::BlockApi;
use rchain_casper::api::block_report_api::BlockReportApi;
use rchain_models::ast::Par;
use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::SignedDeployData;
use rchain_models::casper::protocol::deploy_service::{
    BlockInfo, BlockQuery, BlocksQuery, BlocksQueryByHeight, BondStatusQuery,
    ContinuationAtNameQuery, ContinuationsWithBlockInfo, DataAtNameByBlockQuery, DataAtNameQuery,
    DataWithBlockInfo, DeployExecStatus, ExploratoryDeployQuery, FindDeployQuery, IsFinalizedQuery,
    LightBlockInfo, MachineVerifyQuery, ReportQuery, ServiceError, Status, VisualizeDagQuery,
};
use rchain_models::casper::protocol::report::BlockEventInfo;
use rchain_shared::base16;

/// The deploy service (port of `DeployGrpcServiceV1`).
pub struct DeployGrpcServiceV1 {
    block_api: Arc<dyn BlockApi>,
    block_report_api: Arc<BlockReportApi>,
}

impl DeployGrpcServiceV1 {
    pub fn new(block_api: Arc<dyn BlockApi>, block_report_api: Arc<BlockReportApi>) -> Self {
        DeployGrpcServiceV1 {
            block_api,
            block_report_api,
        }
    }

    /// Queue a deploy (port of `doDeploy`).
    pub async fn do_deploy(&self, deploy: &SignedDeployData) -> Result<String, ServiceError> {
        self.block_api.deploy(deploy).await.map_err(ServiceError::new)
    }

    /// Get a deploy's execution status by signature (port of `deployStatus`).
    pub async fn deploy_status(
        &self,
        request: &FindDeployQuery,
    ) -> Result<DeployExecStatus, ServiceError> {
        self.block_api
            .deploy_status(&request.deploy_id)
            .await
            .map_err(ServiceError::new)
    }

    /// Get a block by hash (port of `getBlock`).
    pub async fn get_block(&self, request: &BlockQuery) -> Result<BlockInfo, ServiceError> {
        self.block_api
            .get_block(&request.hash)
            .await
            .map_err(ServiceError::new)
    }

    /// Render the DAG as Graphviz (port of `visualizeDag`).
    pub async fn visualize_dag(
        &self,
        request: &VisualizeDagQuery,
    ) -> Result<Vec<String>, ServiceError> {
        self.block_api
            .visualize_dag(
                request.depth,
                request.start_block_number,
                request.show_justification_lines,
            )
            .await
            .map_err(ServiceError::new)
    }

    /// Emit machine-verifiable DAG edges (port of `machineVerifiableDag`).
    pub async fn machine_verifiable_dag(
        &self,
        request: &MachineVerifyQuery,
    ) -> Result<String, ServiceError> {
        self.block_api
            .machine_verifiable_dag(request.depth)
            .await
            .map_err(ServiceError::new)
    }

    /// List latest blocks (port of `getBlocks`).
    pub async fn get_blocks(
        &self,
        request: &BlocksQuery,
    ) -> Result<Vec<LightBlockInfo>, ServiceError> {
        self.block_api
            .get_blocks(request.depth)
            .await
            .map_err(ServiceError::new)
    }

    /// Find data sent to a name (port of `listenForDataAtName`).
    pub async fn listen_for_data_at_name(
        &self,
        request: &DataAtNameQuery,
    ) -> Result<(Vec<DataWithBlockInfo>, i32), ServiceError> {
        self.block_api
            .get_listening_name_data_response(request.depth, &request.name)
            .await
            .map_err(ServiceError::new)
    }

    /// Find data sent to a name at a block (port of `getDataAtName`).
    pub async fn get_data_at_name(
        &self,
        request: &DataAtNameByBlockQuery,
    ) -> Result<(Vec<Par>, LightBlockInfo), ServiceError> {
        self.block_api
            .get_data_at_par(&request.par, &request.block_hash, request.use_pre_state_hash)
            .await
            .map_err(ServiceError::new)
    }

    /// Find continuations listening on names (port of `listenForContinuationAtName`).
    pub async fn listen_for_continuation_at_name(
        &self,
        request: &ContinuationAtNameQuery,
    ) -> Result<(Vec<ContinuationsWithBlockInfo>, i32), ServiceError> {
        self.block_api
            .get_listening_name_continuation_response(request.depth, &request.names)
            .await
            .map_err(ServiceError::new)
    }

    /// Find the block containing a deploy (port of `findDeploy`).
    pub async fn find_deploy(
        &self,
        request: &FindDeployQuery,
    ) -> Result<LightBlockInfo, ServiceError> {
        self.block_api
            .find_deploy(&request.deploy_id)
            .await
            .map_err(ServiceError::new)
    }

    /// Get the last finalized block (port of `lastFinalizedBlock`).
    pub async fn last_finalized_block(&self) -> Result<BlockInfo, ServiceError> {
        self.block_api
            .last_finalized_block()
            .await
            .map_err(ServiceError::new)
    }

    /// Check finality of a block (port of `isFinalized`).
    pub async fn is_finalized(&self, request: &IsFinalizedQuery) -> Result<bool, ServiceError> {
        self.block_api
            .is_finalized(&request.hash)
            .await
            .map_err(ServiceError::new)
    }

    /// Check if a validator is bonded (port of `bondStatus`).
    pub async fn bond_status(&self, request: &BondStatusQuery) -> Result<bool, ServiceError> {
        self.block_api
            .bond_status(&request.public_key)
            .await
            .map_err(ServiceError::new)
    }

    /// Run a read-only deploy with immediate rollback (port of `exploratoryDeploy`).
    pub async fn exploratory_deploy(
        &self,
        request: &ExploratoryDeployQuery,
    ) -> Result<(Vec<Par>, LightBlockInfo), ServiceError> {
        let block_hash = if request.block_hash.is_empty() {
            None
        } else {
            Some(request.block_hash.as_str())
        };
        self.block_api
            .exploratory_deploy(&request.term, block_hash, request.use_pre_state_hash)
            .await
            .map_err(ServiceError::new)
    }

    /// Get a block's report events (port of `getEventByHash`).
    pub async fn get_event_by_hash(
        &self,
        request: &ReportQuery,
    ) -> Result<BlockEventInfo, ServiceError> {
        let bytes = base16::decode(&request.hash).ok_or_else(|| {
            ServiceError::new(format!(
                "Request hash: {} is not valid hex string",
                request.hash
            ))
        })?;
        let hash = BlockHash::try_from(bytes.as_slice())
            .map_err(|e| ServiceError::new(e.to_string()))?;
        self.block_report_api
            .block_report(&hash, request.force_replay)
            .await
            .map_err(ServiceError::new)
    }

    /// List blocks in a height range (port of `getBlocksByHeights`).
    pub async fn get_blocks_by_heights(
        &self,
        request: &BlocksQueryByHeight,
    ) -> Result<Vec<LightBlockInfo>, ServiceError> {
        self.block_api
            .get_blocks_by_heights(request.start_block_number, request.end_block_number)
            .await
            .map_err(ServiceError::new)
    }

    /// Get the node status (port of `status`).
    pub async fn status(&self) -> Status {
        self.block_api.status().await
    }
}
