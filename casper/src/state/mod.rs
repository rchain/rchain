//! Block state management (port of `casper/state/`).

use std::sync::Arc;

use async_trait::async_trait;

use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_models::casper::protocol::casper_message::BlockMessage;

use crate::blocks::proposer::propose_result::ProposeResult;

/// Empty block-state status (port of `BlockStateStatus`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct BlockStateStatus;

/// Block state manager (port of `BlockStateManager`).
#[async_trait]
pub trait BlockStateManager: Send + Sync {
    async fn is_empty(&self) -> bool;
}

/// RNode state manager (port of `RNodeStateManager`).
#[async_trait]
pub trait RNodeStateManager: Send + Sync {
    async fn is_empty(&self) -> bool;
}

/// The latest + in-progress proposal results (port of `ProposerState`).
pub struct ProposerState {
    pub latest_propose_result: Option<(ProposeResult, Option<BlockMessage>)>,
    pub curr_propose_result:
        Option<tokio::sync::oneshot::Receiver<(ProposeResult, Option<BlockMessage>)>>,
}

impl Default for ProposerState {
    fn default() -> Self {
        ProposerState {
            latest_propose_result: None,
            curr_propose_result: None,
        }
    }
}

/// The concrete block state manager (port of `BlockStateManagerImpl`).
pub struct BlockStateManagerImpl {
    #[allow(dead_code)] // reserved for the remaining StateManager methods
    block_store: BlockStore,
    block_dag_storage: Arc<dyn BlockDagStorage>,
}

impl BlockStateManagerImpl {
    pub fn new(block_store: BlockStore, block_dag_storage: Arc<dyn BlockDagStorage>) -> Self {
        BlockStateManagerImpl {
            block_store,
            block_dag_storage,
        }
    }
}

#[async_trait]
impl BlockStateManager for BlockStateManagerImpl {
    async fn is_empty(&self) -> bool {
        let dag = self.block_dag_storage.get_representation().await;
        dag.topo_sort(0, Some(1)).map(|v| v.is_empty()).unwrap_or(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn block_state_status_is_unit() {
        assert_eq!(BlockStateStatus::default(), BlockStateStatus);
    }

    #[test]
    fn proposer_state_defaults_to_none() {
        let state = ProposerState::default();
        assert!(state.latest_propose_result.is_none());
    }
}
