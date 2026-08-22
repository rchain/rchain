//! Node state-manager instances (port of `node/state/instances/RNodeStateManagerImpl.scala`).

use std::sync::Arc;

use async_trait::async_trait;

use rchain_casper::state::{BlockStateManager, RNodeStateManager};
use rchain_shared::state::StateManager;

/// The node state manager: empty iff both the rspace state and the block state are empty (port of
/// `RNodeStateManagerImpl`).
pub struct RNodeStateManagerImpl {
    rspace_state_manager: Arc<dyn StateManager + Send + Sync>,
    block_state_manager: Arc<dyn BlockStateManager>,
}

impl RNodeStateManagerImpl {
    pub fn new(
        rspace_state_manager: Arc<dyn StateManager + Send + Sync>,
        block_state_manager: Arc<dyn BlockStateManager>,
    ) -> Self {
        RNodeStateManagerImpl {
            rspace_state_manager,
            block_state_manager,
        }
    }
}

#[async_trait]
impl RNodeStateManager for RNodeStateManagerImpl {
    async fn is_empty(&self) -> bool {
        self.rspace_state_manager.is_empty() && self.block_state_manager.is_empty().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct EmptyState;
    impl StateManager for EmptyState {
        fn is_empty(&self) -> bool {
            true
        }
    }

    struct NonEmptyState;
    impl StateManager for NonEmptyState {
        fn is_empty(&self) -> bool {
            false
        }
    }

    struct EmptyBlocks;
    #[async_trait]
    impl BlockStateManager for EmptyBlocks {
        async fn is_empty(&self) -> bool {
            true
        }
    }

    struct NonEmptyBlocks;
    #[async_trait]
    impl BlockStateManager for NonEmptyBlocks {
        async fn is_empty(&self) -> bool {
            false
        }
    }

    #[tokio::test]
    async fn is_empty_conjoins_both() {
        let both_empty = RNodeStateManagerImpl::new(
            Arc::new(EmptyState),
            Arc::new(EmptyBlocks),
        );
        assert!(both_empty.is_empty().await);

        let rspace_nonempty = RNodeStateManagerImpl::new(
            Arc::new(NonEmptyState),
            Arc::new(EmptyBlocks),
        );
        assert!(!rspace_nonempty.is_empty().await);

        let blocks_nonempty = RNodeStateManagerImpl::new(
            Arc::new(EmptyState),
            Arc::new(NonEmptyBlocks),
        );
        assert!(!blocks_nonempty.is_empty().await);
    }
}
