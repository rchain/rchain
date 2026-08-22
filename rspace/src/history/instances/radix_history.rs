//! The `History` implementation over `RadixTreeImpl`.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/history/instances/RadixHistory.scala`.

use std::collections::HashSet;
use std::sync::Arc;

use async_trait::async_trait;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::typed_store::KeyValueTypedStore;

use crate::history::history::History;
use crate::history::history_action::HistoryAction;
use crate::history::key_segment::KeySegment;
use crate::history::radix_tree::{empty_root_hash, RadixTreeImpl, Node};

/// The radix-tree `History` (port of `RadixHistory`).
pub struct RadixHistory {
    root_hash: Blake2b256Hash,
    root_node: Node,
    impl_: Arc<RadixTreeImpl>,
    store: Arc<dyn KeyValueTypedStore<Blake2b256Hash, Vec<u8>>>,
}

impl RadixHistory {
    pub async fn new(
        root: Blake2b256Hash,
        store: Arc<dyn KeyValueTypedStore<Blake2b256Hash, Vec<u8>>>,
    ) -> Arc<dyn History> {
        let impl_ = Arc::new(RadixTreeImpl::new(store.clone()));
        let root_node = impl_.load_node(root, true).await;
        Arc::new(RadixHistory {
            root_hash: root,
            root_node,
            impl_,
            store,
        })
    }

    fn copy(&self, root_hash: Blake2b256Hash, root_node: Node, impl_: Arc<RadixTreeImpl>) -> Self {
        RadixHistory {
            root_hash,
            root_node,
            impl_,
            store: self.store.clone(),
        }
    }
}

fn has_no_duplicates(actions: &[HistoryAction]) -> bool {
    let keys: Vec<&KeySegment> = actions.iter().map(|a| a.key()).collect();
    let set: HashSet<&KeySegment> = keys.iter().copied().collect();
    set.len() == keys.len()
}

#[async_trait]
impl History for RadixHistory {
    fn root(&self) -> Blake2b256Hash {
        self.root_hash
    }

    async fn read(&self, key: &KeySegment) -> Option<Blake2b256Hash> {
        self.impl_.read(&self.root_node, key).await
    }

    async fn process(&self, actions: &[HistoryAction]) -> Result<Arc<dyn History>, String> {
        assert!(
            has_no_duplicates(actions),
            "Cannot process duplicate actions on one key."
        );
        let result = self.impl_.save_and_commit(&self.root_node, actions).await?;
        self.impl_.clear_read_cache();
        match result {
            Some((new_root_node, new_root_hash)) => Ok(Arc::new(
                self.copy(new_root_hash, new_root_node, self.impl_.clone()),
            )),
            None => Ok(Arc::new(self.copy(
                self.root_hash,
                self.root_node.clone(),
                self.impl_.clone(),
            ))),
        }
    }

    async fn reset(&self, root: Blake2b256Hash) -> Arc<dyn History> {
        let impl_ = Arc::new(RadixTreeImpl::new(self.store.clone()));
        let root_node = impl_.load_node(root, true).await;
        Arc::new(self.copy(root, root_node, impl_))
    }
}

/// The empty root hash (port of `RadixHistory.emptyRootHash`).
pub fn empty_root() -> Blake2b256Hash {
    empty_root_hash()
}
