//! The abstract Merkle history (key-addressable hash reads + batched mutation).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/history/History.scala`.

use std::sync::Arc;

use async_trait::async_trait;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::history::history_action::HistoryAction;
use crate::history::key_segment::KeySegment;
use crate::history::radix_tree::empty_root_hash;

/// The radix-history interface (port of `History[F]`).
#[async_trait]
pub trait History: Send + Sync {
    /// Read the value stored at `key` (port of `read`).
    async fn read(&self, key: &KeySegment) -> Option<Blake2b256Hash>;

    /// Apply a batch of insert/update/delete actions (port of `process`).
    async fn process(&self, actions: &[HistoryAction]) -> Result<Arc<dyn History>, String>;

    /// The current root hash (port of `root`).
    fn root(&self) -> Blake2b256Hash;

    /// Return a `History` rooted at `root` (port of `reset`).
    async fn reset(&self, root: Blake2b256Hash) -> Arc<dyn History>;
}

/// The hash of the empty history root (port of `History.emptyRootHash`).
pub fn empty_root_hash_value() -> Blake2b256Hash {
    empty_root_hash()
}
