//! Higher-level root commit/validation wrapper.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/history/RootRepository.scala`.

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::history::history::empty_root_hash_value;
use crate::history::roots_store::RootsStore;

/// The root repository (port of `RootRepository`).
pub struct RootRepository {
    roots_store: RootsStore,
}

impl RootRepository {
    pub fn new(roots_store: RootsStore) -> Self {
        RootRepository { roots_store }
    }

    pub async fn commit(&self, root: Blake2b256Hash) -> Result<(), String> {
        self.roots_store.record_root(root).await
    }

    /// The current root, recording the empty root on first use (port of `currentRoot`).
    pub async fn current_root(&self) -> Result<Blake2b256Hash, String> {
        match self.roots_store.current_root().await? {
            None => {
                let empty = empty_root_hash_value();
                self.roots_store.record_root(empty).await?;
                Ok(empty)
            }
            Some(root) => Ok(root),
        }
    }

    /// Validate `root` is known and set it current; error otherwise (port of
    /// `validateAndSetCurrentRoot`).
    pub async fn validate_and_set_current_root(&self, root: Blake2b256Hash) -> Result<(), String> {
        match self.roots_store.validate_and_set_current_root(root).await? {
            Some(_) => Ok(()),
            None => Err("unknown root".to_string()),
        }
    }
}
