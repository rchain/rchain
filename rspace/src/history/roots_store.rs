//! Persists the current root hash under fixed keys.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/history/RootsStore.scala`.

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::typed_store::SharedStore;

const CURRENT_ROOT: &[u8] = b"current-root";
const ROOT_TAG: &[u8] = b"root";

/// The roots store (port of `RootsStore`).
pub struct RootsStore {
    store: SharedStore,
}

impl RootsStore {
    pub fn new(store: SharedStore) -> Self {
        RootsStore { store }
    }

    async fn get(&self, key: &[u8]) -> Result<Option<Vec<u8>>, String> {
        let vals = self.store.lock().await.get(&[key.to_vec()])?;
        Ok(vals.into_iter().next().flatten())
    }

    async fn put(&self, key: Vec<u8>, value: Vec<u8>) -> Result<(), String> {
        self.store.lock().await.put(vec![(key, value)])
    }

    /// The current root, if set (port of `currentRoot`).
    pub async fn current_root(&self) -> Result<Option<Blake2b256Hash>, String> {
        Ok(self
            .get(CURRENT_ROOT)
            .await?
            .map(|b| Blake2b256Hash::from_byte_array(&b)))
    }

    /// Set the current root if `key` is a known root (port of `validateAndSetCurrentRoot`).
    pub async fn validate_and_set_current_root(
        &self,
        key: Blake2b256Hash,
    ) -> Result<Option<Blake2b256Hash>, String> {
        let bytes = key.to_byte_array().to_vec();
        if self.get(&bytes).await?.is_some() {
            self.put(CURRENT_ROOT.to_vec(), bytes).await?;
            Ok(Some(key))
        } else {
            Ok(None)
        }
    }

    /// Record `key` as a known root and set it as current (port of `recordRoot`).
    pub async fn record_root(&self, key: Blake2b256Hash) -> Result<(), String> {
        let bytes = key.to_byte_array().to_vec();
        self.put(bytes.clone(), ROOT_TAG.to_vec()).await?;
        self.put(CURRENT_ROOT.to_vec(), bytes).await
    }
}
