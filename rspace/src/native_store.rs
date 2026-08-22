//! The native system-contract store: byte-oriented state (registry / PoS / vault) folded into the
//! same content-addressed radix trie as the tuple space.
//!
//! Native state is stored under dedicated trie prefixes (`PREFIX_REGISTRY`/`PREFIX_POS`/
//! `PREFIX_VAULT`) as `NativeLeaf` payloads. The `InMemNativeStore` is a write-through overlay on top
//! of a `NativeHistoryReader`: reads fall through to the persisted trie, writes are buffered in the
//! overlay, and `drain_changes` produces the `NativeStoreAction`s that the history repository folds
//! into the next checkpoint. This mirrors the `HotStore`/`HistoryRepository` split so native state
//! stays content-addressed, replayable, and queryable at an arbitrary state hash.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex, RwLock};

use async_trait::async_trait;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

/// Trie prefix for native registry entries (`uri-bytes -> Par`).
pub const PREFIX_REGISTRY: u8 = 0x03;
/// Trie prefix for native PoS state (`bonds` / `active` / `withdrawers` / `params` leaves).
pub const PREFIX_POS: u8 = 0x04;
/// Trie prefix for native vault state (`rev-address -> balance`).
pub const PREFIX_VAULT: u8 = 0x05;

/// A native-state mutation, folded into the trie at checkpoint (port of a `NativeStoreAction`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NativeStoreAction {
    Put {
        prefix: u8,
        key: Blake2b256Hash,
        value: Vec<u8>,
    },
    Delete {
        prefix: u8,
        key: Blake2b256Hash,
    },
}

/// A keyed read of native state from a history root (implemented by the history reader).
#[async_trait]
pub trait NativeHistoryReader: Send + Sync {
    async fn get_native(&self, prefix: u8, key: Blake2b256Hash) -> Result<Option<Vec<u8>>, String>;
}

/// A no-op native reader (returns `None` for every key) — used as the initial reader before any
/// checkpoint/reset has established a history root.
struct NoopNativeReader;

#[async_trait]
impl NativeHistoryReader for NoopNativeReader {
    async fn get_native(&self, _prefix: u8, _key: Blake2b256Hash) -> Result<Option<Vec<u8>>, String> {
        Ok(None)
    }
}

/// Snapshot of the native-store overlay (for soft-checkpoint revert). `Some(v)` is a written value,
/// `None` is a tombstone (deletion).
#[derive(Clone, Default, Debug)]
pub struct NativeStoreState {
    overlay: BTreeMap<(u8, Blake2b256Hash), Option<Vec<u8>>>,
}

/// The in-memory native store: a write-through overlay over a [`NativeHistoryReader`].
pub struct InMemNativeStore {
    /// Written keys and tombstones since the last `drain_changes` / `reset`. `Some(v)` = value,
    /// `None` = deleted.
    overlay: Mutex<BTreeMap<(u8, Blake2b256Hash), Option<Vec<u8>>>>,
    /// The base reader for keys not present in the overlay (updated on checkpoint/reset).
    reader: RwLock<Arc<dyn NativeHistoryReader>>,
}

impl InMemNativeStore {
    pub fn new(reader: Arc<dyn NativeHistoryReader>) -> Self {
        InMemNativeStore {
            overlay: Mutex::new(BTreeMap::new()),
            reader: RwLock::new(reader),
        }
    }

    /// A store with no backing reader (reads return `None` until a reader is set).
    pub fn empty() -> Self {
        Self::new(Arc::new(NoopNativeReader))
    }

    /// Read a native value, consulting the overlay first and falling through to the persisted trie.
    pub async fn get(&self, prefix: u8, key: &Blake2b256Hash) -> Result<Option<Vec<u8>>, String> {
        {
            let overlay = self.overlay.lock().unwrap_or_else(|p| p.into_inner());
            if let Some(v) = overlay.get(&(prefix, *key)) {
                return Ok(v.clone());
            }
        }
        let reader = self.reader.read().unwrap_or_else(|p| p.into_inner()).clone();
        reader.get_native(prefix, *key).await
    }

    /// Write a native value into the overlay (and record a `Put` action).
    pub fn put(&self, prefix: u8, key: Blake2b256Hash, value: Vec<u8>) {
        self.overlay
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .insert((prefix, key), Some(value));
    }

    /// Delete a native value (record a `Delete` action via a tombstone).
    pub fn delete(&self, prefix: u8, key: &Blake2b256Hash) {
        self.overlay
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .insert((prefix, *key), None);
    }

    /// Drain the pending mutations, clearing the overlay (the caller folds the actions into a
    /// checkpoint).
    pub fn drain_changes(&self) -> Vec<NativeStoreAction> {
        let mut overlay = self.overlay.lock().unwrap_or_else(|p| p.into_inner());
        let actions = overlay
            .iter()
            .map(|(&(prefix, key), value)| match value {
                Some(v) => NativeStoreAction::Put {
                    prefix,
                    key,
                    value: v.clone(),
                },
                None => NativeStoreAction::Delete { prefix, key },
            })
            .collect();
        overlay.clear();
        actions
    }

    /// Capture the current overlay for a soft-checkpoint rollback.
    pub fn snapshot(&self) -> NativeStoreState {
        NativeStoreState {
            overlay: self.overlay.lock().unwrap_or_else(|p| p.into_inner()).clone(),
        }
    }

    /// Restore a previously captured overlay (soft-checkpoint rollback).
    pub fn revert(&self, state: NativeStoreState) {
        *self.overlay.lock().unwrap_or_else(|p| p.into_inner()) = state.overlay;
    }

    /// Point the store at a new history root (called on checkpoint/reset).
    pub fn set_reader(&self, reader: Arc<dyn NativeHistoryReader>) {
        *self.reader.write().unwrap_or_else(|p| p.into_inner()) = reader;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn put_then_get_round_trips_without_reader() {
        let store = InMemNativeStore::empty();
        let key = Blake2b256Hash::from_bytes([7u8; 32]);
        store.put(PREFIX_POS, key, vec![1, 2, 3]);
        assert_eq!(store.get(PREFIX_POS, &key).await.unwrap(), Some(vec![1, 2, 3]));
    }

    #[tokio::test]
    async fn drain_changes_produces_put_actions() {
        let store = InMemNativeStore::empty();
        let key = Blake2b256Hash::from_bytes([8u8; 32]);
        store.put(PREFIX_REGISTRY, key, vec![9]);
        let changes = store.drain_changes();
        assert_eq!(
            changes,
            vec![NativeStoreAction::Put {
                prefix: PREFIX_REGISTRY,
                key,
                value: vec![9],
            }]
        );
        // Overlay is cleared; a read falls through to the (no-op) reader.
        assert_eq!(store.get(PREFIX_REGISTRY, &key).await.unwrap(), None);
    }

    #[tokio::test]
    async fn delete_records_tombstone_and_blocks_reader() {
        let store = InMemNativeStore::empty();
        let key = Blake2b256Hash::from_bytes([9u8; 32]);
        store.put(PREFIX_VAULT, key, vec![1]);
        store.delete(PREFIX_VAULT, &key);
        assert_eq!(store.get(PREFIX_VAULT, &key).await.unwrap(), None);
        let changes = store.drain_changes();
        assert_eq!(changes, vec![NativeStoreAction::Delete { prefix: PREFIX_VAULT, key }]);
    }

    #[tokio::test]
    async fn snapshot_and_revert_restore_overlay() {
        let store = InMemNativeStore::empty();
        let key = Blake2b256Hash::from_bytes([10u8; 32]);
        store.put(PREFIX_POS, key, vec![1]);
        let snap = store.snapshot();
        store.put(PREFIX_POS, key, vec![2]);
        assert_eq!(store.get(PREFIX_POS, &key).await.unwrap(), Some(vec![2]));
        store.revert(snap);
        assert_eq!(store.get(PREFIX_POS, &key).await.unwrap(), Some(vec![1]));
    }
}
