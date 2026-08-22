//! Key-value store manager.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/store/{KeyValueStoreManager,InMemoryStoreManager,
//! KeyValueStoreManagerSyntax}.scala`. The Scala `TrieMap[String, InMemoryKeyValueStore[F]]` becomes
//! a `tokio::sync::Mutex<BTreeMap<..>>` (deterministic iteration, matching the crate's BTreeMap
//! convention), and `F[_]` becomes `async_trait`.

use std::collections::BTreeMap;
use std::sync::Arc;

use async_trait::async_trait;

use crate::store::{InMemoryKeyValueStore, KeyValueStore};
use crate::typed_store::{Codec, KeyValueTypedStoreCodec, SharedStore};

/// A database identifier (port of `LmdbDirStoreManager.Db`).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Db {
    pub id: String,
    pub name_override: Option<String>,
}

impl Db {
    pub fn new(id: impl Into<String>) -> Self {
        Db {
            id: id.into(),
            name_override: None,
        }
    }
}

/// Mega, giga and tera bytes (port of `LmdbDirStoreManager.mb/gb/tb`).
pub const MB: i64 = 1024 * 1024;
pub const GB: i64 = 1024 * MB;
pub const TB: i64 = 1024 * GB;

/// An LMDB environment config (port of `LmdbDirStoreManager.LmdbEnvConfig`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LmdbEnvConfig {
    pub name: String,
    pub max_env_size: i64,
}

impl LmdbEnvConfig {
    pub fn new(name: impl Into<String>, max_env_size: i64) -> Self {
        LmdbEnvConfig {
            name: name.into(),
            max_env_size,
        }
    }
}

/// A key-value store manager (port of `KeyValueStoreManager[F]`).
#[async_trait]
pub trait KeyValueStoreManager: Send + Sync {
    /// Get (creating if necessary) the named byte store.
    async fn store(&self, name: &str) -> Result<SharedStore, String>;
    async fn shutdown(&self);
}

/// In-memory store manager (port of `InMemoryStoreManager[F]`).
#[derive(Default)]
pub struct InMemoryStoreManager {
    state: tokio::sync::Mutex<BTreeMap<String, SharedStore>>,
}

#[async_trait]
impl KeyValueStoreManager for InMemoryStoreManager {
    async fn store(&self, name: &str) -> Result<SharedStore, String> {
        let mut state = self.state.lock().await;
        Ok(state
            .entry(name.to_string())
            .or_insert_with(|| {
                Arc::new(tokio::sync::Mutex::new(Box::new(
                    InMemoryKeyValueStore::default(),
                ) as Box<dyn KeyValueStore + Send + Sync>))
            })
            .clone())
    }

    async fn shutdown(&self) {}
}

/// Open a typed store from a manager (port of `KeyValueStoreManagerSyntax.database`).
pub async fn database<K, V>(
    manager: &dyn KeyValueStoreManager,
    name: &str,
    k_codec: Arc<dyn Codec<K>>,
    v_codec: Arc<dyn Codec<V>>,
) -> Result<KeyValueTypedStoreCodec<K, V>, String> {
    let store = manager.store(name).await?;
    Ok(KeyValueTypedStoreCodec::new(store, k_codec, v_codec))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typed_store::{KeyValueTypedStore, StringCodec};

    #[tokio::test]
    async fn store_returns_same_named_store() {
        let manager = InMemoryStoreManager::default();
        let a = manager.store("db").await.unwrap();
        let b = manager.store("db").await.unwrap();
        assert!(Arc::ptr_eq(&a, &b));
        let other = manager.store("other").await.unwrap();
        assert!(!Arc::ptr_eq(&a, &other));
    }

    #[tokio::test]
    async fn database_round_trips() {
        let manager = InMemoryStoreManager::default();
        let db = database(
            &manager,
            "strings",
            Arc::new(StringCodec),
            Arc::new(StringCodec),
        )
        .await
        .unwrap();
        db.put(&[("k".to_string(), "v".to_string())]).await.unwrap();
        assert_eq!(db.get(&["k".to_string()]).await.unwrap(), vec![Some("v".to_string())]);
    }
}
