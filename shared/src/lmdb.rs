//! LMDB-backed key-value store (port of `store/LmdbKeyValueStore.scala` +
//! `store/LmdbStoreManager.scala`).
//!
//! The `KeyValueStore` trait is synchronous, so LMDB transactions run inline; the surrounding
//! `tokio::sync::Mutex` in `SharedStore` serializes access to a store. `LmdbStoreManager` opens a
//! single LMDB environment (file) whose named databases are the `KeyValueStore`s.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use async_trait::async_trait;
use lmdb::{Cursor, Database, DatabaseFlags, Environment, Error as LmdbError, Transaction, WriteFlags};

use crate::store::KeyValueStore;
use crate::store_manager::{Db, KeyValueStoreManager, LmdbEnvConfig};
use crate::typed_store::SharedStore;

/// An LMDB-backed key-value store (port of `LmdbKeyValueStore`).
pub struct LmdbKeyValueStore {
    env: Arc<Environment>,
    db: Database,
}

impl LmdbKeyValueStore {
    pub fn new(env: Arc<Environment>, db: Database) -> Self {
        LmdbKeyValueStore { env, db }
    }
}

impl KeyValueStore for LmdbKeyValueStore {
    fn get(&self, keys: &[Vec<u8>]) -> Result<Vec<Option<Vec<u8>>>, String> {
        let txn = self
            .env
            .begin_ro_txn()
            .map_err(|e| format!("LMDB read transaction: {e}"))?;
        let mut result = Vec::with_capacity(keys.len());
        for k in keys {
            match txn.get(self.db, k) {
                Ok(v) => result.push(Some(v.to_vec())),
                Err(LmdbError::NotFound) => result.push(None),
                Err(e) => return Err(format!("LMDB get failed: {e}")),
            }
        }
        txn.commit().map_err(|e| format!("LMDB commit: {e}"))?;
        Ok(result)
    }

    fn put(&mut self, pairs: Vec<(Vec<u8>, Vec<u8>)>) -> Result<(), String> {
        let mut txn = self
            .env
            .begin_rw_txn()
            .map_err(|e| format!("LMDB write transaction: {e}"))?;
        for (k, v) in &pairs {
            txn.put(self.db, k, v, WriteFlags::empty())
                .map_err(|e| format!("LMDB put: {e}"))?;
        }
        txn.commit().map_err(|e| format!("LMDB commit: {e}"))?;
        Ok(())
    }

    fn delete(&mut self, keys: &[Vec<u8>]) -> Result<usize, String> {
        let mut txn = self
            .env
            .begin_rw_txn()
            .map_err(|e| format!("LMDB write transaction: {e}"))?;
        let mut removed = 0;
        for k in keys {
            match txn.del(self.db, k, None) {
                Ok(()) => removed += 1,
                Err(LmdbError::NotFound) => {}
                Err(e) => return Err(format!("LMDB delete failed: {e}")),
            }
        }
        txn.commit().map_err(|e| format!("LMDB commit: {e}"))?;
        Ok(removed)
    }

    fn entries(&self) -> Result<Vec<(Vec<u8>, Vec<u8>)>, String> {
        let txn = self
            .env
            .begin_ro_txn()
            .map_err(|e| format!("LMDB read transaction: {e}"))?;
        let mut out = Vec::new();
        {
            let mut cursor = txn
                .open_ro_cursor(self.db)
                .map_err(|e| format!("LMDB cursor: {e}"))?;
            for item in cursor.iter() {
                let (k, v) = item.map_err(|e| format!("LMDB iterate: {e}"))?;
                out.push((k.to_vec(), v.to_vec()));
            }
        }
        txn.commit().map_err(|e| format!("LMDB commit: {e}"))?;
        Ok(out)
    }

    fn num_records(&self) -> usize {
        let Ok(txn) = self.env.begin_ro_txn() else {
            return 0;
        };
        txn.stat(self.db).map(|s| s.entries()).unwrap_or(0)
    }
}

/// A store manager over a single LMDB environment (port of `LmdbStoreManager`).
pub struct LmdbStoreManager {
    env: Arc<Environment>,
}

impl LmdbStoreManager {
    /// Open (creating if absent) an LMDB environment at `dir_path` with the given max size (port of
    /// `LmdbStoreManager.apply`).
    pub fn new(dir_path: &Path, max_env_size: usize) -> Result<Self, String> {
        std::fs::create_dir_all(dir_path).map_err(|e| e.to_string())?;
        let mut builder = Environment::new();
        builder.set_map_size(max_env_size);
        builder.set_max_dbs(20);
        builder.set_max_readers(2048);
        let env = builder.open(dir_path).map_err(|e| e.to_string())?;
        Ok(LmdbStoreManager {
            env: Arc::new(env),
        })
    }

    /// Open (creating if absent) a named database and return the raw synchronous store (mirror of
    /// [`KeyValueStoreManager::store`] without the `Arc<Mutex>` wrapper, used by the RSpace
    /// exporter/importer which require `KeyValueStore` directly).
    pub fn store_sync(&self, name: &str) -> Result<Box<dyn KeyValueStore + Send + Sync>, String> {
        let db = self
            .env
            .create_db(Some(name), DatabaseFlags::empty())
            .map_err(|e| e.to_string())?;
        Ok(Box::new(LmdbKeyValueStore::new(self.env.clone(), db)))
    }
}

#[async_trait]
impl KeyValueStoreManager for LmdbStoreManager {
    async fn store(&self, name: &str) -> Result<SharedStore, String> {
        let db = self
            .env
            .create_db(Some(name), DatabaseFlags::empty())
            .map_err(|e| e.to_string())?;
        Ok(Arc::new(tokio::sync::Mutex::new(Box::new(
            LmdbKeyValueStore::new(self.env.clone(), db),
        ) as Box<dyn KeyValueStore + Send + Sync>)))
    }

    async fn shutdown(&self) {
        // The environment is closed when the last `Arc` handle is dropped (lmdb-rkv `Drop`).
    }
}

/// A store manager that distributes databases across multiple LMDB environments (files) (port of
/// `LmdbDirStoreManager`).
///
/// Each `Db` is assigned an `LmdbEnvConfig` naming the environment (file) that holds it; databases
/// sharing an environment name live in the same LMDB file. Environments are opened lazily on first
/// access and cached, keyed by the environment name.
#[derive(Clone)]
pub struct LmdbDirStoreManager {
    dir_path: PathBuf,
    /// Database id → (database, environment config).
    db_mapping: BTreeMap<String, (Db, LmdbEnvConfig)>,
    /// Environment name → lazily-opened store manager.
    managers: Arc<tokio::sync::Mutex<BTreeMap<String, Arc<LmdbStoreManager>>>>,
}

impl LmdbDirStoreManager {
    /// Build a directory store manager (port of `LmdbDirStoreManager.apply`). Environments are not
    /// opened until their first database is requested.
    pub fn new(dir_path: impl AsRef<Path>, db_mapping: Vec<(Db, LmdbEnvConfig)>) -> Self {
        let db_mapping = db_mapping
            .into_iter()
            .map(|(db, cfg)| (db.id.clone(), (db, cfg)))
            .collect();
        LmdbDirStoreManager {
            dir_path: dir_path.as_ref().to_path_buf(),
            db_mapping,
            managers: Arc::new(tokio::sync::Mutex::new(BTreeMap::new())),
        }
    }

    /// Open (creating if absent) a named database and return the raw synchronous store (mirror of
    /// [`KeyValueStoreManager::store`] without the `Arc<Mutex>` wrapper).
    pub async fn store_sync(
        &self,
        name: &str,
    ) -> Result<Box<dyn KeyValueStore + Send + Sync>, String> {
        let (db, cfg) = self
            .db_mapping
            .get(name)
            .cloned()
            .ok_or_else(|| format!("unknown database: {name}"))?;

        let manager = {
            let mut managers = self.managers.lock().await;
            if !managers.contains_key(&cfg.name) {
                let dir = self.dir_path.join(&cfg.name);
                let max_env_size =
                    usize::try_from(cfg.max_env_size).map_err(|e| e.to_string())?;
                let created = LmdbStoreManager::new(&dir, max_env_size)
                    .map_err(|e| format!("open LMDB environment {}: {e}", cfg.name))?;
                managers.insert(cfg.name.clone(), Arc::new(created));
            }
            managers
                .get(&cfg.name)
                .cloned()
                .ok_or_else(|| format!("missing manager for {}", cfg.name))?
        };

        let db_name = db.name_override.unwrap_or(db.id);
        manager.store_sync(&db_name)
    }
}

#[async_trait]
impl KeyValueStoreManager for LmdbDirStoreManager {
    async fn store(&self, name: &str) -> Result<SharedStore, String> {
        let (db, cfg) = self
            .db_mapping
            .get(name)
            .cloned()
            .ok_or_else(|| format!("unknown database: {name}"))?;

        let manager = {
            let mut managers = self.managers.lock().await;
            if !managers.contains_key(&cfg.name) {
                let dir = self.dir_path.join(&cfg.name);
                let max_env_size =
                    usize::try_from(cfg.max_env_size).map_err(|e| e.to_string())?;
                let created = LmdbStoreManager::new(&dir, max_env_size)
                    .map_err(|e| format!("open LMDB environment {}: {e}", cfg.name))?;
                managers.insert(cfg.name.clone(), Arc::new(created));
            }
            managers
                .get(&cfg.name)
                .cloned()
                .ok_or_else(|| format!("missing manager for {}", cfg.name))?
        };

        let db_name = db.name_override.unwrap_or(db.id);
        manager.store(&db_name).await
    }

    async fn shutdown(&self) {
        let managers = self.managers.lock().await;
        for manager in managers.values() {
            manager.shutdown().await;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temp_dir() -> std::path::PathBuf {
        std::env::temp_dir().join(format!(
            "rchain-lmdb-test-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ))
    }

    #[tokio::test]
    async fn lmdb_store_round_trips() {
        let dir = temp_dir();
        let manager = LmdbStoreManager::new(&dir, 10 * 1024 * 1024).unwrap();
        let store = manager.store("db").await.unwrap();

        {
            let mut kv = store.lock().await;
            kv.put(vec![
                (b"k1".to_vec(), b"v1".to_vec()),
                (b"k2".to_vec(), b"v2".to_vec()),
            ])
            .unwrap();
        }
        {
            let kv = store.lock().await;
            assert_eq!(kv.get(&[b"k1".to_vec()]).unwrap(), vec![Some(b"v1".to_vec())]);
            assert_eq!(kv.get(&[b"missing".to_vec()]).unwrap(), vec![None]);
            assert_eq!(kv.entries().unwrap().len(), 2);
        }
        {
            let mut kv = store.lock().await;
            assert_eq!(kv.delete(&[b"k1".to_vec()]).unwrap(), 1);
            assert_eq!(kv.entries().unwrap().len(), 1);
        }

        manager.shutdown().await;
        drop(manager);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[tokio::test]
    async fn dir_manager_groups_envs_and_round_trips() {
        let dir = temp_dir();
        let mapping = vec![
            (Db::new("a"), LmdbEnvConfig::new("env1", 1024 * 1024)),
            (Db::new("b"), LmdbEnvConfig::new("env1", 1024 * 1024)),
            (
                Db {
                    id: "c".to_string(),
                    name_override: Some("c-real".to_string()),
                },
                LmdbEnvConfig::new("env2", 1024 * 1024),
            ),
        ];
        let manager = LmdbDirStoreManager::new(&dir, mapping);

        // "a" and "b" share environment "env1" but are distinct databases.
        let store_a = manager.store("a").await.unwrap();
        let store_b = manager.store("b").await.unwrap();
        {
            let mut kv = store_a.lock().await;
            kv.put(vec![(b"k".to_vec(), b"va".to_vec())]).unwrap();
        }
        {
            let mut kv = store_b.lock().await;
            kv.put(vec![(b"k".to_vec(), b"vb".to_vec())]).unwrap();
        }
        {
            let kv = store_a.lock().await;
            assert_eq!(kv.get(&[b"k".to_vec()]).unwrap(), vec![Some(b"va".to_vec())]);
        }
        {
            let kv = store_b.lock().await;
            assert_eq!(kv.get(&[b"k".to_vec()]).unwrap(), vec![Some(b"vb".to_vec())]);
        }

        // The name override is used as the database name.
        let store_c = manager.store("c").await.unwrap();
        {
            let mut kv = store_c.lock().await;
            kv.put(vec![(b"k".to_vec(), b"vc".to_vec())]).unwrap();
        }
        {
            let kv = store_c.lock().await;
            assert_eq!(kv.get(&[b"k".to_vec()]).unwrap(), vec![Some(b"vc".to_vec())]);
        }

        manager.shutdown().await;
        drop(manager);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[tokio::test]
    async fn unknown_database_is_an_error() {
        let dir = temp_dir();
        let manager = LmdbDirStoreManager::new(
            &dir,
            vec![(Db::new("a"), LmdbEnvConfig::new("env1", 1024 * 1024))],
        );
        assert!(manager.store("nope").await.is_err());
    }
}
