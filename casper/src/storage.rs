//! RNode key-value store layout (port of `storage/RNodeKeyValueStoreManager.scala`).

use std::path::Path;

use rchain_shared::lmdb::LmdbDirStoreManager;
use rchain_shared::store_manager::{Db, LmdbEnvConfig, GB, TB};

/// The RNode DB → LMDB environment mapping (port of `rnodeDbMapping`).
///
/// Keys with the same environment name share one LMDB file.
pub fn rnode_db_mapping() -> Vec<(Db, LmdbEnvConfig)> {
    vec![
        // Block storage
        (Db::new("blocks"), LmdbEnvConfig::new("blockstorage", 1 * TB)),
        // Block metadata storage
        (
            Db::new("block-metadata"),
            LmdbEnvConfig::new("dagstorage", 100 * GB),
        ),
        (Db::new("fringe-data"), LmdbEnvConfig::new("dagstorage", 100 * GB)),
        (
            Db::new("finalized-store"),
            LmdbEnvConfig::new("dagstorage", 100 * GB),
        ),
        // Deploys from blocks
        (Db::new("deploy-index"), LmdbEnvConfig::new("dagstorage", 100 * GB)),
        // Runtime mergeable store (cache of mergeable channels for block-merge)
        (
            Db::new("mergeable-channel-cache"),
            LmdbEnvConfig::new("dagstorage", 100 * GB),
        ),
        // Deploys waiting to be added
        (
            Db::new("deploy-pool"),
            LmdbEnvConfig::new("deploypoolstorage", 1 * GB),
        ),
        // Reporting (trace) cache
        (
            Db::new("reporting-cache"),
            LmdbEnvConfig::new("reporting", 10 * TB),
        ),
        // On-chain RSpace (Rholang state); history and roots share one environment.
        (
            Db::new("rspace-history"),
            LmdbEnvConfig::new("rspace/history", 1 * TB),
        ),
        (
            Db::new("rspace-roots"),
            LmdbEnvConfig::new("rspace/history", 1 * TB),
        ),
        (Db::new("rspace-cold"), LmdbEnvConfig::new("rspace/cold", 1 * TB)),
        // Transaction store
        (Db::new("transaction"), LmdbEnvConfig::new("transaction", 1 * GB)),
        // Evaluator RSpace (Rholang state)
        (
            Db::new("eval-history"),
            LmdbEnvConfig::new("eval/history", 1 * TB),
        ),
        (
            Db::new("eval-roots"),
            LmdbEnvConfig::new("eval/history", 1 * TB),
        ),
        (Db::new("eval-cold"), LmdbEnvConfig::new("eval/cold", 1 * TB)),
    ]
}

/// Open the RNode LMDB store manager over `dir_path` (port of `RNodeKeyValueStoreManager.apply`).
///
/// Databases are distributed across LMDB environments (files) per [`rnode_db_mapping`]; the
/// environments are opened lazily on first access.
pub fn rnode_key_value_store_manager(dir_path: &Path) -> LmdbDirStoreManager {
    LmdbDirStoreManager::new(dir_path, rnode_db_mapping())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::store_manager::KeyValueStoreManager;

    #[test]
    fn mapping_has_expected_databases() {
        let mapping = rnode_db_mapping();
        let ids: Vec<&str> = mapping.iter().map(|(db, _)| db.id.as_str()).collect();
        assert!(ids.contains(&"blocks"));
        assert!(ids.contains(&"rspace-history"));
        assert!(ids.contains(&"mergeable-channel-cache"));
        // History and roots share an environment name.
        let history = mapping
            .iter()
            .find(|(db, _)| db.id == "rspace-history")
            .map(|(_, c)| c.name.clone());
        let roots = mapping
            .iter()
            .find(|(db, _)| db.id == "rspace-roots")
            .map(|(_, c)| c.name.clone());
        assert_eq!(history, roots);
    }

    #[tokio::test]
    async fn store_manager_round_trips() {
        let dir = std::env::temp_dir().join(format!(
            "rchain-rnode-kvm-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let manager = rnode_key_value_store_manager(&dir);
        let store = manager.store("deploy-pool").await.unwrap();
        {
            let mut kv = store.lock().await;
            kv.put(vec![(b"k".to_vec(), b"v".to_vec())]).unwrap();
        }
        {
            let kv = store.lock().await;
            assert_eq!(kv.get(&[b"k".to_vec()]).unwrap(), vec![Some(b"v".to_vec())]);
        }
        manager.shutdown().await;
        drop(manager);
        let _ = std::fs::remove_dir_all(&dir);
    }
}
