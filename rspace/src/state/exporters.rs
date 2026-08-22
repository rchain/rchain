//! Disk export of the tuple-space state (port of `RSpaceExporterItems.scala` +
//! `RSpaceExporterDisk.scala`).
//!
//! `get_history_and_data` combines the `TrieExporter` primitives into a resumable chunk, and
//! `write_to_disk` drains chunks into two byte stores (history + cold). The LMDB-backed stores are
//! constructed by the caller (see `rchain_shared::lmdb::LmdbStoreManager`); these functions are
//! synchronous, matching the synchronous `TrieExporter`/`KeyValueStore` abstraction.

use std::collections::BTreeSet;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::state::TrieNode;
use rchain_shared::store::KeyValueStore;

use crate::state::{validate_state_items, EmptyHistoryException, RSpaceExporter, StoreItems};

/// Export one chunk of history + data items and the resume path (port of
/// `RSpaceExporterItems.getHistoryAndData`).
pub fn get_history_and_data<E: RSpaceExporter>(
    exporter: &E,
    start_path: &[(Blake2b256Hash, Option<u8>)],
    skip: usize,
    take: usize,
) -> Result<
    (
        StoreItems<Blake2b256Hash, Vec<u8>>,
        StoreItems<Blake2b256Hash, Vec<u8>>,
    ),
    String,
> {
    let nodes = exporter.get_nodes(start_path, skip, take);
    let last = nodes
        .last()
        .cloned()
        .ok_or_else(|| EmptyHistoryException.to_string())?;

    let (leafs, non_leafs): (Vec<TrieNode<Blake2b256Hash>>, Vec<TrieNode<Blake2b256Hash>>) =
        nodes.into_iter().partition(|n| n.is_leaf);

    let history_keys: Vec<Blake2b256Hash> = non_leafs
        .into_iter()
        .map(|n| n.hash)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    let data_keys: Vec<Blake2b256Hash> = leafs
        .into_iter()
        .map(|n| n.hash)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();

    let history_items = exporter.get_history_items(&history_keys, |b: &[u8]| b.to_vec());
    let data_items = exporter.get_data_items(&data_keys, |b: &[u8]| b.to_vec());

    let mut last_path = last.path;
    last_path.push((last.hash, None));

    Ok((
        StoreItems {
            items: history_items,
            last_path: last_path.clone(),
        },
        StoreItems {
            items: data_items,
            last_path,
        },
    ))
}

/// Export the whole state from `root` into two byte stores in `chunk_size` chunks (port of
/// `RSpaceExporterDisk.writeToDisk`). The `history_store`/`data_store` are the LMDB-backed stores
/// (or any `KeyValueStore`) opened by the caller.
pub fn write_to_disk<E: RSpaceExporter>(
    exporter: &E,
    root: Blake2b256Hash,
    history_store: &mut dyn KeyValueStore,
    data_store: &mut dyn KeyValueStore,
    chunk_size: usize,
) -> Result<(), String> {
    let mut start_path: Vec<(Blake2b256Hash, Option<u8>)> = vec![(root, None)];
    loop {
        let (history, data) = get_history_and_data(exporter, &start_path, 0, chunk_size)?;

        // Validate the chunk against the trie (re-reads the target history store).
        {
            let hs: &dyn KeyValueStore = &*history_store;
            let get_from_history = |k: &Blake2b256Hash| -> Option<Vec<u8>> {
                hs.get(&[k.to_byte_array().to_vec()])
                    .unwrap_or_default()
                    .into_iter()
                    .next()
                    .flatten()
            };
            validate_state_items(
                &history.items,
                &data.items,
                &start_path,
                chunk_size as i32,
                0,
                &get_from_history,
            )
            .map_err(|e| e.to_string())?;
        }

        // Write history + data items (parallel in the Scala, sequential here).
        let history_pairs: Vec<(Vec<u8>, Vec<u8>)> = history
            .items
            .iter()
            .map(|(k, v)| (k.to_byte_array().to_vec(), v.clone()))
            .collect();
        history_store.put(history_pairs)?;

        let data_pairs: Vec<(Vec<u8>, Vec<u8>)> = data
            .items
            .iter()
            .map(|(k, v)| (k.to_byte_array().to_vec(), v.clone()))
            .collect();
        data_store.put(data_pairs)?;

        let received = history.items.len();
        if received < chunk_size {
            return Ok(());
        }
        start_path = history.last_path;
    }
}

/// Open the `history`/`cold` LMDB environments under `dir` and drain the exporter's whole state to
/// them (port of `RSpaceExporterDisk.writeToDisk`'s store-opening wrapper; the chunked drain is
/// [`write_to_disk`]). Gated behind the `lmdb` feature.
#[cfg(feature = "lmdb")]
pub fn write_to_disk_dir<E: RSpaceExporter>(
    exporter: &E,
    dir: &std::path::Path,
    chunk_size: usize,
) -> Result<(), String> {
    use rchain_shared::lmdb::LmdbStoreManager;

    let root = exporter
        .get_root()
        .ok_or_else(|| "exporter has no root to export".to_string())?;
    let history_manager =
        LmdbStoreManager::new(&dir.join("history"), 10 * 1024 * 1024 * 1024)?;
    let cold_manager = LmdbStoreManager::new(&dir.join("cold"), 10 * 1024 * 1024 * 1024)?;
    let mut history_store = history_manager.store_sync("db")?;
    let mut data_store = cold_manager.store_sync("db")?;
    write_to_disk(
        exporter,
        root,
        &mut *history_store,
        &mut *data_store,
        chunk_size,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::state::TrieExporter;

    fn leaf_hash() -> Blake2b256Hash {
        Blake2b256Hash::from_bytes([0x42; 32])
    }
    fn root_hash() -> Blake2b256Hash {
        Blake2b256Hash::from_bytes([0x24; 32])
    }

    struct MockExporter;
    impl TrieExporter<Blake2b256Hash> for MockExporter {
        fn get_nodes(
            &self,
            _start_path: &[(Blake2b256Hash, Option<u8>)],
            _skip: usize,
            _take: usize,
        ) -> Vec<TrieNode<Blake2b256Hash>> {
            vec![
                TrieNode {
                    hash: leaf_hash(),
                    is_leaf: true,
                    path: vec![],
                },
                TrieNode {
                    hash: root_hash(),
                    is_leaf: false,
                    path: vec![(root_hash(), None)],
                },
            ]
        }
        fn get_history_items<Value>(
            &self,
            keys: &[Blake2b256Hash],
            from_buffer: impl Fn(&[u8]) -> Value,
        ) -> Vec<(Blake2b256Hash, Value)> {
            keys.iter().map(|k| (*k, from_buffer(&[1u8]))).collect()
        }
        fn get_data_items<Value>(
            &self,
            keys: &[Blake2b256Hash],
            from_buffer: impl Fn(&[u8]) -> Value,
        ) -> Vec<(Blake2b256Hash, Value)> {
            keys.iter().map(|k| (*k, from_buffer(&[2u8]))).collect()
        }
    }
    impl RSpaceExporter for MockExporter {
        fn get_root(&self) -> Option<Blake2b256Hash> {
            None
        }
    }

    #[test]
    fn get_history_and_data_splits_leafs_and_history() {
        let (history, data) =
            get_history_and_data(&MockExporter, &[(root_hash(), None)], 0, 10).unwrap();
        assert_eq!(history.items, vec![(root_hash(), vec![1u8])]);
        assert_eq!(data.items, vec![(leaf_hash(), vec![2u8])]);
        assert_eq!(
            history.last_path,
            vec![(root_hash(), None), (root_hash(), None)]
        );
    }
}
