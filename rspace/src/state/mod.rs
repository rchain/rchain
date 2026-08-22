//! Tuple-space state export/import (port of `rspace/.../state/`).
//!
//! Ported here are the data types, the pure algorithms (`RSpaceExporter.traverseHistory` over
//! `RadixTree.sequentialExport`, and `RSpaceImporter.validateStateItems`), the store-backed
//! instances (`RSpaceExporterStore`/`RSpaceImporterStore`/`RSpaceStateManagerImpl`), and the disk
//! exporter (`RSpaceExporterDisk.writeToDisk` in `exporters`). The foundational
//! `TrieExporter`/`TrieNode`/`TrieImporter`/`StateManager` abstractions live in `rchain_shared::state`.

use std::collections::BTreeMap;
use std::fmt;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::state::{StateManager, TrieExporter, TrieImporter, TrieNode};

use crate::history::export::{sequential_export, ExportDataSettings};
use crate::history::key_segment::KeySegment;

pub mod exporters;
pub mod instances;

/// Export skip/take counters (port of `RSpaceExporter.Counter`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Counter {
    pub skip: usize,
    pub take: usize,
}

/// Raised when the history is empty (port of `RSpaceExporter.EmptyHistoryException`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EmptyHistoryException;

impl fmt::Display for EmptyHistoryException {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EmptyHistoryException")
    }
}

impl std::error::Error for EmptyHistoryException {}

/// A state-validation failure (port of `RSpaceImporter.StateValidationError`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateValidationError(pub String);

impl fmt::Display for StateValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StateValidationError: {}", self.0)
    }
}

impl std::error::Error for StateValidationError {}

/// A chunk of exported items plus the path of the last item (port of
/// `RSpaceExporterItems.StoreItems`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StoreItems<KeyHash, Value> {
    pub items: Vec<(KeyHash, Value)>,
    pub last_path: Vec<(KeyHash, Option<u8>)>,
}

/// Format a `(hash, index)` path for pretty printing (port of `RSpaceExporter.pathPretty`).
pub fn path_pretty(path: &(Blake2b256Hash, Option<u8>)) -> String {
    let (hash, idx) = path;
    let idx_str = match idx {
        None => "--".to_string(),
        Some(i) => format!("{:02x}", i & 0xff),
    };
    let hash_hex: String = hash
        .as_bytes()
        .iter()
        .take(4)
        .map(|b| format!("{:02x}", b))
        .collect();
    format!("{}:{}", idx_str, hash_hex)
}

/// Decode the last-exported prefix from its 5-hash encoding (port of `createLastPrefix`).
///
/// The input is a peer-supplied resume path, so malformed shapes are an `Err`, not a panic.
fn create_last_prefix(prefix_seq: &[Blake2b256Hash]) -> Result<Option<KeySegment>, String> {
    if prefix_seq.is_empty() {
        return Ok(None);
    }
    if prefix_seq.len() < 5 {
        return Err("Invalid path during export: expected 5 prefix hashes.".to_string());
    }
    let size_prefix = prefix_seq[0].as_bytes()[0] as usize;
    if size_prefix > 128 {
        return Err(format!(
            "Invalid path during export: prefix size {size_prefix} exceeds 128."
        ));
    }
    let mut prefix128 = Vec::with_capacity(128);
    for i in 0..4 {
        prefix128.extend_from_slice(prefix_seq[1 + i].as_bytes());
    }
    Ok(Some(KeySegment::new(prefix128[..size_prefix].to_vec())))
}

/// Build leaf/history `TrieNode`s from their hashes (port of `constructNodes`).
fn construct_nodes(
    leaf_keys: Vec<Blake2b256Hash>,
    node_keys: Vec<Blake2b256Hash>,
) -> Vec<TrieNode<Blake2b256Hash>> {
    let mut out = Vec::with_capacity(leaf_keys.len() + node_keys.len());
    for k in leaf_keys {
        out.push(TrieNode {
            hash: k,
            is_leaf: true,
            path: Vec::new(),
        });
    }
    for k in node_keys {
        out.push(TrieNode {
            hash: k,
            is_leaf: false,
            path: Vec::new(),
        });
    }
    out
}

/// Encode the last-exported prefix as the 6-hash path used to resume export (port of
/// `constructLastPath`).
fn construct_last_path(
    last_prefix: &[u8],
    root_hash: Blake2b256Hash,
) -> Vec<(Blake2b256Hash, Option<u8>)> {
    let prefix_size = last_prefix.len();
    let mut size_array = [0u8; 32];
    size_array[0] = prefix_size as u8;
    let mut prefix128_array = [0u8; 128];
    prefix128_array[..prefix_size].copy_from_slice(last_prefix);

    vec![
        (root_hash, None),
        (Blake2b256Hash::from_byte_array(&size_array), None),
        (
            Blake2b256Hash::from_byte_array(&prefix128_array[0..32]),
            None,
        ),
        (
            Blake2b256Hash::from_byte_array(&prefix128_array[32..64]),
            None,
        ),
        (
            Blake2b256Hash::from_byte_array(&prefix128_array[64..96]),
            None,
        ),
        (
            Blake2b256Hash::from_byte_array(&prefix128_array[96..128]),
            None,
        ),
    ]
}

fn construct_last_node(
    last_hash: Blake2b256Hash,
    last_path: Vec<(Blake2b256Hash, Option<u8>)>,
) -> Vec<TrieNode<Blake2b256Hash>> {
    vec![TrieNode {
        hash: last_hash,
        is_leaf: false,
        path: last_path,
    }]
}

/// Walk the trie and convert the exported nodes into path-indexed `TrieNode`s (port of
/// `RSpaceExporter.traverseHistory`).
pub fn traverse_history(
    start_path: &[(Blake2b256Hash, Option<u8>)],
    skip: i32,
    take: i32,
    get_from_history: &dyn Fn(&Blake2b256Hash) -> Option<Vec<u8>>,
) -> Result<Vec<TrieNode<Blake2b256Hash>>, String> {
    let settings = ExportDataSettings {
        flag_node_prefixes: false,
        flag_node_keys: true,
        flag_node_values: false,
        flag_leaf_prefixes: false,
        flag_leaf_values: true,
    };

    if start_path.is_empty() {
        return Ok(Vec::new());
    }

    let path_seq: Vec<Blake2b256Hash> = start_path.iter().map(|(h, _)| *h).collect();
    let root_hash = path_seq[0];
    let last_prefix = create_last_prefix(&path_seq[1..])?;

    let (data, new_last_prefix_opt) =
        sequential_export(root_hash, last_prefix, skip, take, get_from_history, &settings)?;

    let node_keys = data.node_keys.clone();
    let leaf_keys = data.leaf_values.clone();
    let mut nodes = construct_nodes(leaf_keys, node_keys.clone());
    if !nodes.is_empty() {
        nodes.pop();
    }

    let last_path = match new_last_prefix_opt {
        Some(prefix) => construct_last_path(prefix.as_bytes(), root_hash),
        None => Vec::new(),
    };
    let last_history_node = match node_keys.last() {
        Some(last_hash) => construct_last_node(*last_hash, last_path),
        None => Vec::new(),
    };

    nodes.extend(last_history_node);
    Ok(nodes)
}

/// Validate a chunk of exported history/data items against the trie (port of
/// `RSpaceImporter.validateStateItems`).
pub fn validate_state_items(
    history_items: &[(Blake2b256Hash, Vec<u8>)],
    data_items: &[(Blake2b256Hash, Vec<u8>)],
    start_path: &[(Blake2b256Hash, Option<u8>)],
    chunk_size: i32,
    skip: i32,
    get_from_history: &dyn Fn(&Blake2b256Hash) -> Option<Vec<u8>>,
) -> Result<(), StateValidationError> {
    let received = history_items.len() as i32;
    let is_end = received < chunk_size;
    if !(received == chunk_size || is_end) {
        return Err(StateValidationError(format!(
            "Input size of history items is not valid. Expected chunk size {chunk_size}, received {received}."
        )));
    }

    // Validate tries from the received history items, building the received-node map.
    let mut trie_map: BTreeMap<Blake2b256Hash, Vec<u8>> = BTreeMap::new();
    for (hash, trie_bytes) in history_items {
        let trie_hash = Blake2b256Hash::create(trie_bytes);
        if hash != &trie_hash {
            return Err(StateValidationError(format!(
                "Trie hash does not match decoded trie, key: {}, decoded: {}.",
                hash.to_hex(),
                trie_hash.to_hex()
            )));
        }
        trie_map.insert(trie_hash, trie_bytes.clone());
    }

    let get_node = |hash: &Blake2b256Hash| -> Option<Vec<u8>> {
        match trie_map.get(hash) {
            Some(bytes) => Some(bytes.clone()),
            None => get_from_history(hash),
        }
    };

    let nodes = traverse_history(start_path, skip, chunk_size, &get_node)
        .map_err(StateValidationError)?;

    let mut data_keys = Vec::new();
    let mut history_keys = Vec::new();
    for node in &nodes {
        if node.is_leaf {
            data_keys.push(node.hash);
        } else {
            history_keys.push(node.hash);
        }
    }

    let history_items_keys: Vec<Blake2b256Hash> = history_items.iter().map(|(h, _)| *h).collect();
    if history_items_keys != history_keys {
        return Err(StateValidationError("History items are corrupted.".to_string()));
    }
    let data_items_keys: Vec<Blake2b256Hash> = data_items.iter().map(|(h, _)| *h).collect();
    if data_items_keys != data_keys {
        return Err(StateValidationError("Data items are corrupted.".to_string()));
    }

    // Validate data (leaf) item hashes.
    for (hash, value_bytes) in data_items {
        let data_hash = Blake2b256Hash::create(value_bytes);
        if hash != &data_hash {
            return Err(StateValidationError(format!(
                "Data hash does not match decoded data, key: {}, decoded: {}.",
                hash.to_hex(),
                data_hash.to_hex()
            )));
        }
    }

    Ok(())
}

/// The rspace exporter (port of `RSpaceExporter`).
pub trait RSpaceExporter: TrieExporter<Blake2b256Hash> {
    /// The current root, if set (port of `getRoot`; `None` is the `NoRootError` case).
    fn get_root(&self) -> Option<Blake2b256Hash>;
}

/// The rspace state manager (port of `RSpaceStateManager`).
pub trait RSpaceStateManager: StateManager {
    type Exporter: RSpaceExporter;
    type Importer: RSpaceImporter;

    fn exporter(&self) -> &Self::Exporter;
    fn importer(&self) -> &Self::Importer;
}

/// The rspace importer (port of `RSpaceImporter`).
pub trait RSpaceImporter: TrieImporter<Blake2b256Hash> {
    fn get_history_item(&self, hash: Blake2b256Hash) -> Option<Vec<u8>>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    use crate::history::radix_tree::{empty_node, hash_node, Item};

    fn single_leaf_store() -> (HashMap<Blake2b256Hash, Vec<u8>>, Blake2b256Hash, Blake2b256Hash) {
        let mut root = empty_node();
        let leaf_hash = Blake2b256Hash::from_bytes([0x42; 32]);
        root[0] = Item::Leaf {
            prefix: KeySegment::new(vec![1]),
            value: leaf_hash,
        };
        let (root_hash, root_bytes) = hash_node(&root);
        let store = HashMap::from([(root_hash, root_bytes)]);
        (store, root_hash, leaf_hash)
    }

    #[test]
    fn traverse_history_emits_leaf_and_last_node() {
        let (store, root_hash, leaf_hash) = single_leaf_store();
        let get = |h: &Blake2b256Hash| store.get(h).cloned();
        let nodes =
            traverse_history(&[(root_hash, None)], 0, 10, &get).unwrap();
        assert_eq!(nodes.len(), 2);
        assert!(nodes[0].is_leaf);
        assert_eq!(nodes[0].hash, leaf_hash);
        assert!(!nodes[1].is_leaf);
        assert_eq!(nodes[1].hash, root_hash);
    }

    /// A single-leaf trie whose leaf value is the hash of `data_value` (so the export/import
    /// round-trip can be validated end-to-end).
    fn leaf_trie(
        data_value: Vec<u8>,
    ) -> (
        HashMap<Blake2b256Hash, Vec<u8>>,
        Blake2b256Hash,
        Vec<u8>,
        Blake2b256Hash,
    ) {
        let data_hash = Blake2b256Hash::create(&data_value);
        let mut root = empty_node();
        root[0] = Item::Leaf {
            prefix: KeySegment::new(vec![1]),
            value: data_hash,
        };
        let (root_hash, root_bytes) = hash_node(&root);
        let store = HashMap::from([(root_hash, root_bytes.clone())]);
        (store, root_hash, root_bytes, data_hash)
    }

    #[test]
    fn validate_state_items_accepts_valid_round_trip() {
        let data_value = vec![1, 2, 3];
        let (store, root_hash, root_bytes, data_hash) = leaf_trie(data_value.clone());
        let get = |h: &Blake2b256Hash| store.get(h).cloned();

        let history_items = vec![(root_hash, root_bytes)];
        let data_items = vec![(data_hash, data_value)];
        validate_state_items(&history_items, &data_items, &[(root_hash, None)], 10, 0, &get)
            .unwrap();
    }

    #[test]
    fn validate_state_items_rejects_corrupted_data() {
        let data_value = vec![1, 2, 3];
        let (store, root_hash, root_bytes, data_hash) = leaf_trie(data_value);
        let get = |h: &Blake2b256Hash| store.get(h).cloned();

        // The data bytes no longer hash to the claimed `data_hash`.
        let history_items = vec![(root_hash, root_bytes)];
        let data_items = vec![(data_hash, vec![9, 9, 9])];
        assert!(validate_state_items(&history_items, &data_items, &[(root_hash, None)], 10, 0, &get)
            .is_err());
    }
}
