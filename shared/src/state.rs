//! Trie export/import and state-manager abstractions.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/state/{TrieExporter,TrieImporter,StateManager}.scala`.
//! The Scala `F[_]` effect and `ByteBuffer` zero-copy handling are simplified to synchronous
//! `Vec<u8>` operations, matching the crate's `store` module convention.

/// A trie node with its path from the root (port of `TrieNode`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TrieNode<KeyHash> {
    pub hash: KeyHash,
    pub is_leaf: bool,
    pub path: Vec<(KeyHash, Option<u8>)>,
}

/// Traverses a trie and converts it to path-indexed nodes (port of `TrieExporter`).
pub trait TrieExporter<KeyHash: Clone> {
    /// Get trie nodes with offset from the start path and a number of nodes.
    fn get_nodes(
        &self,
        start_path: &[(KeyHash, Option<u8>)],
        skip: usize,
        take: usize,
    ) -> Vec<TrieNode<KeyHash>>;

    /// Get history values (branch nodes) by key.
    fn get_history_items<Value>(
        &self,
        keys: &[KeyHash],
        from_buffer: impl Fn(&[u8]) -> Value,
    ) -> Vec<(KeyHash, Value)>;

    /// Get data values (leaf nodes) by key.
    fn get_data_items<Value>(
        &self,
        keys: &[KeyHash],
        from_buffer: impl Fn(&[u8]) -> Value,
    ) -> Vec<(KeyHash, Value)>;
}

/// Writes trie history/data items back (port of `TrieImporter`).
pub trait TrieImporter<KeyHash: Clone> {
    /// Set history values (branch nodes).
    fn set_history_items<Value>(
        &mut self,
        data: &[(KeyHash, Value)],
        to_buffer: impl Fn(&Value) -> Vec<u8>,
    );

    /// Set data values (leaf nodes).
    fn set_data_items<Value>(
        &mut self,
        data: &[(KeyHash, Value)],
        to_buffer: impl Fn(&Value) -> Vec<u8>,
    );

    /// Set the current root hash.
    fn set_root(&mut self, key: KeyHash);
}

/// Checks whether the state is empty (port of `StateManager`).
pub trait StateManager {
    fn is_empty(&self) -> bool;
}
