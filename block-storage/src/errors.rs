//! Storage errors.
//!
//! Mirrors `block-storage/src/main/scala/coop/rchain/blockstorage/errors.scala`.

use std::fmt;

/// A storage error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StorageError {
    /// The topo-sort range parameter is invalid.
    TopoSortFragmentParameterError { start_block_number: i64, end_block_number: i64 },
    /// LZ4 block-message decompression failed.
    DecompressionError,
    /// The latest-messages set was empty when a new message was created.
    EmptyLatestMessages,
}

impl fmt::Display for StorageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StorageError::TopoSortFragmentParameterError {
                start_block_number,
                end_block_number,
            } => write!(
                f,
                "topo-sort fragment parameter error: start {start_block_number}, end {end_block_number}"
            ),
            StorageError::DecompressionError => write!(f, "block message decompression failed"),
            StorageError::EmptyLatestMessages => write!(f, "empty latest messages"),
        }
    }
}

impl std::error::Error for StorageError {}

/// A block-store inconsistency: the store is missing a hash it was expected to contain (port of
/// `BlockStoreInconsistencyError`). Signals a fatal error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockStoreInconsistencyError(pub String);

impl fmt::Display for BlockStoreInconsistencyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for BlockStoreInconsistencyError {}

/// A block-DAG inconsistency (port of `BlockDagInconsistencyError`). Signals a fatal error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockDagInconsistencyError(pub String);

impl fmt::Display for BlockDagInconsistencyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for BlockDagInconsistencyError {}

/// A byte-string KV-store inconsistency (port of `ByteStringKVInconsistencyError`). Signals a
/// fatal error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ByteStringKVInconsistencyError(pub String);

impl fmt::Display for ByteStringKVInconsistencyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for ByteStringKVInconsistencyError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inconsistency_errors_carry_messages() {
        assert_eq!(
            BlockStoreInconsistencyError("boom".to_string()).to_string(),
            "boom"
        );
        assert_eq!(
            BlockDagInconsistencyError("boom".to_string()).to_string(),
            "boom"
        );
        assert_eq!(
            ByteStringKVInconsistencyError("boom".to_string()).to_string(),
            "boom"
        );
    }
}
