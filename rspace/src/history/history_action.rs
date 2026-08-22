//! Radix-tree mutation actions.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/history/HistoryAction.scala`.

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::history::key_segment::KeySegment;

/// A radix-tree insert or delete (port of `HistoryAction`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HistoryAction {
    Insert { key: KeySegment, hash: Blake2b256Hash },
    Delete { key: KeySegment },
}

impl HistoryAction {
    pub fn key(&self) -> &KeySegment {
        match self {
            HistoryAction::Insert { key, .. } => key,
            HistoryAction::Delete { key } => key,
        }
    }

    /// Drop the first byte of the key (port of the `trimKeys` helper).
    pub fn trim(&self) -> HistoryAction {
        match self {
            HistoryAction::Insert { key, hash } => HistoryAction::Insert {
                key: key.tail(),
                hash: *hash,
            },
            HistoryAction::Delete { key } => HistoryAction::Delete { key: key.tail() },
        }
    }
}
