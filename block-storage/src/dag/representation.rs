//! The in-memory DAG view.
//!
//! Mirrors `block-storage/src/main/scala/coop/rchain/blockstorage/dag/DagRepresentation.scala`.

use std::collections::{BTreeMap, BTreeSet};

use rchain_models::block_hash::BlockHash;
use rchain_models::fringe_data::FringeData;
use rchain_models::validator::Validator;
use rchain_shared::base16;
use rchain_shared::refined::{BlockHeight, NonNegI64};

use crate::errors::StorageError;

use super::finalizer::Message;
use super::message_state::DagMessageState;

/// The in-memory state of the DAG — an index of the block metadata store.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DagRepresentation {
    pub dag_set: BTreeSet<BlockHash>,
    pub child_map: BTreeMap<BlockHash, BTreeSet<BlockHash>>,
    pub height_map: BTreeMap<BlockHeight, BTreeSet<BlockHash>>,
    pub dag_message_state: DagMessageState<BlockHash, Validator>,
    pub fringe_states: BTreeMap<BTreeSet<BlockHash>, FringeData>,
}

impl DagRepresentation {
    pub fn latest_fringe(&self) -> BTreeSet<Message<BlockHash, Validator>> {
        self.dag_message_state.latest_fringe()
    }

    /// The finalized blocks are the seen-closure of the latest fringe.
    pub fn finalized_blocks_set(&self) -> BTreeSet<BlockHash> {
        self.latest_fringe()
            .iter()
            .flat_map(|m| m.seen.iter().copied())
            .collect()
    }

    pub fn latest_block_number(&self) -> i64 {
        self.height_map
            .keys()
            .last()
            .map(|h| i64::from(*h + NonNegI64::one()))
            .unwrap_or(0)
    }

    pub fn last_finalized_block_hash(&self) -> Option<BlockHash> {
        self.latest_fringe()
            .iter()
            .map(|m| (m.height, m.id))
            .max()
            .map(|(_, id)| id)
    }

    /// The last finalized block hash, or an error if no fringe is available (port of
    /// `DagRepresentationSyntax.lastFinalizedBlockUnsafe`).
    pub fn last_finalized_block_unsafe(&self) -> Result<BlockHash, String> {
        self.last_finalized_block_hash()
            .ok_or_else(|| "Finalized fringe is not available.".to_string())
    }

    pub fn contains(&self, block_hash: &BlockHash) -> bool {
        self.dag_set.contains(block_hash)
    }

    pub fn children(&self, block_hash: &BlockHash) -> Option<&BTreeSet<BlockHash>> {
        self.child_map.get(block_hash)
    }

    pub fn is_finalized(&self, block_hash: &BlockHash) -> bool {
        self.finalized_blocks_set().contains(block_hash)
    }

    /// Blocks grouped by height in the requested range (or `None` for an invalid range).
    pub fn topo_sort(
        &self,
        start_block_number: i64,
        maybe_end_block_number: Option<i64>,
    ) -> Option<Vec<Vec<BlockHash>>> {
        let max_number = self.latest_block_number();
        let start_number = 0.max(start_block_number);
        let end_number = maybe_end_block_number
            .map(|e| e.min(max_number))
            .unwrap_or(max_number);
        let valid_range = start_number >= 0 && start_number <= end_number;
        if valid_range {
            Some(
                self.height_map
                    .iter()
                    .filter(|(h, _)| i64::from(**h) >= start_number && i64::from(**h) <= end_number)
                    .map(|(_, v)| v.iter().copied().collect())
                    .collect(),
            )
        } else {
            None
        }
    }

    /// Blocks grouped by height in the requested range, or an error for an invalid range (port of
    /// `DagRepresentationSyntax.topoSortUnsafe`).
    pub fn topo_sort_unsafe(
        &self,
        start_block_number: i64,
        maybe_end_block_number: Option<i64>,
    ) -> Result<Vec<Vec<BlockHash>>, StorageError> {
        self.topo_sort(start_block_number, maybe_end_block_number)
            .ok_or(StorageError::TopoSortFragmentParameterError {
                start_block_number,
                end_block_number: i64::MAX,
            })
    }

    /// Find a block hash by (possibly truncated) hex prefix.
    pub fn find(&self, truncated_hash: &str) -> Option<BlockHash> {
        // Validate-on-ingress: reject non-hex input (`decode` returns `None`) rather than silently
        // dropping invalid chars via `unsafe_decode`.
        if truncated_hash.len().is_multiple_of(2) {
            let bytes = base16::decode(truncated_hash)?;
            self.dag_set.iter().find(|h| h.starts_with(&bytes)).copied()
        } else {
            let bytes = base16::decode(&truncated_hash[..truncated_hash.len() - 1])?;
            self.dag_set
                .iter()
                .filter(|h| h.starts_with(&bytes))
                .find(|h| h.to_hex().starts_with(truncated_hash))
                .copied()
        }
    }
}
