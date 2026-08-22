//! Block metadata store — in-memory DAG state.
//!
//! Mirrors the pure logic of
//! `block-storage/src/main/scala/coop/rchain/blockstorage/dag/BlockMetadataStore.scala`. The
//! `F[_]`/`Ref`/store wrapper is deferred to the async layer; these pure functions implement
//! `addBlockToDagState` / `validateDagState` / `recreateInMemoryState` (Law 18: height-map
//! contiguity).

use std::collections::{BTreeMap, BTreeSet};

use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_shared::refined::{BlockHeight, NonNegI64};

/// The in-memory DAG state.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DagState {
    pub dag_set: BTreeSet<BlockHash>,
    pub child_map: BTreeMap<BlockHash, BTreeSet<BlockHash>>,
    pub height_map: BTreeMap<BlockHeight, BTreeSet<BlockHash>>,
}

impl DagState {
    pub fn empty() -> Self {
        Self {
            dag_set: BTreeSet::new(),
            child_map: BTreeMap::new(),
            height_map: BTreeMap::new(),
        }
    }
}

/// A projection of `BlockMetadata` used to rebuild in-memory state.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockInfo {
    pub hash: BlockHash,
    pub parents: BTreeSet<BlockHash>,
    pub block_num: BlockHeight,
    pub validation_failed: bool,
}

pub fn block_metadata_to_info(meta: &BlockMetadata) -> BlockInfo {
    BlockInfo {
        hash: meta.block_hash,
        parents: meta.justifications.clone(),
        block_num: meta.block_num,
        validation_failed: meta.validation_failed,
    }
}

pub fn add_block_to_dag_state(block: &BlockInfo, state: &DagState) -> DagState {
    let mut dag_set = state.dag_set.clone();
    dag_set.insert(block.hash);

    let mut child_map = state.child_map.clone();
    for parent in &block.parents {
        child_map.entry(*parent).or_default().insert(block.hash);
    }
    child_map.entry(block.hash).or_default();

    let mut height_map = state.height_map.clone();
    if !block.validation_failed {
        height_map
            .entry(block.block_num)
            .or_default()
            .insert(block.hash);
    }

    DagState {
        dag_set,
        child_map,
        height_map,
    }
}

/// Validate that the height-map keys form a contiguous range (Law 18).
pub fn validate_dag_state(state: &DagState) {
    let m = &state.height_map;
    let (min, max) = match (m.keys().next(), m.keys().next_back()) {
        (Some(first), Some(last)) => (*first, *last + NonNegI64::one()),
        _ => (BlockHeight::zero(), BlockHeight::zero()),
    };
    assert!(
        max - min == i64::try_from(m.len()).unwrap_or(i64::MAX),
        "DAG store height map has numbers not in sequence."
    );
}

/// Rebuild in-memory state from a block-info map, then validate.
pub fn recreate_in_memory_state(blocks: &BTreeMap<BlockHash, BlockInfo>) -> DagState {
    let mut state = DagState::empty();
    for block in blocks.values() {
        state = add_block_to_dag_state(block, &state);
    }
    validate_dag_state(&state);
    state
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hash(byte: u8) -> BlockHash {
        let mut bytes = [0u8; 32];
        bytes[0] = byte;
        BlockHash::new(bytes)
    }

    fn info(hash: BlockHash, parents: &[BlockHash], block_num: i64) -> BlockInfo {
        BlockInfo {
            hash,
            parents: parents.iter().copied().collect(),
            block_num: BlockHeight::try_from(block_num).unwrap(),
            validation_failed: false,
        }
    }

    #[test]
    fn law18_contiguous_height_map_is_valid() {
        let mut blocks = BTreeMap::new();
        let h0 = info(hash(0), &[], 0);
        let h1 = info(hash(1), &[hash(0)], 1);
        let h2 = info(hash(2), &[hash(1)], 2);
        blocks.insert(hash(0), h0);
        blocks.insert(hash(1), h1);
        blocks.insert(hash(2), h2);
        recreate_in_memory_state(&blocks); // does not panic
    }

    #[test]
    #[should_panic(expected = "numbers not in sequence")]
    fn law18_height_map_with_holes_panics() {
        let mut blocks = BTreeMap::new();
        blocks.insert(hash(0), info(hash(0), &[], 0));
        // block_num 2 with no block_num 1 -> hole.
        blocks.insert(hash(2), info(hash(2), &[hash(0)], 2));
        recreate_in_memory_state(&blocks);
    }

    #[test]
    fn add_block_to_dag_state_builds_child_map() {
        let parent = hash(0);
        let child = hash(1);
        let state = DagState::empty();
        let s1 = add_block_to_dag_state(&info(parent, &[], 0), &state);
        let s2 = add_block_to_dag_state(&info(child, &[parent], 1), &s1);
        assert_eq!(s2.child_map[&parent], [child].into_iter().collect());
        assert!(s2.child_map[&child].is_empty());
        assert_eq!(s2.dag_set, [parent, child].into_iter().collect());
    }
}
