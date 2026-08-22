//! Block proto utilities (port of `util/ProtoUtil.scala`) — Law 16 content addressing.

use std::collections::{BTreeMap, BTreeSet};

use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_models::block::state_hash::StateHash;
use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{BlockMessage, RholangState};
use rchain_models::validator::Validator;
use rchain_shared::refined::{BlockHeight, NonNegI64, SeqNum};

/// The maximum block number among the given metadata, or `-1` if empty (port of
/// `maxBlockNumberMetadata`).
pub fn max_block_number_metadata(blocks: &[BlockMetadata]) -> i64 {
    blocks.iter().fold(-1, |acc, b| acc.max(i64::from(b.block_num)))
}

/// Look up a block's (non-failed) parent metadata (port of `ProtoUtil.getParentsMetadata`).
pub async fn get_parents_metadata(
    dag: &dyn BlockDagStorage,
    b: &BlockMetadata,
) -> Result<Vec<BlockMetadata>, String> {
    let mut parents = Vec::new();
    for j in &b.justifications {
        let meta = dag
            .lookup(j)
            .await?
            .ok_or_else(|| format!("missing justification {}", j.to_hex()))?;
        if !meta.validation_failed {
            parents.push(meta);
        }
    }
    Ok(parents)
}

/// Look up a block's parents with block number at or above `block_number` (port of
/// `ProtoUtil.getParentMetadatasAboveBlockNumber`).
pub async fn get_parent_metadatas_above_block_number(
    dag: &dyn BlockDagStorage,
    b: &BlockMetadata,
    block_number: i64,
) -> Result<Vec<BlockMetadata>, String> {
    let parents = get_parents_metadata(dag, b).await?;
    Ok(parents
        .into_iter()
        .filter(|p| i64::from(p.block_num) >= block_number)
        .collect())
}

/// Create the hash of a `BlockMessage`; all fields except `sig` are included (port of `hashBlock`).
///
/// `block_hash` and `sig` are cleared before hashing. The Scala clears them to an *empty*
/// `ByteString`; the fixed-width Rust `BlockHash` maps that to a zero-filled 32-byte value.
pub fn hash_block(block: &BlockMessage) -> BlockHash {
    let mut cleared = block.clone();
    cleared.block_hash = BlockHash::new([0u8; 32]);
    cleared.sig = Vec::new();
    let bytes = cleared.to_bytes();
    BlockHash::from(Blake2b256Hash::create(&bytes))
}

/// Build an unsigned block, filling in its content-addressed hash (port of `unsignedBlockProto`).
#[allow(clippy::too_many_arguments)]
pub fn unsigned_block_proto(
    version: i32,
    shard_id: String,
    block_number: BlockHeight,
    sender: Validator,
    seq_num: SeqNum,
    pre_state_hash: StateHash,
    post_state_hash: StateHash,
    justifications: Vec<BlockHash>,
    bonds: BTreeMap<Validator, NonNegI64>,
    rejected_deploys: BTreeSet<Vec<u8>>,
    state: RholangState,
) -> BlockMessage {
    let block = BlockMessage {
        version,
        shard_id,
        block_hash: BlockHash::new([0u8; 32]),
        block_number,
        sender,
        seq_num,
        pre_state_hash,
        post_state_hash,
        justifications,
        bonds,
        rejected_deploys,
        rejected_blocks: BTreeSet::new(),
        rejected_senders: BTreeSet::new(),
        state,
        sig_algorithm: "secp256k1".to_string(),
        sig: Vec::new(),
    };
    let hash = hash_block(&block);
    BlockMessage {
        block_hash: hash,
        ..block
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block() -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: BlockHash::new([0u8; 32]),
            block_number: 0.try_into().unwrap(),
            sender: Validator::new([0x11; 65]),
            seq_num: 0.try_into().unwrap(),
            pre_state_hash: StateHash::new([0x01; 32]),
            post_state_hash: StateHash::new([0x02; 32]),
            justifications: vec![],
            bonds: BTreeMap::new(),
            rejected_deploys: BTreeSet::new(),
            rejected_blocks: BTreeSet::new(),
            rejected_senders: BTreeSet::new(),
            state: RholangState {
                deploys: vec![],
                system_deploys: vec![],
            },
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![],
        }
    }

    #[test]
    fn hash_block_is_deterministic_and_ignores_sig() {
        let mut a = block();
        a.sig = vec![0xaa, 0xbb];
        let mut b = block();
        b.sig = vec![0xcc];
        assert_eq!(hash_block(&a), hash_block(&b));
    }

    #[test]
    fn hash_block_changes_with_body() {
        let mut a = block();
        a.block_number = 1.try_into().unwrap();
        let b = block();
        assert_ne!(hash_block(&a), hash_block(&b));
    }

    #[test]
    fn max_block_number_folds() {
        let mk = |n: i64| BlockMetadata {
            block_num: rchain_shared::refined::BlockHeight::try_from(n).unwrap(),
            ..BlockMetadata {
                block_hash: BlockHash::new([0u8; 32]),
                block_num: rchain_shared::refined::BlockHeight::try_from(n).unwrap(),
                sender: Validator::new([0u8; 65]),
                seq_num: 0.try_into().unwrap(),
                justifications: BTreeSet::new(),
                bonds_map: BTreeMap::new(),
                validated: true,
                validation_failed: false,
                fringe: BTreeSet::new(),
                fringe_state_hash: rchain_models::block::state_hash::StateHash::new([0u8; 32]),
                member_of_fringe: None,
            }
        };
        assert_eq!(max_block_number_metadata(&[mk(3), mk(7), mk(5)]), 7);
        assert_eq!(max_block_number_metadata(&[]), -1);
    }
}
