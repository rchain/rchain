//! Block-storage syntax (port of `BlockStoreSyntax.scala`, `ApprovedStoreSyntax.scala`, and
//! `BlockDagStorageSyntax.scala`).

use std::collections::BTreeSet;

use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{BlockMessage, FinalizedFringe};
use rchain_shared::base16;

use crate::approved_store::{ApprovedStore, FINALIZED_FRINGE_KEY};
use crate::block_store::BlockStore;
use crate::dag::dag_storage::BlockDagStorage;
use crate::errors::{BlockDagInconsistencyError, BlockStoreInconsistencyError};

/// Truncated base16 for error messages (port of `PrettyPrinter.buildString(ByteString)`).
fn build_string(bytes: &[u8]) -> String {
    let hex = base16::encode(bytes);
    if hex.len() > 10 {
        format!("{}...", &hex[..10])
    } else {
        hex
    }
}

/// Get a block, raising an inconsistency error if it is absent (port of
/// `BlockStoreSyntax.getUnsafe`).
pub async fn get_block_unsafe(
    store: &BlockStore,
    hash: &BlockHash,
) -> Result<BlockMessage, BlockStoreInconsistencyError> {
    let msg = format!("BlockStore is missing hash {}", build_string(hash.as_bytes()));
    store
        .get(&[*hash])
        .await
        .map_err(BlockStoreInconsistencyError)?
        .into_iter()
        .next()
        .flatten()
        .ok_or(BlockStoreInconsistencyError(msg))
}

/// Put a block into the store (port of `BlockStoreSyntax.put`).
pub async fn put_block(store: &BlockStore, block: BlockMessage) -> Result<(), String> {
    let hash = block.block_hash;
    store.put(&[(hash, block)]).await
}

/// Get the approved block (port of `ApprovedStoreSyntax.getApprovedBlock`).
pub async fn get_approved_block(store: &ApprovedStore) -> Result<Option<FinalizedFringe>, String> {
    let results = store.get(&[FINALIZED_FRINGE_KEY]).await?;
    Ok(results.into_iter().next().flatten())
}

/// Put the approved block (port of `ApprovedStoreSyntax.putApprovedBlock`).
pub async fn put_approved_block(store: &ApprovedStore, block: FinalizedFringe) -> Result<(), String> {
    store.put(&[(FINALIZED_FRINGE_KEY, block)]).await
}

/// Look up block metadata, raising an inconsistency error if absent (port of
/// `BlockDagStorageSyntax.lookupUnsafe`).
pub async fn lookup_unsafe(
    bds: &dyn BlockDagStorage,
    hash: &BlockHash,
) -> Result<BlockMetadata, BlockDagInconsistencyError> {
    let msg = format!("DAG storage is missing hash {}", build_string(hash.as_bytes()));
    bds.lookup(hash)
        .await
        .map_err(BlockDagInconsistencyError)?
        .ok_or(BlockDagInconsistencyError(msg))
}

/// Insert a genesis block with its validated metadata (port of
/// `BlockDagStorageSyntax.insertGenesis`).
pub async fn insert_genesis(
    bds: &dyn BlockDagStorage,
    genesis_block: BlockMessage,
) -> Result<(), String> {
    let mut bmd = BlockMetadata::from_block(&genesis_block);
    bmd.validated = true;
    bmd.validation_failed = false;
    bmd.fringe = BTreeSet::new();
    bmd.fringe_state_hash = genesis_block.pre_state_hash;
    bds.insert(bmd, genesis_block).await
}
