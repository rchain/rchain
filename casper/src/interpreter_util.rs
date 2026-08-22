//! Interpreter utilities (port of `rholang/InterpreterUtil.scala`).

use std::collections::{BTreeMap, BTreeSet};

use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::ast::Par;
use rchain_models::block::state_hash::StateHash;
use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{BlockMessage, SignedDeployData};
use rchain_rholang::errors::RholangError;
use rchain_rholang::system_processes::BlockData;
use rchain_shared::base16;

use crate::block_random_seed::BlockRandomSeed;
use crate::block_status::BlockStatus;
use crate::merging::{BlockIndex, ParentsMergedState};
use crate::multi_parent_casper::get_pre_state_for_parents;
use crate::rholang::{ReplayFailure, SystemDeployRuntimeResult, UserDeployRuntimeResult};
use crate::runtime_manager::RuntimeManager;
use crate::system_deploy::SystemDeploy;

/// Parse + normalize a rholang term (port of `mkTerm`).
pub fn mk_term(rho: &str, env: &BTreeMap<String, Par>) -> Result<Par, RholangError> {
    rchain_rholang::normalizer::source_to_adt_with_env(rho, env).map(Par::from)
}

/// Replay a block's deploys and return the computed state hash (port of `replayBlock`).
pub async fn replay_block(
    runtime: &RuntimeManager,
    block: &BlockMessage,
    rand: &Blake2b512Random,
) -> Result<Blake2b256Hash, ReplayFailure> {
    let start_hash = Blake2b256Hash::from_byte_array(block.pre_state_hash.as_bytes());
    let block_data = BlockData::from_block(block);
    let with_cost_accounting = !block.justifications.is_empty();
    let (state_hash, _mergeable) = runtime
        .replay_compute_state(
            &start_hash,
            &block.state.deploys,
            &block.state.system_deploys,
            rand,
            block_data,
            with_cost_accounting,
            &block.bonds,
        )
        .await?;
    Ok(state_hash)
}

/// Map a replay result into an `Option` of the matching state hash (port of `handleErrors`).
pub fn handle_errors(
    ts_hash: &Blake2b256Hash,
    result: Result<Blake2b256Hash, ReplayFailure>,
) -> Result<Option<Blake2b256Hash>, String> {
    match result {
        Ok(computed) => {
            if *ts_hash == computed {
                Ok(Some(computed))
            } else {
                Ok(None)
            }
        }
        Err(ReplayFailure::InternalError(cause)) => Err(format!(
            "Internal errors encountered while processing deploy: {cause}"
        )),
        Err(_) => Ok(None),
    }
}

/// Compute the post-state + processed deploys from a deploy sequence (port of
/// `computeDeploysCheckpoint`).
#[allow(clippy::too_many_arguments)]
pub async fn compute_deploys_checkpoint(
    runtime: &RuntimeManager,
    deploys: &[SignedDeployData],
    system_deploys: &[SystemDeploy],
    rand: &Blake2b512Random,
    block_data: BlockData,
    pre_state_hash: &Blake2b256Hash,
) -> Result<
    (
        Blake2b256Hash,
        Vec<UserDeployRuntimeResult>,
        Vec<SystemDeployRuntimeResult>,
    ),
    String,
> {
    runtime
        .compute_state(pre_state_hash, deploys, system_deploys, rand, block_data)
        .await
}

/// The hard-coded empty-state (genesis pre-state) hash (port of `emptyStateHashFixed`).
///
/// Recomputed after the native system-process install + de-blessed genesis (the empty state is now
/// "system processes installed + empty native state", with no registry-bootstrap echo).
pub fn empty_state_hash_fixed() -> Blake2b256Hash {
    Blake2b256Hash::from_byte_array(&base16::unsafe_decode(
        "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
    ))
}

/// Validate a block by recomputing its pre-state and replaying its deploys (port of
/// `validateBlockCheckpoint`). Returns the block metadata plus a `bool` (valid) / `BlockStatus`
/// (rejectable) outcome.
pub async fn validate_block_checkpoint<F, Fut>(
    runtime: &RuntimeManager,
    dag: &dyn BlockDagStorage,
    block_store: &BlockStore,
    block: &BlockMessage,
    block_index: &F,
) -> Result<(BlockMetadata, Result<bool, BlockStatus>), String>
where
    F: Fn(BlockHash) -> Fut,
    Fut: std::future::Future<Output = Result<BlockIndex, String>>,
{
    // Non-failed parent hashes.
    let mut parents: Vec<BlockHash> = Vec::new();
    for j in &block.justifications {
        if let Some(meta) = dag.lookup(j).await? {
            if !meta.validation_failed {
                parents.push(meta.block_hash);
            }
        }
    }
    let parents_set: BTreeSet<BlockHash> = parents.iter().copied().collect();

    let pre_state = if !parents_set.is_empty() {
        get_pre_state_for_parents(dag, block_store, runtime, &parents_set, block_index).await?
    } else {
        // Genesis block: no parents.
        let genesis_pre_state_hash = empty_state_hash_fixed();
        ParentsMergedState {
            justifications: Vec::new(),
            max_block_num: 0,
            max_seq_nums: BTreeMap::from([(block.sender, 0)]),
            fringe: BTreeSet::new(),
            fringe_state: genesis_pre_state_hash,
            fringe_bonds_map: block.bonds.clone(),
            fringe_rejected_deploys: BTreeSet::new(),
            pre_state_hash: genesis_pre_state_hash,
            rejected_deploys: BTreeSet::new(),
        }
    };

    let incoming_pre_state_hash = Blake2b256Hash::from_byte_array(block.pre_state_hash.as_bytes());
    let result: Result<bool, BlockStatus> = if incoming_pre_state_hash != pre_state.pre_state_hash {
        Ok(false)
    } else if pre_state.fringe_rejected_deploys != block.rejected_deploys {
        Err(BlockStatus::InvalidRejectedDeploy)
    } else {
        let rand = BlockRandomSeed::random_generator_from_block(block);
        let post_state_hash = Blake2b256Hash::from_byte_array(block.post_state_hash.as_bytes());
        let replay_result = replay_block(runtime, block, &rand).await;
        let handled = handle_errors(&post_state_hash, replay_result)?;
        Ok(handled.is_some())
    };

    let validation_failed = match &result {
        Err(_) => true,
        Ok(valid) => !*valid,
    };
    let bmd = BlockMetadata {
        validated: true,
        validation_failed,
        fringe: pre_state.fringe,
        fringe_state_hash: StateHash::from_slice(pre_state.fringe_state.as_bytes()),
        ..BlockMetadata::from_block(block)
    };

    Ok((bmd, result))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hash(byte: u8) -> Blake2b256Hash {
        Blake2b256Hash::from_bytes([byte; 32])
    }

    #[test]
    fn handle_errors_accepts_matching_hash() {
        assert_eq!(
            handle_errors(&hash(1), Ok(hash(1))).unwrap(),
            Some(hash(1))
        );
    }

    #[test]
    fn handle_errors_rejects_mismatching_hash() {
        assert_eq!(handle_errors(&hash(1), Ok(hash(2))).unwrap(), None);
    }

    #[test]
    fn handle_errors_raises_internal_error() {
        let r = handle_errors(&hash(1), Err(ReplayFailure::internal_error("boom")));
        assert!(r.is_err());
    }

    #[test]
    fn handle_errors_soft_fails_on_replay_status_mismatch() {
        let r = handle_errors(
            &hash(1),
            Err(ReplayFailure::replay_status_mismatch(true, false)),
        );
        assert_eq!(r.unwrap(), None);
    }
}
