//! The multi-parent CBC-Casper façade (port of `MultiParentCasper.scala`).

use std::collections::{BTreeMap, BTreeSet};

use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::{BlockDagStorage, DeployId};
use rchain_block_storage::dag::finalizer::{Finalizer, Message};
use rchain_block_storage::dag::message_map;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_models::block::state_hash::StateHash;
use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{BlockMessage, SignedDeployData};
use rchain_models::normalizer_env::NormalizerEnv;
use rchain_models::validator::Validator;

use crate::block_status::BlockStatus;
use crate::interpreter_util::validate_block_checkpoint;
use crate::merging::{BlockIndex, DeployChainIndex, MergeScope, ParentsMergedState};
use crate::runtime_manager::RuntimeManager;

/// A deploy-parsing error (port of `ParsingError`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsingError(pub String);

/// A block-validation failure (port of `MultiParentCasper.validate`'s error channel).
///
/// A validation failure marks the block invalid but still returns its (failed) metadata so the DAG
/// can record it; an internal error is a store/runtime lookup failure with no block outcome.
#[derive(Clone, Debug)]
pub enum ValidateError {
    ValidationFailed(BlockMetadata, BlockStatus),
    Internal(String),
}

/// The size of the deploy safety range (port of `deployLifespan`).
pub const DEPLOY_LIFESPAN: i64 = 50;

/// Build a `ParsingError` from details (port of `parsingError`).
pub fn parsing_error(details: impl Into<String>) -> ParsingError {
    ParsingError(format!("Parsing error: {}", details.into()))
}

/// Look up the last finalized block (port of `lastFinalizedBlock`).
pub async fn last_finalized_block(
    dag: &dyn BlockDagStorage,
    block_store: &BlockStore,
) -> Result<BlockMessage, String> {
    let repr = dag.get_representation().await;
    let hash = repr
        .last_finalized_block_hash()
        .ok_or_else(|| "no finalized block in the DAG".to_string())?;
    let mut vals = block_store.get(&[hash]).await?;
    vals.pop()
        .flatten()
        .ok_or_else(|| format!("missing finalized block {}", hash.to_hex()))
}

/// Add a deploy to the deploy pool and return its id (port of `addDeploy`).
pub async fn add_deploy(
    dag: &dyn BlockDagStorage,
    deploy: &SignedDeployData,
) -> Result<DeployId, String> {
    dag.add_deploy(deploy.clone()).await?;
    Ok(deploy.sig.clone())
}

/// Parse-check a deploy term, then add the deploy to the pool (port of `deploy`).
pub async fn deploy(
    dag: &dyn BlockDagStorage,
    deploy: &SignedDeployData,
) -> Result<DeployId, ParsingError> {
    // Normalize against the deploy's environment (deployer id / deploy id), so a term that
    // references `rho:rchain:deployerId`/`deployId` parses the same way it will when processed.
    let normalizer_env = NormalizerEnv::new(deploy);
    rchain_rholang::normalizer::source_to_adt_with_env(&deploy.data.term, normalizer_env.to_env())
        .map_err(|e| parsing_error(format!("Error in parsing term: \n{e}")))?;
    add_deploy(dag, deploy)
        .await
        .map_err(parsing_error)
}

async fn get_block_unsafe(block_store: &BlockStore, hash: &BlockHash) -> Result<BlockMessage, String> {
    let mut vals = block_store.get(&[*hash]).await?;
    vals.pop()
        .flatten()
        .ok_or_else(|| format!("missing block {}", hash.to_hex()))
}

/// Compute the merged pre-state for a set of parent blocks (port of `getPreStateForParents`).
pub async fn get_pre_state_for_parents<F, Fut>(
    dag: &dyn BlockDagStorage,
    block_store: &BlockStore,
    runtime: &RuntimeManager,
    parent_hashes: &BTreeSet<BlockHash>,
    block_index: &F,
) -> Result<ParentsMergedState, String>
where
    F: Fn(BlockHash) -> Fut,
    Fut: std::future::Future<Output = Result<BlockIndex, String>>,
{
    if parent_hashes.is_empty() {
        return Err(
            "Parents must not be empty to calculate pre-state. Genesis block pre-state is loaded from config."
                .to_string(),
        );
    }

    let dag_repr = dag.get_representation().await;
    let msg_map = &dag_repr.dag_message_state.msg_map;

    let mut justifications: Vec<BlockMetadata> = Vec::new();
    for h in parent_hashes {
        let meta = dag
            .lookup(h)
            .await?
            .ok_or_else(|| format!("missing justification {}", h.to_hex()))?;
        justifications.push(meta);
    }

    let parents: BTreeSet<Message<BlockHash, Validator>> = parent_hashes
        .iter()
        .map(|h| {
            msg_map
                .get(h)
                .cloned()
                .ok_or_else(|| format!("parent not in message map: {}", h.to_hex()))
        })
        .collect::<Result<_, String>>()?;

    // Currently finalized fringe.
    let prev_fringe = message_map::latest_fringe(msg_map, &parents);
    let prev_fringe_hashes: BTreeSet<BlockHash> = prev_fringe.iter().map(|m| m.id).collect();
    let fringe_record = dag_repr
        .fringe_states
        .get(&prev_fringe_hashes)
        .ok_or_else(|| {
            format!(
                "Fringe state not available in state cache, fringe: {:?}",
                prev_fringe_hashes
            )
        })?;
    let prev_fringe_state = fringe_record.state_hash;
    let prev_fringe_rejected_deploys = fringe_record.rejected_deploys.clone();

    // Bonds map: from the latest justification for an empty fringe, else from the PoS contract.
    let bonds_map = if prev_fringe.is_empty() {
        // When the fringe is empty (genesis), the bonds map is taken from the justifications.
        // They must all agree — a forged or disagreeing bonds map would otherwise be silently
        // picked from an arbitrary justification (M-1).
        let mut iter = justifications.iter().map(|j| j.bonds_map.clone());
        let first = iter.next().unwrap_or_default();
        for other in iter {
            if other != first {
                return Err("justifications disagree on the bonds map".to_string());
            }
        }
        first
    } else {
        let state_hash = StateHash::from_slice(prev_fringe_state.as_bytes());
        runtime.compute_bonds(&state_hash).await?
    };

    // If a new fringe is finalized, merge it.
    let finalizer = Finalizer::new(msg_map);
    let (_parent_fringe, new_fringe_opt) = finalizer.calculate_finalization(&parents, &bonds_map);
    let new_fringe_hashes: Option<BTreeSet<BlockHash>> =
        new_fringe_opt.map(|f| f.iter().map(|m| m.id).collect());

    let new_fringe_result = match &new_fringe_hashes {
        Some(fringe) => {
            let (m_scope, base_opt) =
                MergeScope::from_dag(fringe, &prev_fringe_hashes, &dag_repr.child_map, msg_map)?;
            let base_state = match base_opt {
                Some(h) => {
                    Blake2b256Hash::from_byte_array(
                        get_block_unsafe(block_store, &h).await?.post_state_hash.as_bytes(),
                    )
                }
                None => prev_fringe_state,
            };
            let result = MergeScope::merge(
                &m_scope,
                base_state,
                &dag_repr.fringe_states,
                runtime.get_history_repo(),
                block_index,
                DeployChainIndex::deploy_chain_cost,
            )
            .await?;
            Some(result)
        }
        None => None,
    };
    let (fringe_state, fringe_rejected_deploys) =
        new_fringe_result.unwrap_or((prev_fringe_state, prev_fringe_rejected_deploys));

    let max_height = justifications
        .iter()
        .map(|m| i64::from(m.block_num))
        .max()
        .unwrap_or(-1);
    let max_seq_nums: BTreeMap<Validator, i64> =
        justifications.iter().map(|m| (m.sender, i64::from(m.seq_num))).collect();
    let new_fringe = new_fringe_hashes.unwrap_or(prev_fringe_hashes);

    // Merge the conflict scope (non-finalized blocks above the fringe).
    let (pre_state_hash, cs_rejected_deploys) = if parent_hashes.len() == 1 {
        let parent = parent_hashes
            .iter()
            .next()
            .ok_or_else(|| "expected one parent".to_string())?;
        let block = get_block_unsafe(block_store, parent).await?;
        (
            Blake2b256Hash::from_byte_array(block.post_state_hash.as_bytes()),
            BTreeSet::new(),
        )
    } else {
        let (m_scope, base_opt) =
            MergeScope::from_dag(parent_hashes, &new_fringe, &dag_repr.child_map, msg_map)?;
        let base_state = match base_opt {
            Some(h) => {
                Blake2b256Hash::from_byte_array(
                    get_block_unsafe(block_store, &h).await?.post_state_hash.as_bytes(),
                )
            }
            None => fringe_state,
        };
        MergeScope::merge(
            &m_scope,
            base_state,
            &dag_repr.fringe_states,
            runtime.get_history_repo(),
            block_index,
            DeployChainIndex::deploy_chain_cost,
        )
        .await?
    };

    Ok(ParentsMergedState {
        justifications,
        max_block_num: max_height,
        max_seq_nums,
        fringe: new_fringe,
        fringe_state,
        fringe_bonds_map: bonds_map,
        fringe_rejected_deploys,
        pre_state_hash,
        rejected_deploys: cs_rejected_deploys,
    })
}

/// Compute the pre-state for a new block from the DAG's latest messages (port of
/// `getPreStateForNewBlock`).
pub async fn get_pre_state_for_new_block<F, Fut>(
    dag: &dyn BlockDagStorage,
    block_store: &BlockStore,
    runtime: &RuntimeManager,
    block_index: &F,
) -> Result<ParentsMergedState, String>
where
    F: Fn(BlockHash) -> Fut,
    Fut: std::future::Future<Output = Result<BlockIndex, String>>,
{
    let dag_repr = dag.get_representation().await;
    let parent_hashes: BTreeSet<BlockHash> = dag_repr
        .dag_message_state
        .latest_msgs
        .values()
        .map(|m| m.id)
        .collect();
    get_pre_state_for_parents(dag, block_store, runtime, &parent_hashes, block_index).await
}

/// Validate a block: block summary, replay checkpoint, bonds cache, neglected-invalid-block, and
/// phlo price (port of `MultiParentCasper.validate`).
pub async fn validate<F, Fut>(
    dag: &dyn BlockDagStorage,
    block_store: &BlockStore,
    runtime: &RuntimeManager,
    block: &BlockMessage,
    shard_id: &str,
    min_phlo_price: i64,
    block_index: &F,
) -> Result<BlockMetadata, ValidateError>
where
    F: Fn(BlockHash) -> Fut,
    Fut: std::future::Future<Output = Result<BlockIndex, String>>,
{
    let init_block_meta = BlockMetadata::from_block(block);

    // Block summary (justification regression, sequence/block number, deploy checks).
    match crate::validate::block_summary(dag, block_store, block, shard_id, DEPLOY_LIFESPAN).await {
        Ok(Ok(())) => {}
        Ok(Err(status)) => {
            return Err(ValidateError::ValidationFailed(
                mark_failed(&init_block_meta),
                status,
            ))
        }
        Err(e) => return Err(ValidateError::Internal(format!("block summary failed: {e}"))),
    }

    // Replay validation.
    let (block_metadata, validated) =
        validate_block_checkpoint(runtime, dag, block_store, block, block_index)
            .await
            .map_err(|e| ValidateError::Internal(format!("validateBlockCheckpoint failed: {e}")))?;
    match validated {
        Err(status) => {
            return Err(ValidateError::ValidationFailed(
                mark_failed(&block_metadata),
                status,
            ))
        }
        Ok(true) => {}
        Ok(false) => {
            return Err(ValidateError::ValidationFailed(
                mark_failed(&block_metadata),
                BlockStatus::InvalidStateHash,
            ))
        }
    }

    // Bonds cache.
    match crate::validate::bonds_cache(runtime, block).await {
        Ok(Ok(())) => {}
        Ok(Err(status)) => {
            return Err(ValidateError::ValidationFailed(
                mark_failed(&block_metadata),
                status,
            ))
        }
        Err(e) => return Err(ValidateError::Internal(format!("bondsCache failed: {e}"))),
    }

    // Neglected invalid block.
    match crate::validate::neglected_invalid_block(dag, block).await {
        Ok(Ok(())) => {}
        Ok(Err(status)) => {
            return Err(ValidateError::ValidationFailed(
                mark_failed(&block_metadata),
                status,
            ))
        }
        Err(e) => return Err(ValidateError::Internal(format!("neglectedInvalidBlock failed: {e}"))),
    }

    // Phlo price (best-effort; treated as valid on failure, per the Scala recoverWith).
    let _ = crate::validate::phlo_price(block, min_phlo_price);

    // Build/cache the block index.
    let _ = BlockIndex::get_block_index(runtime, block_store, block.block_hash).await;

    Ok(block_metadata)
}

fn mark_failed(meta: &BlockMetadata) -> BlockMetadata {
    BlockMetadata {
        validated: true,
        validation_failed: true,
        ..meta.clone()
    }
}
