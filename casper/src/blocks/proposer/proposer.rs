//! Block proposer (port of `blocks/proposer/Proposer.scala`).
//!
//! `Proposer.apply` builds the dependency closures from the DAG/runtime; the `proposeEffect`
//! (broadcast via `CommUtil`) is supplied by the caller.

use std::collections::BTreeSet;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::{BlockDagStorage, DeployId};
use rchain_models::block::state_hash::StateHash;
use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::BlockMessage;
use rchain_models::validator::Validator;
use rchain_shared::refined::{BlockHeight, NonNegI64};
use rchain_sdk::consensus::is_super_majority;

use super::block_creator::BlockCreator;
use super::propose_result::{BlockCreatorResult, ProposeResult, ProposeStatus};
use crate::merging::BlockIndex;
use crate::multi_parent_casper::{get_pre_state_for_new_block, ValidateError, DEPLOY_LIFESPAN};
use crate::runtime_manager::RuntimeManager;
use crate::validator_identity::ValidatorIdentity;

type BoxFuture<T> = Pin<Box<dyn Future<Output = T> + Send + 'static>>;

/// A proposal result signaled to the caller (port of `ProposerResult`).
#[derive(Clone, Debug)]
pub enum ProposerResult {
    Empty,
    Success {
        status: ProposeStatus,
        block: BlockMessage,
    },
    Failure {
        status: ProposeStatus,
        seq_number: i64,
    },
    Started {
        seq_number: i64,
    },
}

/// The block proposer (port of `Proposer`).
pub struct Proposer {
    get_latest_seq_number: Arc<dyn Fn(Validator) -> BoxFuture<i64> + Send + Sync>,
    check_active_validator: Arc<dyn Fn(&ValidatorIdentity) -> BoxFuture<bool> + Send + Sync>,
    create_block:
        Arc<dyn Fn(&ValidatorIdentity) -> BoxFuture<Result<BlockCreatorResult, String>> + Send + Sync>,
    validate_block:
        Arc<dyn Fn(&BlockMessage) -> BoxFuture<Result<(), ValidateError>> + Send + Sync>,
    propose_effect: Arc<dyn Fn(&BlockMessage) -> BoxFuture<()> + Send + Sync>,
    validator: ValidatorIdentity,
}

impl Proposer {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        get_latest_seq_number: Arc<dyn Fn(Validator) -> BoxFuture<i64> + Send + Sync>,
        check_active_validator: Arc<dyn Fn(&ValidatorIdentity) -> BoxFuture<bool> + Send + Sync>,
        create_block: Arc<
            dyn Fn(&ValidatorIdentity) -> BoxFuture<Result<BlockCreatorResult, String>> + Send + Sync,
        >,
        validate_block: Arc<
            dyn Fn(&BlockMessage) -> BoxFuture<Result<(), ValidateError>> + Send + Sync,
        >,
        propose_effect: Arc<dyn Fn(&BlockMessage) -> BoxFuture<()> + Send + Sync>,
        validator: ValidatorIdentity,
    ) -> Self {
        Proposer {
            get_latest_seq_number,
            check_active_validator,
            create_block,
            validate_block,
            propose_effect,
            validator,
        }
    }

    async fn do_propose(&self) -> Result<(ProposeResult, Option<BlockMessage>), String> {
        if !(self.check_active_validator)(&self.validator).await {
            return Ok((
                ProposeResult {
                    propose_status: ProposeStatus::NotBonded,
                },
                None,
            ));
        }

        match (self.create_block)(&self.validator).await? {
            BlockCreatorResult::NoNewDeploys => Ok((
                ProposeResult {
                    propose_status: ProposeStatus::NoNewDeploys,
                },
                None,
            )),
            BlockCreatorResult::Created(block) => match (self.validate_block)(&block).await {
                Ok(()) => {
                    (self.propose_effect)(&block).await;
                    Ok((
                        ProposeResult {
                            propose_status: ProposeStatus::ProposeSuccess,
                        },
                        Some(block),
                    ))
                }
                Err(ValidateError::ValidationFailed(_, status)) => Err(format!(
                    "Validation of self created block failed with reason: {status:?}, cancelling propose."
                )),
                Err(ValidateError::Internal(e)) => Err(format!(
                    "Validation of self created block failed with internal error: {e}, cancelling propose."
                )),
            },
        }
    }

    pub async fn propose(
        &self,
        is_async: bool,
        propose_id: tokio::sync::oneshot::Sender<ProposerResult>,
    ) -> Result<(ProposeResult, Option<BlockMessage>), String> {
        let validator = Validator::from_slice(self.validator.public_key.bytes());
        let next_seq = (self.get_latest_seq_number)(validator).await + 1;

        if is_async {
            let _ = propose_id.send(ProposerResult::Started {
                seq_number: next_seq,
            });
            self.do_propose().await
        } else {
            let result = self.do_propose().await;
            let proposer_result = match &result {
                Ok((result, Some(block))) => ProposerResult::Success {
                    status: result.propose_status.clone(),
                    block: block.clone(),
                },
                Ok((result, None)) => ProposerResult::Failure {
                    status: result.propose_status.clone(),
                    seq_number: next_seq,
                },
                Err(_) => ProposerResult::Failure {
                    status: ProposeStatus::BugError,
                    seq_number: next_seq,
                },
            };
            let _ = propose_id.send(proposer_result);
            result
        }
    }

    /// Build a `Proposer` from its dependencies (port of `Proposer.apply`). The DAG/block-store/
    /// runtime/block-index are captured by `Arc` so the returned proposer is `'static` and can be
    /// driven from a spawned task.
    #[allow(clippy::too_many_arguments)]
    pub fn apply<F, Fut>(
        validator_identity: ValidatorIdentity,
        shard_id: String,
        min_phlo_price: i64,
        epoch_length: i32,
        dag: Arc<dyn BlockDagStorage>,
        block_store: BlockStore,
        runtime: Arc<RuntimeManager>,
        block_index: F,
        propose_effect: Arc<dyn Fn(&BlockMessage) -> BoxFuture<()> + Send + Sync>,
    ) -> Proposer
    where
        F: Fn(BlockHash) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<BlockIndex, String>> + Send + 'static,
    {
        let block_index = Arc::new(block_index);

        let get_latest_seq_number: Arc<dyn Fn(Validator) -> BoxFuture<i64> + Send + Sync> = {
            let dag = dag.clone();
            Arc::new(move |sender| {
                let dag = dag.clone();
                Box::pin(async move {
                    let dag_repr = dag.get_representation().await;
                    dag_repr
                        .dag_message_state
                        .latest_msgs
                        .get(&sender)
                        .map(|m| i64::from(m.sender_seq))
                        .unwrap_or(-1)
                })
            })
        };

        let check_active_validator: Arc<dyn Fn(&ValidatorIdentity) -> BoxFuture<bool> + Send + Sync> =
            {
                let dag = dag.clone();
                Arc::new(move |vi: &ValidatorIdentity| {
                    let sender = Validator::from_slice(vi.public_key.bytes());
                    let dag = dag.clone();
                    Box::pin(async move {
                        let dag_repr = dag.get_representation().await;
                        let fringe = dag_repr.dag_message_state.latest_fringe();
                        let bonds_map = if let Some(m) = fringe.iter().next() {
                            m.bonds_map.clone()
                        } else if let Some((_, hashes)) = dag_repr.height_map.iter().next() {
                            match hashes.iter().next() {
                                Some(h) => dag
                                    .lookup(h)
                                    .await
                                    .ok()
                                    .flatten()
                                    .map(|m| m.bonds_map)
                                    .unwrap_or_default(),
                                None => Default::default(),
                            }
                        } else {
                            Default::default()
                        };
                        bonds_map.contains_key(&sender)
                    })
                })
            };

        let create_block: Arc<
            dyn Fn(&ValidatorIdentity) -> BoxFuture<Result<BlockCreatorResult, String>> + Send + Sync,
        > = {
            let runtime = runtime.clone();
            let dag = dag.clone();
            let block_store = block_store.clone();
            let block_index = block_index.clone();
            let shard_id = shard_id.clone();
            Arc::new(move |vi: &ValidatorIdentity| {
                let runtime = runtime.clone();
                let dag = dag.clone();
                let block_store = block_store.clone();
                let block_index = block_index.clone();
                let vi = vi.clone();
                let shard_id = shard_id.clone();
                Box::pin(async move {
                    create_block(
                        runtime.as_ref(),
                        dag.as_ref(),
                        &block_store,
                        block_index.as_ref(),
                        &vi,
                        &shard_id,
                        epoch_length,
                    )
                    .await
                })
            })
        };

        let validate_block: Arc<dyn Fn(&BlockMessage) -> BoxFuture<Result<(), ValidateError>> + Send + Sync> =
            {
                let runtime = runtime.clone();
                let dag = dag.clone();
                let block_store = block_store.clone();
                let block_index = block_index.clone();
                let shard_id = shard_id.clone();
                Arc::new(move |block: &BlockMessage| {
                    let runtime = runtime.clone();
                    let dag = dag.clone();
                    let block_store = block_store.clone();
                    let block_index = block_index.clone();
                    let block = block.clone();
                    let shard_id = shard_id.clone();
                    Box::pin(async move {
                        match crate::multi_parent_casper::validate(
                            dag.as_ref(),
                            &block_store,
                            runtime.as_ref(),
                            &block,
                            &shard_id,
                            min_phlo_price,
                            block_index.as_ref(),
                        )
                        .await
                        {
                            Ok(meta) => {
                                dag.insert(meta, block.clone())
                                    .await
                                    .map_err(|e| {
                                        ValidateError::Internal(format!(
                                            "failed to insert block into DAG: {e}"
                                        ))
                                    })?;
                                Ok(())
                            }
                            Err(err) => Err(err),
                        }
                    })
                })
            };

        Proposer::new(
            get_latest_seq_number,
            check_active_validator,
            create_block,
            validate_block,
            propose_effect,
            validator_identity,
        )
    }
}

async fn get_block(block_store: &BlockStore, hash: &BlockHash) -> Result<Option<BlockMessage>, String> {
    let mut vals = block_store.get(&[*hash]).await?;
    Ok(vals.pop().flatten())
}

#[allow(clippy::too_many_arguments)]
async fn create_block<'a, F, Fut>(
    runtime: &'a RuntimeManager,
    dag: &'a dyn BlockDagStorage,
    block_store: &'a BlockStore,
    block_index: &'a F,
    validator_identity: &ValidatorIdentity,
    shard_id: &str,
    epoch_length: i32,
) -> Result<BlockCreatorResult, String>
where
    F: Fn(BlockHash) -> Fut + Sync,
    Fut: Future<Output = Result<BlockIndex, String>>,
{
    let pre_state = get_pre_state_for_new_block(dag, block_store, runtime, block_index).await?;
    let pre_state_hash = pre_state.pre_state_hash;
    let creators_validator = Validator::from_slice(validator_identity.public_key.bytes());
    let next_block_num = pre_state
        .justifications
        .iter()
        .map(|m| m.block_num)
        .max()
        .map(|m| m + NonNegI64::one())
        .unwrap_or_else(BlockHeight::zero);
    let parent_hashes: Vec<BlockHash> =
        pre_state.justifications.iter().map(|m| m.block_hash).collect();
    let offenders: BTreeSet<Validator> = pre_state
        .justifications
        .iter()
        .filter(|m| m.validation_failed)
        .map(|m| m.sender)
        .collect();

    let pre_state_bonds = runtime
        .compute_bonds(&StateHash::from_slice(pre_state_hash.as_bytes()))
        .await?;
    let bonded: BTreeSet<Validator> = pre_state_bonds
        .iter()
        .filter(|(_, b)| i64::from(**b) > 0)
        .map(|(v, _)| *v)
        .collect();
    let to_slash: BTreeSet<Validator> = offenders.intersection(&bonded).copied().collect();

    let change_epoch =
        i64::from(next_block_num) != 0 && epoch_length as i64 % i64::from(next_block_num) == 0;

    // Attestation suppression: no new state transitions, or not yet a super-majority.
    let dag_repr = dag.get_representation().await;
    let seen = |h: &BlockHash| {
        dag_repr
            .dag_message_state
            .msg_map
            .get(h)
            .map(|m| m.seen.clone())
            .unwrap_or_default()
    };
    let parent_seen: BTreeSet<BlockHash> = parent_hashes.iter().flat_map(|h| seen(h)).collect();
    let fringe_seen: BTreeSet<BlockHash> = pre_state.fringe.iter().flat_map(|h| seen(h)).collect();
    let conflict_set: Vec<BlockHash> = parent_seen.difference(&fringe_seen).copied().collect();

    let has_deploys =
        |b: &BlockMessage| !b.state.system_deploys.is_empty() || !b.state.deploys.is_empty();

    let mut nothing_to_finalize = true;
    for h in &conflict_set {
        if let Some(b) = get_block(block_store, h).await? {
            if has_deploys(&b) {
                nothing_to_finalize = false;
                break;
            }
        }
    }

    let creators_latest = pre_state
        .justifications
        .iter()
        .find(|m| m.sender == creators_validator);
    let newly_seen: BTreeSet<BlockHash> = match creators_latest {
        Some(m) => m
            .justifications
            .iter()
            .flat_map(|h| seen(h))
            .collect::<BTreeSet<_>>()
            .difference(&parent_seen)
            .copied()
            .collect(),
        None => BTreeSet::new(),
    };
    let mut new_blocks = Vec::new();
    for h in &newly_seen {
        if let Some(b) = get_block(block_store, h).await? {
            new_blocks.push(b);
        }
    }
    let new_state_transition = new_blocks.iter().any(|b| has_deploys(b));
    let new_senders: BTreeSet<Validator> = new_blocks.iter().map(|b| b.sender).collect();
    let attestation_stake: i128 = pre_state_bonds
        .iter()
        .filter(|(v, _)| new_senders.contains(v))
        .map(|(_, s)| i128::from(i64::from(*s)))
        .sum();
    let pre_state_bonds_stake: i128 = pre_state_bonds
        .values()
        .map(|s| i128::from(i64::from(*s)))
        .sum();
    let waiting_for_supermajority =
        !(new_state_transition || is_super_majority(attestation_stake, pre_state_bonds_stake));

    let suppress_attestation = nothing_to_finalize || waiting_for_supermajority;

    // User deploys: filter future / expired / replayed.
    let pooled = dag.pooled_deploys().await?;
    let mut deploys: Vec<DeployId> = Vec::new();
    for (id, d) in pooled {
        let future = d.data.valid_after_block_number > i64::from(next_block_num);
        let expired = d.data.valid_after_block_number < next_block_num - DEPLOY_LIFESPAN;
        let replay_attack = dag.lookup_by_deploy_id(&id).await?.is_some();
        if !(future || expired || replay_attack) {
            deploys.push(id);
        }
    }

    BlockCreator {
        id: validator_identity.clone(),
        shard_id: shard_id.to_string(),
    }
    .create(
        runtime,
        dag,
        &pre_state,
        &deploys,
        &to_slash,
        change_epoch,
        suppress_attestation,
    )
    .await
}

#[cfg(test)]
mod tests {
    use super::*;

    fn proposer(create: BlockCreatorResult) -> Proposer {
        let get_seq: Arc<dyn Fn(Validator) -> BoxFuture<i64> + Send + Sync> =
            Arc::new(|_v| Box::pin(async { 0i64 }));
        let check_active: Arc<dyn Fn(&ValidatorIdentity) -> BoxFuture<bool> + Send + Sync> =
            Arc::new(|_v| Box::pin(async { true }));
        let create_block: Arc<
            dyn Fn(&ValidatorIdentity) -> BoxFuture<Result<BlockCreatorResult, String>> + Send + Sync,
        > = Arc::new(move |_v| {
            let create = create.clone();
            Box::pin(async move { Ok(create) })
        });
        let validate: Arc<
            dyn Fn(&BlockMessage) -> BoxFuture<Result<(), ValidateError>> + Send + Sync,
        > = Arc::new(|_b| Box::pin(async { Ok(()) }));
        let effect: Arc<dyn Fn(&BlockMessage) -> BoxFuture<()> + Send + Sync> =
            Arc::new(|_b| Box::pin(async {}));
        let validator = ValidatorIdentity::from_hex(
            "67e56582298859ddae725f972992a07c6c4fb9f62a8fff58ce3ca926a1063530",
        )
        .unwrap();
        Proposer::new(get_seq, check_active, create_block, validate, effect, validator)
    }

    fn block() -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: BlockHash::new([1u8; 32]),
            block_number: 0.try_into().unwrap(),
            sender: Validator::new([0u8; 65]),
            seq_num: 0.try_into().unwrap(),
            pre_state_hash: StateHash::new([0u8; 32]),
            post_state_hash: StateHash::new([0u8; 32]),
            justifications: vec![],
            bonds: std::collections::BTreeMap::new(),
            rejected_deploys: std::collections::BTreeSet::new(),
            rejected_blocks: std::collections::BTreeSet::new(),
            rejected_senders: std::collections::BTreeSet::new(),
            state: rchain_models::casper::protocol::casper_message::RholangState::default(),
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![],
        }
    }

    #[tokio::test]
    async fn no_new_deploys_returns_failure() {
        let p = proposer(BlockCreatorResult::NoNewDeploys);
        let (tx, rx) = tokio::sync::oneshot::channel();
        let (result, block_opt) = p.propose(false, tx).await.unwrap();
        assert_eq!(result.propose_status, ProposeStatus::NoNewDeploys);
        assert!(block_opt.is_none());
        assert!(matches!(rx.await.unwrap(), ProposerResult::Failure { .. }));
    }

    #[tokio::test]
    async fn created_block_returns_success() {
        let p = proposer(BlockCreatorResult::Created(block()));
        let (tx, rx) = tokio::sync::oneshot::channel();
        let (result, block_opt) = p.propose(false, tx).await.unwrap();
        assert_eq!(result.propose_status, ProposeStatus::ProposeSuccess);
        assert!(block_opt.is_some());
        assert!(matches!(rx.await.unwrap(), ProposerResult::Success { .. }));
    }
}
