//! Casper merge index data structures (Law 17: merge determinism).
//!
//! Ports the pure data types, conflict/dependency relations, and the effectful constructors
//! (`DeployChainIndex.apply`, `BlockIndex.apply`, `MergeScope.merge`) from `casper/.../merging/`.

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::hash::{Hash, Hasher};
use std::sync::{Mutex, OnceLock};

use rchain_block_storage::block_store::BlockStore;
use rchain_shared::refined::NonNegI64;
use rchain_block_storage::dag::finalizer::Message;
use rchain_block_storage::dag::message_map;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{
    BlockMessage, Event, ProcessedDeploy, ProcessedSystemDeploy, SystemDeployData,
};
use rchain_models::fringe_data::FringeData;
use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_models::validator::Validator;
use rchain_rholang::merging::{calculate_number_channel_merge, read_mergeable_values};
use rchain_rholang::storage::RhoHistoryRepository;
use rchain_rspace::history::history_repository::HistoryRepository;
use rchain_rspace::hot_store_trie_action::HotStoreTrieAction;
use rchain_rspace::merger::event_log_index::{EventLogIndex, NumberChannelsDiff};
use rchain_rspace::merger::event_log_merging_logic::{are_conflicting, depends};
use rchain_rspace::merger::state_change::StateChange;
use rchain_rspace::merger::state_change_merger::compute_trie_actions;
use rchain_rspace::trace::event::{Event as REvent, Produce};
use rchain_sdk::dag::merging::{
    compute_dependency_map, compute_greedy_non_intersecting_branches,
    compute_relation_map_for_merge_set, resolve_conflict_set,
};
use rchain_shared::serialize::Serialize;

use crate::event_converter::to_rspace_event;
use crate::runtime_manager::RuntimeManager;

/// A deploy id paired with its execution cost (port of `DeployIdWithCost`).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeployIdWithCost {
    pub id: Vec<u8>,
    pub cost: i64,
}

/// The index of a single deploy (port of `DeployIndex`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeployIndex {
    pub deploy_id: Vec<u8>,
    pub cost: i64,
    pub event_log_index: EventLogIndex,
}

impl Ord for DeployIndex {
    fn cmp(&self, other: &Self) -> Ordering {
        self.deploy_id.cmp(&other.deploy_id)
    }
}

impl PartialOrd for DeployIndex {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl DeployIndex {
    pub const SYS_SLASH_DEPLOY_COST: i64 = 0;
    pub const SYS_CLOSE_BLOCK_DEPLOY_COST: i64 = 0;
    pub const SYS_EMPTY_DEPLOY_COST: i64 = 0;

    pub fn sys_slash_deploy_id() -> Vec<u8> {
        vec![1]
    }
    pub fn sys_close_block_deploy_id() -> Vec<u8> {
        vec![2]
    }
    pub fn sys_empty_deploy_id() -> Vec<u8> {
        vec![3]
    }
}

/// The merged state seen by a block's parents (port of `ParentsMergedState`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParentsMergedState {
    pub justifications: Vec<BlockMetadata>,
    pub max_block_num: i64,
    pub max_seq_nums: BTreeMap<Validator, i64>,
    pub fringe: BTreeSet<BlockHash>,
    pub fringe_state: Blake2b256Hash,
    pub fringe_bonds_map: BTreeMap<Validator, NonNegI64>,
    pub fringe_rejected_deploys: BTreeSet<Vec<u8>>,
    pub pre_state_hash: Blake2b256Hash,
    pub rejected_deploys: BTreeSet<Vec<u8>>,
}

/// The index of deploys depending on each other within a single block (port of `DeployChainIndex`).
#[derive(Clone, Debug)]
pub struct DeployChainIndex {
    pub host_block: Blake2b256Hash,
    pub deploys_with_cost: BTreeSet<DeployIdWithCost>,
    pub pre_state_hash: Blake2b256Hash,
    pub post_state_hash: Blake2b256Hash,
    pub event_log_index: EventLogIndex,
    pub state_changes: StateChange,
}

// Equality/hash are over `deploysWithCost` only (the Scala override), to speed up rejection-option
// computation.
impl PartialEq for DeployChainIndex {
    fn eq(&self, other: &Self) -> bool {
        self.deploys_with_cost == other.deploys_with_cost
    }
}

impl Eq for DeployChainIndex {}

impl Hash for DeployChainIndex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.deploys_with_cost.hash(state);
    }
}

// Ordering is over `(hostBlock, postStateHash)` (the Scala `Ordering.by`), distinct from equality
// (which is over `deploysWithCost`).
impl PartialOrd for DeployChainIndex {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DeployChainIndex {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.host_block, self.post_state_hash).cmp(&(other.host_block, other.post_state_hash))
    }
}

impl DeployChainIndex {
    /// The total cost of the deploy chain (port of `deployChainCost`).
    pub fn deploy_chain_cost(r: &DeployChainIndex) -> i64 {
        r.deploys_with_cost.iter().map(|d| d.cost).sum()
    }

    /// Whether `a` depends on `b` (port of `depends`).
    pub fn depends(a: &DeployChainIndex, b: &DeployChainIndex) -> bool {
        depends(&a.event_log_index, &b.event_log_index)
    }

    /// Whether two sets of deploy chains conflict (port of `branchesAreConflicting`).
    pub fn branches_are_conflicting(
        a: &BTreeSet<DeployChainIndex>,
        b: &BTreeSet<DeployChainIndex>,
    ) -> bool {
        let a_ids: BTreeSet<&Vec<u8>> = a
            .iter()
            .flat_map(|d| d.deploys_with_cost.iter().map(|x| &x.id))
            .collect();
        let b_ids: BTreeSet<&Vec<u8>> = b
            .iter()
            .flat_map(|d| d.deploys_with_cost.iter().map(|x| &x.id))
            .collect();
        let a_event = a
            .iter()
            .fold(EventLogIndex::empty(), |acc, d| EventLogIndex::combine(&acc, &d.event_log_index));
        let b_event = b
            .iter()
            .fold(EventLogIndex::empty(), |acc, d| EventLogIndex::combine(&acc, &d.event_log_index));
        !a_ids.is_disjoint(&b_ids) || are_conflicting(&a_event, &b_event)
    }

    /// Whether two deploy chains conflict (port of `deploysAreConflicting`).
    pub fn deploys_are_conflicting(a: &DeployChainIndex, b: &DeployChainIndex) -> bool {
        let a_ids: BTreeSet<&Vec<u8>> = a.deploys_with_cost.iter().map(|x| &x.id).collect();
        let b_ids: BTreeSet<&Vec<u8>> = b.deploys_with_cost.iter().map(|x| &x.id).collect();
        !a_ids.is_disjoint(&b_ids) || are_conflicting(&a.event_log_index, &b.event_log_index)
    }

    /// Build a deploy chain from its member deploys + pre/post state (port of
    /// `DeployChainIndex.apply`).
    pub async fn apply<C, P, A, K>(
        host_block: Blake2b256Hash,
        deploys: &BTreeSet<DeployIndex>,
        pre_state_hash: Blake2b256Hash,
        post_state_hash: Blake2b256Hash,
        history_repository: &HistoryRepository<C, P, A, K>,
    ) -> Result<DeployChainIndex, String>
    where
        C: Serialize<C> + Send + Sync + 'static,
        P: Serialize<P> + Send + Sync + 'static,
        A: Serialize<A> + Send + Sync + 'static,
        K: Serialize<K> + Send + Sync + 'static,
    {
        let deploys_with_cost: BTreeSet<DeployIdWithCost> = deploys
            .iter()
            .map(|d| DeployIdWithCost {
                id: d.deploy_id.clone(),
                cost: d.cost,
            })
            .collect();
        let event_log_index = deploys.iter().fold(EventLogIndex::empty(), |acc, d| {
            EventLogIndex::combine(&acc, &d.event_log_index)
        });

        let pre_reader = history_repository.get_history_reader(pre_state_hash).await;
        let pre_binary = pre_reader.reader_binary();
        let post_reader = history_repository.get_history_reader(post_state_hash).await;
        let post_binary = post_reader.reader_binary();

        let state_changes = StateChange::apply(
            pre_binary.as_ref(),
            post_binary.as_ref(),
            &event_log_index,
        )
        .await?;

        Ok(DeployChainIndex {
            host_block,
            deploys_with_cost,
            pre_state_hash,
            post_state_hash,
            event_log_index,
            state_changes,
        })
    }
}

/// The index of a block: its deploy chains (port of `BlockIndex`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockIndex {
    pub block_hash: BlockHash,
    pub deploy_chains: Vec<DeployChainIndex>,
}

impl BlockIndex {
    /// Build an `EventLogIndex` from a deploy's events against the pre-state (port of
    /// `BlockIndex.createEventLogIndex`).
    pub async fn create_event_log_index<C, P, A, K>(
        events: &[Event],
        history_repository: &HistoryRepository<C, P, A, K>,
        pre_state_hash: Blake2b256Hash,
        mergeable_chs: NumberChannelsDiff,
    ) -> Result<EventLogIndex, String>
    where
        C: Serialize<C> + Send + Sync + 'static,
        P: Serialize<P> + Send + Sync + 'static,
        A: Serialize<A> + Send + Sync + 'static,
        K: Serialize<K> + Send + Sync + 'static,
    {
        let pre_reader = history_repository.get_history_reader(pre_state_hash).await;
        let rspace_events: Vec<REvent> = events.iter().map(to_rspace_event).collect();

        // Collect the distinct produces referenced by the trace to resolve the two pre-state
        // predicates (`produceExistsInPreState` and `produceTouchesPreStateJoin`).
        let mut produces: BTreeSet<Produce> = BTreeSet::new();
        for e in &rspace_events {
            match e {
                REvent::Produce(p) => {
                    produces.insert(p.clone());
                }
                REvent::Comm(c) => {
                    for p in &c.produces {
                        produces.insert(p.clone());
                    }
                }
                REvent::Consume(_) => {}
            }
        }

        let mut exists_in_pre_state: BTreeSet<Produce> = BTreeSet::new();
        let mut touches_pre_state_join: BTreeSet<Produce> = BTreeSet::new();
        for p in &produces {
            let data = pre_reader
                .get_data(p.channels_hash)
                .await
                .map_err(|e| e.to_string())?;
            if data.iter().any(|d| d.source == *p) {
                exists_in_pre_state.insert(p.clone());
            }
            let joins = pre_reader
                .get_joins(p.channels_hash)
                .await
                .map_err(|e| e.to_string())?;
            if joins.iter().any(|j| j.len() > 1) {
                touches_pre_state_join.insert(p.clone());
            }
        }

        Ok(EventLogIndex::apply(
            &rspace_events,
            |p| exists_in_pre_state.contains(p),
            |p| touches_pre_state_join.contains(p),
            mergeable_chs,
        ))
    }

    /// Build a `BlockIndex` from the processed deploys + mergeable-channel data (port of
    /// `BlockIndex.apply`).
    #[allow(clippy::too_many_arguments)]
    pub async fn apply<C, P, A, K>(
        block_hash: BlockHash,
        usr_processed_deploys: &[ProcessedDeploy],
        sys_processed_deploys: &[ProcessedSystemDeploy],
        pre_state_hash: Blake2b256Hash,
        post_state_hash: Blake2b256Hash,
        history_repository: &HistoryRepository<C, P, A, K>,
        mergeable_chan_data: &[NumberChannelsDiff],
    ) -> Result<BlockIndex, String>
    where
        C: Serialize<C> + Send + Sync + 'static,
        P: Serialize<P> + Send + Sync + 'static,
        A: Serialize<A> + Send + Sync + 'static,
        K: Serialize<K> + Send + Sync + 'static,
    {
        let usr_count = usr_processed_deploys.len();
        let deploy_count = usr_count + sys_processed_deploys.len();
        let mrg_count = mergeable_chan_data.len();
        if deploy_count != mrg_count {
            return Err(format!(
                "Cache of mergeable channels ({mrg_count}) doesn't match deploys count ({deploy_count})."
            ));
        }

        let (usr_mergeable, sys_mergeable) = mergeable_chan_data.split_at(usr_count);

        let mut deploy_indices: BTreeSet<DeployIndex> = BTreeSet::new();

        // User deploy indices (failed deploys are skipped).
        for (d, merge_chs) in usr_processed_deploys.iter().zip(usr_mergeable.iter()) {
            if d.is_failed {
                continue;
            }
            let event_log_index = Self::create_event_log_index(
                &d.deploy_log,
                history_repository,
                pre_state_hash,
                merge_chs.clone(),
            )
            .await?;
            deploy_indices.insert(DeployIndex {
                deploy_id: d.deploy.sig.clone(),
                cost: i64::try_from(d.cost.cost).map_err(|e| e.to_string())?,
                event_log_index,
            });
        }

        // System deploy indices (only `Succeeded` blocks contribute).
        for (sd, merge_chs) in sys_processed_deploys.iter().zip(sys_mergeable.iter()) {
            let (id, log) = match sd {
                ProcessedSystemDeploy::Succeeded {
                    event_list,
                    system_deploy: SystemDeployData::Slash(_),
                } => (sys_deploy_id(&block_hash, 1), event_list),
                ProcessedSystemDeploy::Succeeded {
                    event_list,
                    system_deploy: SystemDeployData::CloseBlock,
                } => (sys_deploy_id(&block_hash, 2), event_list),
                ProcessedSystemDeploy::Succeeded {
                    event_list,
                    system_deploy: SystemDeployData::Empty,
                } => (sys_deploy_id(&block_hash, 3), event_list),
                ProcessedSystemDeploy::Failed { .. } => continue,
            };
            let event_log_index = Self::create_event_log_index(
                log,
                history_repository,
                pre_state_hash,
                merge_chs.clone(),
            )
            .await?;
            deploy_indices.insert(DeployIndex {
                deploy_id: id,
                cost: 0,
                event_log_index,
            });
        }

        // Deploys in a block execute sequentially, so there are only dependencies (no conflicts).
        let dependency_map =
            compute_dependency_map(&deploy_indices, &deploy_indices, |l, r| {
                depends(&l.event_log_index, &r.event_log_index)
            });
        let deploy_chains =
            compute_greedy_non_intersecting_branches(&deploy_indices, &dependency_map);

        let host_block = Blake2b256Hash::from_bytes(*block_hash.as_bytes());
        let mut chains = Vec::new();
        for chain in &deploy_chains {
            chains.push(
                DeployChainIndex::apply(
                    host_block,
                    chain,
                    pre_state_hash,
                    post_state_hash,
                    history_repository,
                )
                .await?,
            );
        }

        Ok(BlockIndex {
            block_hash,
            deploy_chains: chains,
        })
    }
}

/// Build the system-deploy id as `blockHash ++ prefix` (port of `blockHash.concat(SYS_*_DEPLOY_ID)`).
fn sys_deploy_id(block_hash: &BlockHash, prefix: u8) -> Vec<u8> {
    let mut id = block_hash.as_bytes().to_vec();
    id.push(prefix);
    id
}

/// The in-memory block-index cache (port of `BlockIndex.cache`).
static BLOCK_INDEX_CACHE: OnceLock<Mutex<BTreeMap<BlockHash, BlockIndex>>> = OnceLock::new();

async fn get_block_unsafe(block_store: &BlockStore, hash: &BlockHash) -> Result<BlockMessage, String> {
    let mut vals = block_store.get(&[*hash]).await?;
    vals.pop()
        .flatten()
        .ok_or_else(|| format!("missing block {}", hash.to_hex()))
}

impl BlockIndex {
    /// Load (or compute + cache) a block index (port of `BlockIndex.getBlockIndex`).
    pub async fn get_block_index(
        runtime: &RuntimeManager,
        block_store: &BlockStore,
        block_hash: BlockHash,
    ) -> Result<BlockIndex, String> {
        let cache = BLOCK_INDEX_CACHE.get_or_init(|| Mutex::new(BTreeMap::new()));
        if let Some(idx) = cache.lock().unwrap_or_else(|p| p.into_inner()).get(&block_hash) {
            return Ok(idx.clone());
        }

        let block = get_block_unsafe(block_store, &block_hash).await?;
        let sender = block.sender.as_bytes().to_vec();
        let pre_state_hash = Blake2b256Hash::from_byte_array(block.pre_state_hash.as_bytes());
        let post_state_hash = Blake2b256Hash::from_byte_array(block.post_state_hash.as_bytes());
        let mergeable_chs = runtime
            .load_mergeable_channels(post_state_hash.as_bytes(), &sender, i64::from(block.seq_num))
            .await?;

        let index = BlockIndex::apply(
            block.block_hash,
            &block.state.deploys,
            &block.state.system_deploys,
            pre_state_hash,
            post_state_hash,
            runtime.get_history_repo(),
            &mergeable_chs,
        )
        .await?;

        cache.lock().unwrap_or_else(|p| p.into_inner()).insert(block_hash, index.clone());
        Ok(index)
    }

    /// Remove cached block indices for `hashes` (port of `BlockIndex.cache.remove` in `pruneDiff`).
    ///
    /// The cache is advisory: a pruned entry is simply recomputed on the next `get_block_index`, so
    /// pruning is always safe (it never affects correctness, only recomputation cost). This bounds
    /// the otherwise-unbounded in-memory cache as the fringe finalizes.
    pub fn prune_cache(hashes: &[BlockHash]) {
        let Some(cache) = BLOCK_INDEX_CACHE.get() else {
            return;
        };
        let mut guard = cache.lock().unwrap_or_else(|p| p.into_inner());
        for h in hashes {
            guard.remove(h);
        }
    }
}

/// The scope of a merge: final (immutable) and conflict (alterable) blocks (port of `MergeScope`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MergeScope {
    pub final_scope: BTreeSet<BlockHash>,
    pub conflict_scope: BTreeSet<BlockHash>,
}

impl MergeScope {
    /// Create a merge scope from the DAG (port of `MergeScope.fromDag`).
    pub fn from_dag(
        merge_fringe: &BTreeSet<BlockHash>,
        final_fringe: &BTreeSet<BlockHash>,
        child_map: &BTreeMap<BlockHash, BTreeSet<BlockHash>>,
        dag_data: &BTreeMap<BlockHash, Message<BlockHash, Validator>>,
    ) -> Result<(MergeScope, Option<BlockHash>), String> {
        let prune_fringe: BTreeSet<BlockHash> =
            message_map::prune_fringe(dag_data, final_fringe, child_map)
                .iter()
                .map(|m| m.id)
                .collect();
        MergeScope::from_fringes(merge_fringe, final_fringe, &prune_fringe, dag_data)
    }

    /// Create a merge scope from explicit fringes (port of `MergeScope.fromFringes`).
    pub fn from_fringes(
        merge_fringe: &BTreeSet<BlockHash>,
        final_fringe: &BTreeSet<BlockHash>,
        prune_fringe: &BTreeSet<BlockHash>,
        dag_data: &BTreeMap<BlockHash, Message<BlockHash, Validator>>,
    ) -> Result<(MergeScope, Option<BlockHash>), String> {
        let merge_msgs: BTreeSet<Message<BlockHash, Validator>> = merge_fringe
            .iter()
            .map(|h| {
                dag_data
                    .get(h)
                    .cloned()
                    .ok_or_else(|| format!("merge fringe not in dag: {}", h.to_hex()))
            })
            .collect::<Result<_, String>>()?;
        let final_msgs: BTreeSet<Message<BlockHash, Validator>> = final_fringe
            .iter()
            .map(|h| {
                dag_data
                    .get(h)
                    .cloned()
                    .ok_or_else(|| format!("final fringe not in dag: {}", h.to_hex()))
            })
            .collect::<Result<_, String>>()?;
        let prune_msgs: BTreeSet<Message<BlockHash, Validator>> = prune_fringe
            .iter()
            .map(|h| {
                dag_data
                    .get(h)
                    .cloned()
                    .ok_or_else(|| format!("prune fringe not in dag: {}", h.to_hex()))
            })
            .collect::<Result<_, String>>()?;

        let c_scope = message_map::between(dag_data, &merge_msgs, &final_msgs);
        let f_scope = message_map::between(dag_data, &final_msgs, &prune_msgs);

        let f_scope_ids: BTreeSet<BlockHash> = f_scope.iter().map(|m| m.id).collect();
        let base_msg = if f_scope.is_empty() {
            let genesis = message_map::find_with_empty_parents(dag_data)
                .ok_or_else(|| "Final scope is empty but no genesis found.".to_string())?;
            Some(genesis.id)
        } else {
            None
        };
        let c_scope_ids: BTreeSet<BlockHash> = c_scope.iter().map(|m| m.id).collect();
        let conflict_scope: BTreeSet<BlockHash> = c_scope_ids
            .difference(&base_msg.into_iter().collect())
            .copied()
            .collect();

        Ok((
            MergeScope {
                final_scope: f_scope_ids,
                conflict_scope,
            },
            base_msg,
        ))
    }

    /// Merge the conflict scope into the base state, returning the new state hash + rejected
    /// deploy ids (port of `MergeScope.merge`).
    pub async fn merge<F, Fut>(
        merge_scope: &MergeScope,
        base_state: Blake2b256Hash,
        fringe_states: &BTreeMap<BTreeSet<BlockHash>, FringeData>,
        history_repository: &RhoHistoryRepository,
        block_index: &F,
        rejection_cost: impl Fn(&DeployChainIndex) -> i64,
    ) -> Result<(Blake2b256Hash, BTreeSet<Vec<u8>>), String>
    where
        F: Fn(BlockHash) -> Fut,
        Fut: std::future::Future<Output = Result<BlockIndex, String>>,
    {
        let conflict_indices: Vec<BlockIndex> = {
            let mut v = Vec::new();
            for h in &merge_scope.conflict_scope {
                v.push(block_index(*h).await?);
            }
            v
        };
        let final_indices: Vec<BlockIndex> = {
            let mut v = Vec::new();
            for h in &merge_scope.final_scope {
                v.push(block_index(*h).await?);
            }
            v
        };

        let conflict_set: BTreeSet<DeployChainIndex> = conflict_indices
            .iter()
            .flat_map(|b| b.deploy_chains.iter().cloned())
            .collect();
        let final_set: BTreeSet<DeployChainIndex> = final_indices
            .iter()
            .flat_map(|b| b.deploy_chains.iter().cloned())
            .collect();

        // Finalization decisions made in the final set.
        let mut rejections_map: BTreeMap<BlockHash, BTreeSet<Vec<u8>>> = BTreeMap::new();
        for (fringe, fd) in fringe_states {
            for h in fringe {
                rejections_map.insert(*h, fd.rejected_deploys.clone());
            }
        }
        let mut rejected_finally: BTreeSet<DeployChainIndex> = BTreeSet::new();
        let mut accepted_finally: BTreeSet<DeployChainIndex> = BTreeSet::new();
        for b in &final_indices {
            let rejected = rejections_map.get(&b.block_hash);
            for chain in &b.deploy_chains {
                let first_id = chain.deploys_with_cost.iter().next().map(|d| d.id.clone());
                let is_rejected = match (rejected, &first_id) {
                    (Some(rej), Some(id)) => rej.contains(id),
                    _ => false,
                };
                if is_rejected {
                    rejected_finally.insert(chain.clone());
                } else {
                    accepted_finally.insert(chain.clone());
                }
            }
        }

        // Mergeable channels.
        let mergeable_diffs_map: BTreeMap<DeployChainIndex, NumberChannelsDiff> = conflict_set
            .iter()
            .map(|b| (b.clone(), b.event_log_index.number_channels_data.clone()))
            .collect();
        let mut all_channel_hashes: BTreeSet<Blake2b256Hash> = BTreeSet::new();
        for diffs in mergeable_diffs_map.values() {
            all_channel_hashes.extend(diffs.keys().copied());
        }
        let init_mergeable_values =
            read_mergeable_values(history_repository, base_state, &all_channel_hashes).await?;

        let (conflicts_map, dependency_map) = compute_relation_map_for_merge_set(
            &conflict_set,
            &final_set,
            DeployChainIndex::deploys_are_conflicting,
            DeployChainIndex::depends,
        );

        let (to_merge, rejected) = resolve_conflict_set(
            &conflict_set,
            &accepted_finally,
            &rejected_finally,
            rejection_cost,
            &conflicts_map,
            &dependency_map,
            &mergeable_diffs_map,
            &init_mergeable_values,
        );

        let new_state =
            MergeScope::compute_merged_state(&to_merge, base_state, history_repository).await?;
        let rejected_ids: BTreeSet<Vec<u8>> = rejected
            .iter()
            .flat_map(|d| d.deploys_with_cost.iter().map(|x| x.id.clone()))
            .collect();
        Ok((new_state, rejected_ids))
    }

    /// Merge a set of deploy chains into the base state and produce the new state hash (port of
    /// `MergeScope.computeMergedState`).
    pub async fn compute_merged_state(
        to_merge: &BTreeSet<DeployChainIndex>,
        base_state: Blake2b256Hash,
        history_repository: &RhoHistoryRepository,
    ) -> Result<Blake2b256Hash, String> {
        let history_reader = history_repository.get_history_reader(base_state).await;
        let base_reader = history_reader.reader_binary();

        // Combine all state changes + mergeable diffs.
        let all_changes = to_merge
            .iter()
            .fold(StateChange::empty(), |acc, b| StateChange::combine(&acc, &b.state_changes));
        let mut mergeable_diffs: NumberChannelsDiff = BTreeMap::new();
        for b in to_merge {
            for (k, v) in &b.event_log_index.number_channels_data {
                *mergeable_diffs.entry(*k).or_insert(0) += v;
            }
        }

        // Pre-compute the mergeable-channel override actions.
        let mut overrides: BTreeMap<
            Blake2b256Hash,
            HotStoreTrieAction<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>,
        > = BTreeMap::new();
        for (hash, diff) in &mergeable_diffs {
            let changes = all_changes.datums_changes.get(hash).cloned().unwrap_or_default();
            let action =
                calculate_number_channel_merge(*hash, *diff, &changes, history_reader.as_ref())
                    .await?;
            overrides.insert(*hash, action);
        }

        let trie_actions = compute_trie_actions(
            &all_changes,
            base_reader.as_ref(),
            mergeable_diffs.clone(),
            |hash, _changes, _chs| overrides.get(hash).cloned(),
        )
        .await?;

        let reset_repo = history_repository
            .reset(base_state)
            .await
            .map_err(|e| e.to_string())?;
        let new_repo = reset_repo
            .do_checkpoint(&trie_actions)
            .await
            .map_err(|e| e.to_string())?;
        Ok(new_repo.root())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn chain(id: u8, cost: i64) -> DeployChainIndex {
        DeployChainIndex {
            host_block: Blake2b256Hash::from_bytes([id; 32]),
            deploys_with_cost: BTreeSet::from([DeployIdWithCost {
                id: vec![id],
                cost,
            }]),
            pre_state_hash: Blake2b256Hash::from_bytes([0u8; 32]),
            post_state_hash: Blake2b256Hash::from_bytes([0u8; 32]),
            event_log_index: EventLogIndex::empty(),
            state_changes: StateChange::empty(),
        }
    }

    #[test]
    fn deploy_chain_cost_sums() {
        let a = chain(1, 5);
        let b = chain(2, 7);
        assert_eq!(DeployChainIndex::deploy_chain_cost(&a), 5);
        assert_eq!(DeployChainIndex::deploy_chain_cost(&b), 7);
    }

    #[test]
    fn equality_is_by_deploys_with_cost() {
        let a = chain(1, 5);
        let mut b = chain(1, 5);
        // Different host block — equality is over deploysWithCost only.
        b.host_block = Blake2b256Hash::from_bytes([9; 32]);
        assert_eq!(a, b);
        // Different cost — not equal.
        let c = chain(1, 6);
        assert_ne!(a, c);
    }

    #[test]
    fn deploys_are_conflicting_by_shared_id() {
        let a = chain(1, 5);
        let mut b = chain(1, 5);
        // Same deploy id -> conflicting (shared id), regardless of event logs.
        b.host_block = Blake2b256Hash::from_bytes([9; 32]);
        assert!(DeployChainIndex::deploys_are_conflicting(&a, &b));
        // Different ids, empty event logs -> not conflicting.
        assert!(!DeployChainIndex::deploys_are_conflicting(&chain(1, 5), &chain(2, 5)));
    }

    fn msg(id: u8, parents: &[u8], seen: &[u8]) -> Message<BlockHash, Validator> {
        let hash = |b: u8| BlockHash::new([b; 32]);
        Message {
            id: hash(id),
            height: rchain_shared::refined::BlockHeight::zero(),
            sender: Validator::new([id; 65]),
            sender_seq: rchain_shared::refined::SeqNum::zero(),
            bonds_map: BTreeMap::new(),
            parents: parents.iter().map(|&b| hash(b)).collect(),
            fringe: BTreeSet::new(),
            seen: seen.iter().map(|&b| hash(b)).collect(),
        }
    }

    #[test]
    fn merge_scope_genesis_returns_genesis_as_base() {
        let g = msg(1, &[], &[1]);
        let c = msg(2, &[1], &[1, 2]);
        let dag = BTreeMap::from([(g.id, g.clone()), (c.id, c.clone())]);

        let (scope, base) = MergeScope::from_fringes(
            &[c.id].into_iter().collect(),
            &BTreeSet::new(),
            &BTreeSet::new(),
            &dag,
        )
        .unwrap();
        // Final scope empty -> genesis is the base; conflict scope excludes it.
        assert!(scope.final_scope.is_empty());
        assert_eq!(scope.conflict_scope, [c.id].into_iter().collect());
        assert_eq!(base, Some(g.id));
    }
}
