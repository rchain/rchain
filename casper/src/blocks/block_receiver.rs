//! Block receiver (port of `blocks/BlockReceiver.scala`).
//!
//! The pure `BlockReceiverState` state machine (begin/end storing + finished), the `not_validated`
//! helper, and the `apply` stream wiring (incoming + validated block streams → validation queue)
//! are ported.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::BlockMessage;
use rchain_shared::log::{Log, LogSource};
use tokio::sync::mpsc;

use crate::blocks::block_retriever::{AdmitHashReason, BlockRetriever};
use crate::validate::{block_hash, block_signature, format_of_fields};

/// Block-receive status (port of `RecvStatus`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RecvStatus {
    /// Begin checking and storing block.
    BeginStoreBlock,
    /// Block stored in the block store, waiting for validation and DAG insertion.
    EndStoreBlock,
    /// Block sent to validation.
    PendingValidation,
    /// Requested missing dependencies.
    Requested,
}

/// Block receiver state (port of `BlockReceiverState`).
///
/// It consists of three events: two to store blocks (begin and end) to prevent a race when storing
/// blocks, and `finished` when a block is validated and added to the DAG (end of processing).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockReceiverState<MId: Ord + Clone + std::fmt::Debug> {
    /// Blocks received and stored in BlockStore (not validated) with parent relations.
    blocks_st: BTreeMap<MId, BTreeSet<MId>>,
    /// Blocks receiving status.
    receive_st: BTreeMap<MId, RecvStatus>,
    /// Blocks mapping with children relations.
    child_relations: BTreeMap<MId, BTreeSet<MId>>,
}

impl<MId: Ord + Clone + std::fmt::Debug> BlockReceiverState<MId> {
    /// Create an empty receiver state (port of `BlockReceiverState.apply`).
    pub fn new() -> Self {
        BlockReceiverState {
            blocks_st: BTreeMap::new(),
            receive_st: BTreeMap::new(),
            child_relations: BTreeMap::new(),
        }
    }

    /// Begin storing a block, marking it to prevent duplicate threads storing the same block. The
    /// returned flag is `true` when storing should proceed (port of `beginStored`).
    pub fn begin_stored(&self, id: MId) -> (Self, bool) {
        // If state is not known or pending request, it's expected, so continue with receiving.
        let expected_receive = match self.receive_st.get(&id) {
            Some(RecvStatus::Requested) => true,
            Some(_) => false,
            None => true,
        };
        if expected_receive {
            let mut receive_st = self.receive_st.clone();
            receive_st.insert(id, RecvStatus::BeginStoreBlock);
            (
                BlockReceiverState {
                    receive_st,
                    ..self.clone()
                },
                true,
            )
        } else {
            (self.clone(), false)
        }
    }

    /// Storing of the block is done, waiting validation. Returns the updated state and the unseen
    /// parent dependencies (port of `endStored`). The Scala `assert` is a `Result` error here so an
    /// invariant violation is logged and the block skipped rather than panicking the task.
    pub fn end_stored(&self, id: MId, parents: Vec<(MId, bool)>) -> Result<(Self, BTreeSet<MId>), String> {
        let cur_state_opt = self.receive_st.get(&id);
        if cur_state_opt != Some(&RecvStatus::BeginStoreBlock) {
            return Err(format!(
                "Received should be called only in begin received state, actual: {:?}, hash: {:?}",
                cur_state_opt, id
            ));
        }

        // Update blocks state, keep unseen parents only.
        let parents_not_stored: BTreeSet<MId> = parents
            .iter()
            .filter(|(_, not_stored)| *not_stored)
            .map(|(parent, _)| parent.clone())
            .collect();
        let mut unseen_parents = parents_not_stored;
        unseen_parents.retain(|parent| {
            !self.blocks_st.contains_key(parent)
                && !self.receive_st.contains_key(parent)
                && parent != &id
        });
        let mut new_blocks_st = self.blocks_st.clone();
        new_blocks_st.insert(id.clone(), unseen_parents.clone());

        // Update block status to received and set unseen parents to Requested.
        let mut new_receive_st = self.receive_st.clone();
        new_receive_st.insert(id.clone(), RecvStatus::EndStoreBlock);
        for parent in &unseen_parents {
            new_receive_st.insert(parent.clone(), RecvStatus::Requested);
        }

        // Update children relations of the received block.
        let mut new_child_relations = self.child_relations.clone();
        for (parent, _) in &parents {
            new_child_relations
                .entry(parent.clone())
                .or_default()
                .insert(id.clone());
        }

        let new_state = BlockReceiverState {
            blocks_st: new_blocks_st,
            receive_st: new_receive_st,
            child_relations: new_child_relations,
        };
        Ok((new_state, unseen_parents))
    }

    /// Finish block validation, updating state and returning the next blocks with validated
    /// dependencies (port of `finished`). The Scala `assert` is a `Result` error here so an
    /// invariant violation is logged and the block skipped rather than panicking the task.
    pub fn finished(&self, id: MId, parents: BTreeSet<MId>) -> Result<(Self, BTreeSet<MId>), String> {
        let parents_in_state = self.blocks_st.contains_key(&id);
        let is_received = matches!(
            self.receive_st.get(&id),
            Some(RecvStatus::EndStoreBlock) | Some(RecvStatus::PendingValidation)
        );
        // To finish a block it must be present in the state (parents relations and at least stored).
        if !(parents_in_state && is_received) {
            return Err(format!("Calling finished on unexpected block hash {:?}.", id));
        }

        // Remove the finished block from its children's dependencies and from the blocks state.
        let childs: Vec<MId> = self
            .child_relations
            .get(&id)
            .map(|s| s.iter().cloned().collect())
            .unwrap_or_default();
        let updated_blocks: BTreeMap<MId, BTreeSet<MId>> = childs
            .iter()
            .map(|child| {
                let mut deps = self.blocks_st.get(child).cloned().unwrap_or_default();
                deps.remove(&id);
                (child.clone(), deps)
            })
            .collect();
        let mut new_blocks_st = self.blocks_st.clone();
        for (child, deps) in &updated_blocks {
            new_blocks_st.insert(child.clone(), deps.clone());
        }
        new_blocks_st.remove(&id);

        // Next blocks with all dependencies validated and not already in pending validation state.
        let deps_validated: BTreeSet<MId> = updated_blocks
            .iter()
            .filter(|(bid, parents)| {
                let pending = matches!(self.receive_st.get(*bid), Some(RecvStatus::PendingValidation));
                parents.is_empty() && !pending
            })
            .map(|(bid, _)| bid.clone())
            .collect();

        // Set next blocks to pending validation and remove the finished block.
        let mut new_receive_st = self.receive_st.clone();
        for dep in &deps_validated {
            new_receive_st.insert(dep.clone(), RecvStatus::PendingValidation);
        }
        new_receive_st.remove(&id);

        // Remove the finished block from children relations.
        let mut new_child_relations: BTreeMap<MId, BTreeSet<MId>> = BTreeMap::new();
        for (parent, childs) in &self.child_relations {
            if parents.contains(parent) {
                let mut childs = childs.clone();
                childs.remove(&id);
                if !childs.is_empty() {
                    new_child_relations.insert(parent.clone(), childs);
                }
            } else {
                new_child_relations.insert(parent.clone(), childs.clone());
            }
        }

        let new_state = BlockReceiverState {
            blocks_st: new_blocks_st,
            receive_st: new_receive_st,
            child_relations: new_child_relations,
        };
        Ok((new_state, deps_validated))
    }
}

impl<MId: Ord + Clone + std::fmt::Debug> Default for BlockReceiverState<MId> {
    fn default() -> Self {
        Self::new()
    }
}

/// Check whether a block is stored but not yet validated into the DAG (port of
/// `BlockReceiver.notValidated`).
pub async fn not_validated(
    block_store: &BlockStore,
    dag: &dyn BlockDagStorage,
    hash: &BlockHash,
) -> bool {
    let in_store = block_store
        .contains(&[*hash])
        .await
        .unwrap_or_default()
        .first()
        .copied()
        .unwrap_or(false);
    if !in_store {
        return false;
    }
    let repr = dag.get_representation().await;
    !repr.contains(hash)
}

// -------------------------------------------------------------------------------------------------
// Stream wiring (port of `BlockReceiver.apply`)
// -------------------------------------------------------------------------------------------------

/// Check that a block is cryptographically safe and part of the same shard (port of
/// `checkIfOfInterest`).
async fn check_if_of_interest(
    block: &BlockMessage,
    conf_shard_name: &str,
    log: &dyn Log,
    source: LogSource,
) -> bool {
    let valid_shard = conf_shard_name == block.shard_id;
    if !valid_shard {
        log.info(
            source,
            &format!(
                "Ignored block with invalid shard, expected: {conf_shard_name}, received: {}",
                block.shard_id
            ),
        );
    }
    valid_shard
        && format_of_fields(block)
        && block_hash(block)
        && block_signature(block)
}

/// Check that a block is older than the current DAG's lowest height (port of `checkIfKnown`).
async fn check_if_known(block: &BlockMessage, dag: &dyn BlockDagStorage) -> bool {
    let repr = dag.get_representation().await;
    repr.height_map
        .first_key_value()
        .map(|(h, _)| i64::from(*h))
        .unwrap_or(-1)
        > i64::from(block.block_number)
}

/// Request the missing dependencies of a block (port of `requestMissingDependencies`).
async fn request_missing_dependencies(
    deps: &BTreeSet<BlockHash>,
    block_retriever: &BlockRetriever,
) {
    for hash in deps {
        block_retriever
            .admit_hash(hash, None, AdmitHashReason::MissingDependencyRequested)
            .await;
    }
}

/// Re-send stored blocks back to the incoming queue for validation (port of `sendToValidate`).
async fn send_to_validate(
    hashes: &BTreeSet<BlockHash>,
    block_store: &BlockStore,
    put_to_incoming_queue: &(dyn Fn(BlockMessage) + Send + Sync),
) {
    for hash in hashes {
        let block = block_store
            .get(&[*hash])
            .await
            .ok()
            .and_then(|mut v| v.pop())
            .flatten();
        if let Some(block) = block {
            put_to_incoming_queue(block);
        }
    }
}

/// Process incoming blocks (port of `incomingBlocks`): filter, store, resolve dependencies, and
/// forward dependency-free blocks to the output queue.
async fn incoming_blocks(
    mut incoming_blocks_rx: mpsc::UnboundedReceiver<BlockMessage>,
    state: Arc<tokio::sync::Mutex<BlockReceiverState<BlockHash>>>,
    conf_shard_name: Arc<str>,
    block_store: BlockStore,
    dag: Arc<dyn BlockDagStorage>,
    block_retriever: Arc<BlockRetriever>,
    put_to_incoming_queue: Arc<dyn Fn(BlockMessage) + Send + Sync>,
    out_tx: mpsc::UnboundedSender<BlockHash>,
    log: Arc<dyn Log>,
) {
    let source = LogSource::new("casper.blocks.BlockReceiver");
    while let Some(block) = incoming_blocks_rx.recv().await {
        // Filter out blocks that are not of interest.
        if !check_if_of_interest(&block, &conf_shard_name, log.as_ref(), source).await {
            log.info(
                source,
                &format!("Block {} is malformed. Dropped", block.block_hash.to_hex()),
            );
            continue;
        }

        // Begin storing the block.
        let should_check = {
            let mut guard = state.lock().await;
            let (new_state, should_check) = guard.begin_stored(block.block_hash);
            *guard = new_state;
            should_check
        };

        // Ignore blocks older than the DAG.
        if check_if_known(&block, dag.as_ref()).await {
            log.info(
                source,
                &format!("Block {} is not of interest. Dropped", block.block_hash.to_hex()),
            );
            continue;
        }

        if !should_check {
            continue;
        }

        // Store the block and resolve its parent dependencies.
        let block_stored = block_store
            .contains(&[block.block_hash])
            .await
            .unwrap_or_default()
            .first()
            .copied()
            .unwrap_or(false);
        if !block_stored {
            let _ = block_store.put(&[(block.block_hash, block.clone())]).await;
        }

        let mut parents = Vec::new();
        for hash in &block.justifications {
            let not_stored = !block_store
                .contains(&[*hash])
                .await
                .unwrap_or_default()
                .first()
                .copied()
                .unwrap_or(false);
            parents.push((*hash, not_stored));
        }

        let pending_requests = {
            let mut guard = state.lock().await;
            match guard.end_stored(block.block_hash, parents) {
                Ok((new_state, unseen)) => {
                    *guard = new_state;
                    unseen
                }
                Err(e) => {
                    log.error(source, &e);
                    continue;
                }
            }
        };

        block_retriever.ack_received(&block.block_hash).await;

        let repr = dag.get_representation().await;
        let has_all_deps = block.justifications.iter().all(|h| repr.contains(h));

        let mut parents_to_validate = BTreeSet::new();
        for hash in &block.justifications {
            if not_validated(&block_store, dag.as_ref(), hash).await {
                parents_to_validate.insert(*hash);
            }
        }

        if has_all_deps {
            let _ = out_tx.send(block.block_hash);
        } else {
            if !pending_requests.is_empty() {
                request_missing_dependencies(&pending_requests, block_retriever.as_ref()).await;
            }
            if !parents_to_validate.is_empty() {
                send_to_validate(&parents_to_validate, &block_store, put_to_incoming_queue.as_ref())
                    .await;
            }
        }
    }
}

/// Process validated blocks (port of `validatedBlocks`): update state and forward the next
/// dependency-free blocks to the output queue.
async fn validated_blocks(
    mut finished_processing_rx: mpsc::UnboundedReceiver<BlockMessage>,
    state: Arc<tokio::sync::Mutex<BlockReceiverState<BlockHash>>>,
    out_tx: mpsc::UnboundedSender<BlockHash>,
    log: Arc<dyn Log>,
) {
    let source = LogSource::new("casper.blocks.BlockReceiver");
    while let Some(block) = finished_processing_rx.recv().await {
        let parents: BTreeSet<BlockHash> = block.justifications.iter().copied().collect();
        let next = {
            let mut guard = state.lock().await;
            match guard.finished(block.block_hash, parents) {
                Ok((new_state, next)) => {
                    *guard = new_state;
                    next
                }
                Err(e) => {
                    log.error(source, &e);
                    continue;
                }
            }
        };
        for hash in next {
            let _ = out_tx.send(hash);
        }
    }
}

/// Wire the incoming and validated block streams to a shared validation queue (port of
/// `BlockReceiver.apply`). Returns the queue of block hashes ready for validation.
pub fn apply(
    state: Arc<tokio::sync::Mutex<BlockReceiverState<BlockHash>>>,
    incoming_blocks_rx: mpsc::UnboundedReceiver<BlockMessage>,
    finished_processing_rx: mpsc::UnboundedReceiver<BlockMessage>,
    conf_shard_name: String,
    block_store: BlockStore,
    dag: Arc<dyn BlockDagStorage>,
    block_retriever: Arc<BlockRetriever>,
    put_to_incoming_queue: Arc<dyn Fn(BlockMessage) + Send + Sync>,
    log: Arc<dyn Log>,
) -> mpsc::UnboundedReceiver<BlockHash> {
    let (out_tx, out_rx) = mpsc::unbounded_channel::<BlockHash>();

    tokio::spawn(incoming_blocks(
        incoming_blocks_rx,
        state.clone(),
        Arc::from(conf_shard_name),
        block_store,
        dag.clone(),
        block_retriever,
        put_to_incoming_queue,
        out_tx.clone(),
        log.clone(),
    ));
    tokio::spawn(validated_blocks(finished_processing_rx, state, out_tx, log));

    out_rx
}

#[cfg(test)]
mod tests {
    use super::*;

    type MId = String;

    fn parents(items: &[(&str, bool)]) -> Vec<(MId, bool)> {
        items
            .iter()
            .map(|(id, stored)| (id.to_string(), *stored))
            .collect()
    }

    #[test]
    fn begin_stored_true_if_unknown() {
        let (st, is_receiving) = BlockReceiverState::<MId>::new().begin_stored("A1".to_string());
        assert_eq!(st.receive_st, BTreeMap::from([("A1".to_string(), RecvStatus::BeginStoreBlock)]));
        assert!(is_receiving);
    }

    #[test]
    fn begin_stored_false_if_not_requested() {
        let (st, _) = BlockReceiverState::<MId>::new().begin_stored("A1".to_string());
        let (new_st, is_receiving) = st.begin_stored("A1".to_string());
        assert_eq!(new_st.receive_st, BTreeMap::from([("A1".to_string(), RecvStatus::BeginStoreBlock)]));
        assert!(!is_receiving);
    }

    #[test]
    fn begin_stored_true_if_requested() {
        let (st, _) = BlockReceiverState::<MId>::new().begin_stored("A2".to_string());
        let (new_st, _) = st
            .end_stored("A2".to_string(), parents(&[("A1", true)]))
            .unwrap();
        // Unseen parent A1 now has Requested status.
        let (_, is_receiving) = new_st.begin_stored("A1".to_string());
        assert!(is_receiving);
    }

    #[test]
    fn end_stored_errors_if_not_begin_store_block() {
        let (st, _) = BlockReceiverState::<MId>::new().begin_stored("A1".to_string());
        let (new_st, _) = st.end_stored("A1".to_string(), Vec::new()).unwrap();
        // A1 is now EndStoreBlock but should be BeginStoreBlock.
        assert!(new_st.end_stored("A1".to_string(), Vec::new()).is_err());
    }

    #[test]
    fn end_stored_updates_state_and_child_relations() {
        let (st, _) = BlockReceiverState::<MId>::new().begin_stored("A2".to_string());
        let (new_st, unseen_parents) = st
            .end_stored("A2".to_string(), parents(&[("A1", true)]))
            .unwrap();

        assert_eq!(st.receive_st.get("A2"), Some(&RecvStatus::BeginStoreBlock));
        assert_eq!(new_st.receive_st.get("A2"), Some(&RecvStatus::EndStoreBlock));

        assert!(!st.receive_st.contains_key("A1"));
        assert_eq!(new_st.receive_st.get("A1"), Some(&RecvStatus::Requested));
        assert_eq!(unseen_parents, BTreeSet::from(["A1".to_string()]));

        assert_eq!(
            new_st.blocks_st,
            BTreeMap::from([("A2".to_string(), BTreeSet::from(["A1".to_string()]))])
        );
        assert_eq!(
            new_st.child_relations,
            BTreeMap::from([("A1".to_string(), BTreeSet::from(["A2".to_string()]))])
        );
    }

    #[test]
    fn finished_errors_if_block_not_in_state() {
        assert!(
            BlockReceiverState::<MId>::new()
                .finished("A1".to_string(), BTreeSet::new())
                .is_err()
        );
    }

    #[test]
    fn finished_errors_if_block_not_received() {
        let (st, _) = BlockReceiverState::<MId>::new().begin_stored("A1".to_string());
        assert!(st.finished("A1".to_string(), BTreeSet::new()).is_err());
    }

    #[test]
    fn finished_returns_empty_state_if_all_processed() {
        let (st1, _) = BlockReceiverState::<MId>::new().begin_stored("A1".to_string());
        let (st2, _) = st1.end_stored("A1".to_string(), Vec::new()).unwrap();
        // A1 has no dependencies; finishing it removes it from the state.
        let (st3, _) = st2.finished("A1".to_string(), BTreeSet::new()).unwrap();
        assert!(st3.blocks_st.is_empty());
        assert!(st3.receive_st.is_empty());
        assert!(st3.child_relations.is_empty());
    }

    #[test]
    fn finished_removes_resolved_deps_and_returns_next() {
        let (st1, _) = BlockReceiverState::<MId>::new().begin_stored("A2".to_string());
        assert_eq!(st1.receive_st.get("A2"), Some(&RecvStatus::BeginStoreBlock));

        let (st2, a2_unseen) = st1
            .end_stored("A2".to_string(), parents(&[("A1", true)]))
            .unwrap();
        assert_eq!(st2.blocks_st.get("A2"), Some(&BTreeSet::from(["A1".to_string()])));
        assert_eq!(st2.receive_st.get("A2"), Some(&RecvStatus::EndStoreBlock));
        assert_eq!(st2.receive_st.get("A1"), Some(&RecvStatus::Requested));
        assert_eq!(st2.child_relations.get("A1"), Some(&BTreeSet::from(["A2".to_string()])));
        assert_eq!(a2_unseen, BTreeSet::from(["A1".to_string()]));

        let (st3, _) = st2.begin_stored("A1".to_string());
        assert_eq!(st3.receive_st.get("A1"), Some(&RecvStatus::BeginStoreBlock));

        let (st4, a1_unseen) = st3.end_stored("A1".to_string(), Vec::new()).unwrap();
        assert_eq!(st4.blocks_st.get("A1"), Some(&BTreeSet::new()));
        assert_eq!(st4.receive_st.get("A1"), Some(&RecvStatus::EndStoreBlock));
        assert!(a1_unseen.is_empty());

        // Finishing A1 removes it from receive state; child A2 becomes PendingValidation.
        let (st5, deps_validated) = st4.finished("A1".to_string(), BTreeSet::new()).unwrap();
        assert!(!st5.receive_st.contains_key("A1"));
        assert_eq!(st5.receive_st.get("A2"), Some(&RecvStatus::PendingValidation));
        assert_eq!(deps_validated, BTreeSet::from(["A2".to_string()]));
    }
}
