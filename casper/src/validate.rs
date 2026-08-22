//! Block validation predicates (port of `Validate.scala`) — the pure, effect-free checks.

use rchain_models::block_hash::BlockHash;
use rchain_models::block_version::SUPPORTED;
use rchain_models::casper::protocol::casper_message::BlockMessage;

use crate::block_status::BlockStatus;
use crate::proto_util::hash_block;

/// Validate that the block's identifying fields are non-empty (port of `formatOfFields`).
pub fn format_of_fields(b: &BlockMessage) -> bool {
    if b.block_hash == BlockHash::new([0u8; 32]) {
        false
    } else if b.sig.is_empty() {
        false
    } else if b.sig_algorithm.is_empty() {
        false
    } else if b.shard_id.is_empty() {
        false
    } else if !b.shard_id.is_ascii() {
        // Non-ASCII shard ids are rejected at ingress rather than asserted inside
        // `BlockRandomSeed::new` (which only `debug_assert!`s the invariant).
        false
    } else {
        true
    }
}

/// Validate that the block version is supported (port of `version`).
pub fn version(b: &BlockMessage) -> bool {
    SUPPORTED.contains(&b.version)
}

/// Validate that the block hash matches its content-addressed value (Law 16; port of `blockHash`).
pub fn block_hash(b: &BlockMessage) -> bool {
    b.block_hash == hash_block(b)
}

/// Validate the block signature against the sender's public key (port of `blockSignature`).
pub fn block_signature(b: &BlockMessage) -> bool {
    match rchain_crypto::signatures::signatures_alg::from_algorithm(&b.sig_algorithm) {
        Some(alg) => alg.verify(b.block_hash.as_bytes(), &b.sig, b.sender.as_bytes()),
        None => false,
    }
}

/// Validate that no deploy is scheduled for a future block (port of `futureTransaction`).
pub fn future_transaction(b: &BlockMessage) -> BlockStatus {
    if b.state
        .deploys
        .iter()
        .any(|d| d.deploy.data.valid_after_block_number > i64::from(b.block_number))
    {
        BlockStatus::ContainsFutureDeploy
    } else {
        BlockStatus::Valid
    }
}

/// Validate that no deploy has expired (port of `transactionExpiration`).
pub fn transaction_expiration(b: &BlockMessage, expiration_threshold: i64) -> BlockStatus {
    let earliest = b.block_number - expiration_threshold;
    if b.state
        .deploys
        .iter()
        .any(|d| d.deploy.data.valid_after_block_number <= earliest)
    {
        BlockStatus::ContainsExpiredDeploy
    } else {
        BlockStatus::Valid
    }
}

/// Validate that all deploys belong to the validator's shard (port of `deploysShardIdentifier`).
pub fn deploys_shard_identifier(b: &BlockMessage, shard_id: &str) -> BlockStatus {
    if b.state.deploys.iter().all(|d| d.deploy.data.shard_id == shard_id) {
        BlockStatus::Valid
    } else {
        BlockStatus::InvalidDeployShardId
    }
}

/// Validate that all deploys meet the minimum phlo price (port of `phloPrice`).
pub fn phlo_price(b: &BlockMessage, min_phlo_price: i64) -> BlockStatus {
    if b.state
        .deploys
        .iter()
        .all(|d| d.deploy.data.phlo_price >= min_phlo_price)
    {
        BlockStatus::Valid
    } else {
        BlockStatus::ContainsLowCostDeploy
    }
}

// --- Effectful checks (depend on the block DAG) ------------------------------------------------

use std::collections::{BTreeMap, BTreeSet, HashSet, VecDeque};

use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_block_storage::dag::finalizer::Message;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::validator::Validator;

use crate::proto_util::{
    get_parent_metadatas_above_block_number, get_parents_metadata, max_block_number_metadata,
};
use crate::runtime_manager::RuntimeManager;

/// A block-validation outcome: `Ok(())` is valid, `Err(status)` is the invalid status (port of
/// `ValidBlockProcessing`).
pub type ValidBlockProcessing = Result<(), BlockStatus>;

/// Validate the block number is one more than the maximum non-failed parent number (port of
/// `blockNumber`).
pub async fn block_number(
    dag: &dyn BlockDagStorage,
    b: &BlockMessage,
) -> Result<ValidBlockProcessing, String> {
    let mut max_block_number = -1i64;
    for j in &b.justifications {
        let meta = dag
            .lookup(j)
            .await?
            .ok_or_else(|| format!("missing justification {}", j.to_hex()))?;
        if !meta.validation_failed {
            max_block_number = max_block_number.max(i64::from(meta.block_num));
        }
    }
    if max_block_number + 1 == i64::from(b.block_number) {
        Ok(Ok(()))
    } else {
        Ok(Err(BlockStatus::InvalidBlockNumber))
    }
}

/// Validate the sender's sequence number is one more than its latest justification's (port of
/// `sequenceNumber`).
pub async fn sequence_number(
    dag: &dyn BlockDagStorage,
    b: &BlockMessage,
) -> Result<ValidBlockProcessing, String> {
    let mut creator_latest_seq = -1i64;
    for j in &b.justifications {
        let meta = dag
            .lookup(j)
            .await?
            .ok_or_else(|| format!("missing justification {}", j.to_hex()))?;
        if meta.sender == b.sender {
            creator_latest_seq = creator_latest_seq.max(i64::from(meta.seq_num));
        }
    }
    if creator_latest_seq + 1 == i64::from(b.seq_num) {
        Ok(Ok(()))
    } else {
        Ok(Err(BlockStatus::InvalidSequenceNumber))
    }
}

/// Validate there is no justification regression (port of `justificationRegressions`).
pub async fn justification_regressions(
    dag: &dyn BlockDagStorage,
    b: &BlockMessage,
) -> Result<ValidBlockProcessing, String> {
    let valid = check_justification_regression(dag, b).await?.unwrap_or(true);
    if valid {
        Ok(Ok(()))
    } else {
        Ok(Err(BlockStatus::JustificationRegression))
    }
}

async fn check_justification_regression(
    dag: &dyn BlockDagStorage,
    b: &BlockMessage,
) -> Result<Option<bool>, String> {
    let repr = dag.get_representation().await;
    let msg_map: &BTreeMap<BlockHash, Message<BlockHash, Validator>> =
        &repr.dag_message_state.msg_map;

    // `justifications.map(msgMap.get).sequence` — None if any is missing (see the Scala TODO).
    let justifications: Option<Vec<Message<BlockHash, Validator>>> = b
        .justifications
        .iter()
        .map(|j| msg_map.get(j).cloned())
        .collect();
    let justifications = match justifications {
        Some(js) => js,
        None => return Ok(None),
    };

    let prev_msg = match justifications.iter().find(|m| m.sender == b.sender) {
        Some(m) => m,
        None => return Ok(None),
    };

    let res = justifications.iter().all(|just| {
        let just_prev_msg = prev_msg
            .parents
            .iter()
            .filter_map(|p| msg_map.get(p))
            .find(|m| m.sender == just.sender);
        match just_prev_msg {
            Some(just_prev_msg) => just_prev_msg.seen.difference(&just.seen).next().is_none(),
            None => true,
        }
    });
    Ok(Some(res))
}

/// Validate that a block does not neglect an invalid-but-still-bonded justification (port of
/// `neglectedInvalidBlock`).
pub async fn neglected_invalid_block(
    dag: &dyn BlockDagStorage,
    b: &BlockMessage,
) -> Result<ValidBlockProcessing, String> {
    let mut justifications = Vec::new();
    for j in &b.justifications {
        if let Some(meta) = dag.lookup(j).await? {
            justifications.push(meta);
        }
    }
    let neglected = justifications
        .iter()
        .filter(|m| m.validation_failed)
        .map(|m| m.sender)
        .any(|v| b.bonds.get(&v).map(|&stake| i64::from(stake) > 0).unwrap_or(false));
    if neglected {
        Ok(Err(BlockStatus::NeglectedInvalidBlock))
    } else {
        Ok(Ok(()))
    }
}

/// Look up a block from the block store, failing if absent (port of `BlockStore.getUnsafe`).
async fn get_block_unsafe(block_store: &BlockStore, hash: &BlockHash) -> Result<BlockMessage, String> {
    let mut vals = block_store.get(&[*hash]).await?;
    vals.pop()
        .flatten()
        .ok_or_else(|| format!("missing block {}", hash.to_hex()))
}

/// Validate that no deploy with the same sig has been produced in the chain within the expiration
/// window (port of `repeatDeploy`).
pub async fn repeat_deploy(
    dag: &dyn BlockDagStorage,
    block_store: &BlockStore,
    block: &BlockMessage,
    expiration_threshold: i64,
) -> Result<ValidBlockProcessing, String> {
    let deploy_key_set: BTreeSet<Vec<u8>> =
        block.state.deploys.iter().map(|d| d.deploy.sig.clone()).collect();

    let block_metadata = BlockMetadata::from_block(block);
    let init_parents = get_parents_metadata(dag, &block_metadata).await?;
    let max_block_number = max_block_number_metadata(&init_parents);
    let earliest_block_number = max_block_number + 1 - expiration_threshold;

    // Breadth-first traversal of the parent chain above the expiration horizon (port of
    // `DagOps.bfTraverseF(...).findF(...)`).
    let mut queue: VecDeque<BlockMetadata> = init_parents.into_iter().collect();
    let mut visited: HashSet<BlockHash> = HashSet::new();
    while let Some(curr) = queue.pop_front() {
        if visited.contains(&curr.block_hash) {
            continue;
        }
        visited.insert(curr.block_hash);

        let b = get_block_unsafe(block_store, &curr.block_hash).await?;
        if b.state.deploys.iter().any(|d| deploy_key_set.contains(&d.deploy.sig)) {
            return Ok(Err(BlockStatus::InvalidRepeatDeploy));
        }

        let parents =
            get_parent_metadatas_above_block_number(dag, &curr, earliest_block_number).await?;
        for p in parents {
            if !visited.contains(&p.block_hash) {
                queue.push_back(p);
            }
        }
    }
    Ok(Ok(()))
}

/// Validate that the block's bond cache matches the proof-of-stake contract's bonds at the post
/// state (port of `bondsCache`).
pub async fn bonds_cache(
    runtime: &RuntimeManager,
    block: &BlockMessage,
) -> Result<ValidBlockProcessing, String> {
    let tuplespace_hash = block.post_state_hash;
    let computed_bonds = runtime.compute_bonds(&tuplespace_hash).await?;
    if block.bonds == computed_bonds {
        Ok(Ok(()))
    } else {
        Ok(Err(BlockStatus::InvalidBondsCache))
    }
}

/// Compose the effectful + pure checks (port of `blockSummary`).
pub async fn block_summary(
    dag: &dyn BlockDagStorage,
    block_store: &BlockStore,
    block: &BlockMessage,
    shard_id: &str,
    expiration_threshold: i64,
) -> Result<ValidBlockProcessing, String> {
    if let Err(status) = justification_regressions(dag, block).await? {
        return Ok(Err(status));
    }
    if let Err(status) = sequence_number(dag, block).await? {
        return Ok(Err(status));
    }
    if let Err(status) = block_number(dag, block).await? {
        return Ok(Err(status));
    }
    let pure = [
        deploys_shard_identifier(block, shard_id),
        future_transaction(block),
        transaction_expiration(block, expiration_threshold),
    ];
    for status in pure {
        if !status.is_valid() {
            return Ok(Err(status));
        }
    }
    repeat_deploy(dag, block_store, block, expiration_threshold).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{BTreeMap, BTreeSet};

    use rchain_models::casper::protocol::casper_message::{
        DeployData, ProcessedDeploy, RholangState, SignedDeployData, PCost,
    };
    use rchain_models::validator::Validator;

    fn deploy(valid_after: i64, phlo_price: i64, shard_id: &str) -> ProcessedDeploy {
        ProcessedDeploy {
            deploy: SignedDeployData {
                data: DeployData {
                    term: "Nil".to_string(),
                    timestamp: 0,
                    phlo_price,
                    phlo_limit: 100,
                    valid_after_block_number: valid_after,
                    shard_id: shard_id.to_string(),
                },
                deployer: vec![],
                sig: vec![1],
                sig_algorithm: "secp256k1".to_string(),
            },
            cost: PCost { cost: 0 },
            deploy_log: vec![],
            is_failed: false,
            system_deploy_error: None,
        }
    }

    fn block() -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: BlockHash::new([0xab; 32]),
            block_number: 10.try_into().unwrap(),
            sender: Validator::new([0x11; 65]),
            seq_num: 0.try_into().unwrap(),
            pre_state_hash: rchain_models::block::state_hash::StateHash::new([1u8; 32]),
            post_state_hash: rchain_models::block::state_hash::StateHash::new([2u8; 32]),
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
            sig: vec![1],
        }
    }

    #[test]
    fn version_and_format_checks() {
        let mut b = block();
        assert!(version(&b));
        b.version = 2;
        assert!(!version(&b));
        b.version = 1;
        assert!(format_of_fields(&b));
        b.sig = vec![];
        assert!(!format_of_fields(&b));
    }

    #[test]
    fn format_of_fields_rejects_non_ascii_shard_id() {
        let mut b = block();
        b.shard_id = "røøt".to_string();
        assert!(!format_of_fields(&b));
    }

    #[test]
    fn block_hash_detects_tampering() {
        let mut b = block();
        let h = hash_block(&b);
        b.block_hash = h;
        assert!(block_hash(&b));
        b.block_number = 999.try_into().unwrap();
        assert!(!block_hash(&b));
    }

    #[test]
    fn deploy_validators() {
        let mut b = block();
        b.state.deploys = vec![deploy(5, 10, "root")];
        assert_eq!(future_transaction(&b), BlockStatus::Valid);
        assert_eq!(transaction_expiration(&b, 100), BlockStatus::Valid);
        assert_eq!(deploys_shard_identifier(&b, "root"), BlockStatus::Valid);
        assert_eq!(phlo_price(&b, 10), BlockStatus::Valid);

        b.state.deploys = vec![deploy(20, 10, "root")];
        assert_eq!(future_transaction(&b), BlockStatus::ContainsFutureDeploy);

        b.state.deploys = vec![deploy(0, 10, "other")];
        assert_eq!(deploys_shard_identifier(&b, "root"), BlockStatus::InvalidDeployShardId);

        b.state.deploys = vec![deploy(5, 1, "root")];
        assert_eq!(phlo_price(&b, 10), BlockStatus::ContainsLowCostDeploy);
    }
}

#[cfg(test)]
mod effectful_tests {
    use super::*;
    use async_trait::async_trait;
    use rchain_block_storage::dag::codecs::{BlockHashCodec, BlockMessageCodec};
    use rchain_block_storage::dag::dag_storage::DeployId;
    use rchain_block_storage::dag::message_state::DagMessageState;
    use rchain_block_storage::dag::representation::DagRepresentation;
    use rchain_models::block_metadata::BlockMetadata;
    use rchain_models::casper::protocol::casper_message::{
        DeployData, PCost, ProcessedDeploy, SignedDeployData,
    };
    use rchain_shared::store::InMemoryKeyValueStore;
    use rchain_shared::typed_store::KeyValueTypedStoreCodec;
    use std::collections::BTreeSet;
    use std::sync::Arc;

    fn hash(byte: u8) -> BlockHash {
        let mut bytes = [0u8; 32];
        bytes[0] = byte;
        BlockHash::new(bytes)
    }

    fn meta(hash: BlockHash, block_num: i64, sender_byte: u8, seq: i64, failed: bool) -> BlockMetadata {
        BlockMetadata {
            block_hash: hash,
            block_num: rchain_shared::refined::BlockHeight::try_from(block_num).unwrap(),
            sender: Validator::new([sender_byte; 65]),
            seq_num: rchain_shared::refined::SeqNum::try_from(seq).unwrap(),
            justifications: BTreeSet::new(),
            bonds_map: BTreeMap::new(),
            validated: true,
            validation_failed: failed,
            fringe: BTreeSet::new(),
            fringe_state_hash: rchain_models::block::state_hash::StateHash::new([0u8; 32]),
            member_of_fringe: None,
        }
    }

    struct MockDag {
        metadata: BTreeMap<BlockHash, BlockMetadata>,
        representation: DagRepresentation,
    }

    #[async_trait]
    impl BlockDagStorage for MockDag {
        async fn get_representation(&self) -> DagRepresentation {
            self.representation.clone()
        }
        async fn insert(&self, _m: BlockMetadata, _b: BlockMessage) -> Result<(), String> {
            Ok(())
        }
        async fn lookup(&self, h: &BlockHash) -> Result<Option<BlockMetadata>, String> {
            Ok(self.metadata.get(h).cloned())
        }
        async fn lookup_by_deploy_id(&self, _d: &DeployId) -> Result<Option<BlockHash>, String> {
            Ok(None)
        }
        async fn add_deploy(&self, _d: SignedDeployData) -> Result<(), String> {
            Ok(())
        }
        async fn pooled_deploys(&self) -> Result<BTreeMap<DeployId, SignedDeployData>, String> {
            Ok(BTreeMap::new())
        }
        async fn contains_deploy_in_pool(&self, _d: &DeployId) -> Result<bool, String> {
            Ok(false)
        }
    }

    fn mock(metadata: BTreeMap<BlockHash, BlockMetadata>) -> MockDag {
        MockDag {
            metadata,
            representation: DagRepresentation {
                dag_set: BTreeSet::new(),
                child_map: BTreeMap::new(),
                height_map: BTreeMap::new(),
                dag_message_state: DagMessageState::empty(),
                fringe_states: BTreeMap::new(),
            },
        }
    }

    fn block(sender_byte: u8, block_num: i64, seq: i64, justifications: Vec<BlockHash>) -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: hash(0xee),
            block_number: rchain_shared::refined::BlockHeight::try_from(block_num).unwrap(),
            sender: Validator::new([sender_byte; 65]),
            seq_num: rchain_shared::refined::SeqNum::try_from(seq).unwrap(),
            pre_state_hash: rchain_models::block::state_hash::StateHash::new([1u8; 32]),
            post_state_hash: rchain_models::block::state_hash::StateHash::new([2u8; 32]),
            justifications,
            bonds: BTreeMap::new(),
            rejected_deploys: BTreeSet::new(),
            rejected_blocks: BTreeSet::new(),
            rejected_senders: BTreeSet::new(),
            state: rchain_models::casper::protocol::casper_message::RholangState::default(),
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![1],
        }
    }

    #[tokio::test]
    async fn block_number_must_be_parent_max_plus_one() {
        let parent = hash(1);
        let dag = mock(BTreeMap::from([(parent, meta(parent, 4, 1, 0, false))]));
        let b = block(2, 5, 0, vec![parent]);
        assert_eq!(block_number(&dag, &b).await.unwrap(), Ok(()));

        let bad = block(2, 6, 0, vec![parent]);
        assert_eq!(
            block_number(&dag, &bad).await.unwrap(),
            Err(BlockStatus::InvalidBlockNumber)
        );
    }

    #[tokio::test]
    async fn sequence_number_must_be_creator_latest_plus_one() {
        let parent = hash(1);
        let dag = mock(BTreeMap::from([(parent, meta(parent, 4, 1, 2, false))]));
        let b = block(1, 5, 3, vec![parent]);
        assert_eq!(sequence_number(&dag, &b).await.unwrap(), Ok(()));

        let bad = block(1, 5, 4, vec![parent]);
        assert_eq!(
            sequence_number(&dag, &bad).await.unwrap(),
            Err(BlockStatus::InvalidSequenceNumber)
        );
    }

    #[tokio::test]
    async fn neglected_invalid_block_detects_bonded_invalid_justification() {
        let invalid = hash(1);
        let dag = mock(BTreeMap::from([(invalid, meta(invalid, 0, 1, 0, true))]));
        let mut b = block(2, 1, 0, vec![invalid]);
        b.bonds.insert(Validator::new([1u8; 65]), 100.try_into().unwrap());
        assert_eq!(
            neglected_invalid_block(&dag, &b).await.unwrap(),
            Err(BlockStatus::NeglectedInvalidBlock)
        );

        b.bonds.clear();
        assert_eq!(neglected_invalid_block(&dag, &b).await.unwrap(), Ok(()));
    }

    fn deploy(sig: u8) -> ProcessedDeploy {
        ProcessedDeploy {
            deploy: SignedDeployData {
                data: DeployData {
                    term: "Nil".to_string(),
                    timestamp: 0,
                    phlo_price: 1,
                    phlo_limit: 1,
                    valid_after_block_number: 0,
                    shard_id: "root".to_string(),
                },
                deployer: vec![],
                sig: vec![sig],
                sig_algorithm: "secp256k1".to_string(),
            },
            cost: PCost { cost: 0 },
            deploy_log: vec![],
            is_failed: false,
            system_deploy_error: None,
        }
    }

    async fn block_store(blocks: Vec<BlockMessage>) -> BlockStore {
        let store: BlockStore = Arc::new(KeyValueTypedStoreCodec::new(
            Arc::new(tokio::sync::Mutex::new(Box::new(
                InMemoryKeyValueStore::default(),
            ))),
            Arc::new(BlockHashCodec),
            Arc::new(BlockMessageCodec),
        ));
        let pairs: Vec<(BlockHash, BlockMessage)> =
            blocks.into_iter().map(|b| (b.block_hash, b)).collect();
        store.put(&pairs).await.unwrap();
        store
    }

    #[tokio::test]
    async fn repeat_deploy_detects_duplicate_sig_in_parent_chain() {
        let genesis = hash(1);
        let parent = hash(2);
        let dag = mock(BTreeMap::from([
            (genesis, meta(genesis, 0, 1, 0, false)),
            (parent, meta(parent, 1, 1, 1, false)),
        ]));

        let mut genesis_block = block(1, 0, 0, vec![]);
        genesis_block.block_hash = genesis;
        genesis_block.state.deploys = vec![deploy(9)];
        let mut parent_block = block(1, 1, 1, vec![genesis]);
        parent_block.block_hash = parent;
        parent_block.state.deploys = vec![deploy(1)];
        let store = block_store(vec![genesis_block, parent_block]).await;

        // Current block reuses the parent's deploy sig [1].
        let mut current = block(1, 2, 2, vec![parent]);
        current.state.deploys = vec![deploy(1)];
        assert_eq!(
            repeat_deploy(&dag, &store, &current, 100).await.unwrap(),
            Err(BlockStatus::InvalidRepeatDeploy)
        );

        // Current block with a fresh deploy sig is valid.
        let mut current = block(1, 2, 2, vec![parent]);
        current.state.deploys = vec![deploy(7)];
        assert_eq!(
            repeat_deploy(&dag, &store, &current, 100).await.unwrap(),
            Ok(())
        );
    }
}
