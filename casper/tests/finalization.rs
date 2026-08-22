//! Full-block-pipeline finalization test (CBC-Casper).
//!
//! Drives the real `create_genesis_block` → `BlockCreator` → `validate` → `insert` pipeline over an
//! in-memory DAG with multiple bonded validators, mirroring the Scala
//! `MultiParentCasperFinalizationSpec` round-robin scenario.

mod common;

use std::collections::BTreeSet;
use std::sync::Arc;

use rchain_block_storage::block_store::{self, BlockStore};
use rchain_block_storage::dag::codecs::{
    Blake2b256HashCodec, BlockHashCodec, BlockMetadataCodec, FringeDataCodec, SignedDeployDataCodec,
};
use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_block_storage::syntax::{insert_genesis, put_block};
use rchain_casper::block_metadata_store::BlockMetadataStore;
use rchain_casper::blocks::proposer::block_creator::BlockCreator;
use rchain_casper::blocks::proposer::propose_result::BlockCreatorResult;
use rchain_casper::dag::BlockDagKeyValueStorage;
use rchain_casper::genesis::contracts::{ProofOfStake, Registry, Validator as GenesisValidator};
use rchain_casper::genesis::{create_genesis_block, Genesis};
use rchain_casper::merging::BlockIndex;
use rchain_casper::multi_parent_casper;
use rchain_casper::runtime_manager::RuntimeManager;
use rchain_casper::validator_identity::ValidatorIdentity;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{BlockMessage, SignedDeployData};
use rchain_models::fringe_data::FringeData;
use rchain_shared::base16;
use rchain_shared::refined::NonNegI64;
use rchain_shared::store_manager::{database, InMemoryStoreManager};
use rchain_shared::typed_store::{BytesCodec, KeyValueTypedStore};

use common::build_runtime_manager;

const SHARD: &str = "root";

/// The blessed genesis terms recurse deeper than the default 2 MiB worker stack, so the finalization
/// test runs under a larger stack (matching the node binary + node integration tests).
fn test_runtime() -> tokio::runtime::Runtime {
    tokio::runtime::Builder::new_multi_thread()
        .thread_stack_size(32 * 1024 * 1024)
        .enable_all()
        .build()
        .expect("build test runtime")
}

fn identity(hex: &str) -> ValidatorIdentity {
    ValidatorIdentity::from_hex(hex).expect("validator identity")
}

async fn build_dag() -> Arc<BlockDagKeyValueStorage> {
    let manager = InMemoryStoreManager::default();
    let metadata_kv: Arc<dyn KeyValueTypedStore<BlockHash, BlockMetadata>> = Arc::new(
        database(
            &manager,
            "block-metadata",
            Arc::new(BlockHashCodec),
            Arc::new(BlockMetadataCodec),
        )
        .await
        .expect("metadata store"),
    );
    let metadata_store = Arc::new(
        BlockMetadataStore::create(metadata_kv)
            .await
            .expect("metadata store"),
    );
    let fringe_store: Arc<dyn KeyValueTypedStore<Blake2b256Hash, FringeData>> = Arc::new(
        database(
            &manager,
            "fringe-data",
            Arc::new(Blake2b256HashCodec),
            Arc::new(FringeDataCodec),
        )
        .await
        .expect("fringe store"),
    );
    let deploy_index: Arc<dyn KeyValueTypedStore<Vec<u8>, BlockHash>> = Arc::new(
        database(
            &manager,
            "deploy-index",
            Arc::new(BytesCodec),
            Arc::new(BlockHashCodec),
        )
        .await
        .expect("deploy index"),
    );
    let deploy_store: Arc<dyn KeyValueTypedStore<Vec<u8>, SignedDeployData>> = Arc::new(
        database(
            &manager,
            "deploy-pool",
            Arc::new(BytesCodec),
            Arc::new(SignedDeployDataCodec),
        )
        .await
        .expect("deploy store"),
    );
    Arc::new(
        BlockDagKeyValueStorage::create(metadata_store, fringe_store, deploy_index, deploy_store)
            .await
            .expect("dag storage"),
    )
}

async fn build_block_store() -> BlockStore {
    block_store::create(&InMemoryStoreManager::default())
        .await
        .expect("block store")
}

/// Create + store the genesis block, bonding `validators` with `stakes` and signing with the first.
async fn make_genesis(
    runtime: &RuntimeManager,
    dag: &Arc<BlockDagKeyValueStorage>,
    block_store: &BlockStore,
    validators: &[ValidatorIdentity],
    stakes: &[i64],
) -> BlockMessage {
    let pos_validators: Vec<GenesisValidator> = validators
        .iter()
        .zip(stakes)
        .map(|(v, s)| GenesisValidator {
            pk: v.public_key.clone(),
            stake: NonNegI64::try_from(*s).unwrap(),
        })
        .collect();
    let pub_hex = base16::encode(validators[0].public_key.bytes());
    let genesis = Genesis {
        sender: validators[0].public_key.clone(),
        shard_id: SHARD.to_string(),
        block_number: 0,
        proof_of_stake: ProofOfStake {
            minimum_bond: 1,
            maximum_bond: 1_000_000,
            validators: pos_validators,
            epoch_length: 10_000,
            quarantine_length: 50_000,
            number_of_active_validators: 100,
            pos_multi_sig_public_keys: vec![],
            pos_multi_sig_quorum: 0,
            pos_vault_pub_key: pub_hex.clone(),
        },
        registry: Registry {
            system_contract_pub_key: pub_hex,
        },
        vaults: vec![],
    };
    let block = create_genesis_block(&validators[0], &genesis, runtime)
        .await
        .expect("create genesis block");
    put_block(block_store, block.clone()).await.expect("put genesis");
    insert_genesis(dag.as_ref(), block.clone()).await.expect("insert genesis");
    block
}

/// Propose a block from `validator` (attestation; no user deploys).
async fn propose(
    runtime: &RuntimeManager,
    dag: &Arc<BlockDagKeyValueStorage>,
    block_store: &BlockStore,
    validator: &ValidatorIdentity,
) -> BlockMessage {
    let block_index = |hash: BlockHash| BlockIndex::get_block_index(runtime, block_store, hash);
    let pre_state = multi_parent_casper::get_pre_state_for_new_block(
        dag.as_ref(),
        block_store,
        runtime,
        &block_index,
    )
    .await
    .expect("pre-state");
    let creator = BlockCreator {
        id: validator.clone(),
        shard_id: SHARD.to_string(),
    };
    let block = match creator
        .create(runtime, dag.as_ref(), &pre_state, &[], &BTreeSet::new(), false, false)
        .await
        .expect("create block")
    {
        BlockCreatorResult::Created(b) => b,
        BlockCreatorResult::NoNewDeploys => panic!("expected a block"),
    };
    put_block(block_store, block.clone()).await.expect("put block");
    let metadata = multi_parent_casper::validate(
        dag.as_ref(),
        block_store,
        runtime,
        &block,
        SHARD,
        0,
        &block_index,
    )
    .await
    .expect("validate block");
    dag.insert(metadata, block.clone()).await.expect("insert block");
    block
}

/// The `Finalizer` only advances the fringe on a *fork* structure: the justifications must sit two or
/// more layers above the candidate next-fringe, so `calculate_next_fringe_support_map` sees parents
/// outside the next layer. A lockstep DAG (the block pipeline's `latest_msgs` proposer always builds
/// one) never advances the fringe — the Scala `MultiParentCasperFinalizationSpec` is itself `ignore`d
/// with a "TODO adjust/remove when new finalizer test is implemented" note. This drives the fork
/// structure through `DagMessageState` directly and asserts the fringe advances past genesis.
#[test]
fn fork_structure_advances_fringe() {
    use rchain_block_storage::dag::message_state::DagMessageState;
    use rchain_models::validator::Validator;
    use rchain_shared::refined::{BlockHeight, SeqNum};

    fn validator(byte: u8) -> Validator {
        Validator::new([byte; 65])
    }
    fn id(sender: u8, height: i64) -> BlockHash {
        let mut bytes = [0u8; 32];
        bytes[0] = sender;
        bytes[1] = (height & 0xff) as u8;
        bytes[2] = ((height >> 8) & 0xff) as u8;
        BlockHash::new(bytes)
    }
    fn h(n: i64) -> BlockHeight {
        BlockHeight::try_from(n).expect("height")
    }
    fn s(n: i64) -> SeqNum {
        SeqNum::try_from(n).expect("seq num")
    }

    let v0 = validator(0);
    let v1 = validator(1);
    let v2 = validator(2);
    // The genesis sender is not a bonded validator, so a bonded validator's self-parent chain never
    // reaches genesis (mirrors the Scala genesis being signed by a distinct "genesis validator").
    let g = validator(255);
    let bonds: std::collections::BTreeMap<Validator, NonNegI64> = [
        (v0.clone(), NonNegI64::try_from(10).unwrap()),
        (v1.clone(), NonNegI64::try_from(10).unwrap()),
        (v2.clone(), NonNegI64::try_from(10).unwrap()),
    ]
    .into_iter()
    .collect();

    let st: DagMessageState<BlockHash, Validator> = DagMessageState::empty();
    let genesis =
        st.create_message(id(255, 0), h(0), g, s(0), bonds.clone(), &BTreeSet::new());
    let st = st.insert_msg(&genesis);

    // Layer 1: a three-way fork — each validator sees only genesis.
    let a1 = st.create_message(
        id(0, 1), h(1), v0.clone(), s(1), bonds.clone(),
        &[genesis.clone()].into_iter().collect(),
    );
    let st = st.insert_msg(&a1);
    let b1 = st.create_message(
        id(1, 1), h(1), v1.clone(), s(1), bonds.clone(),
        &[genesis.clone()].into_iter().collect(),
    );
    let st = st.insert_msg(&b1);
    let c1 = st.create_message(
        id(2, 1), h(1), v2.clone(), s(1), bonds.clone(),
        &[genesis.clone()].into_iter().collect(),
    );
    let st = st.insert_msg(&c1);

    // Layer 2: convergence — each sees all of layer 1.
    let l1: BTreeSet<_> = [a1.clone(), b1.clone(), c1.clone()].into_iter().collect();
    let a2 = st.create_message(id(0, 2), h(2), v0.clone(), s(2), bonds.clone(), &l1);
    let st = st.insert_msg(&a2);
    let b2 = st.create_message(id(1, 2), h(2), v1.clone(), s(2), bonds.clone(), &l1);
    let st = st.insert_msg(&b2);
    let c2 = st.create_message(id(2, 2), h(2), v2.clone(), s(2), bonds.clone(), &l1);
    let st = st.insert_msg(&c2);

    // Layer 3: convergence — each sees all of layer 2.
    let l2: BTreeSet<_> = [a2.clone(), b2.clone(), c2.clone()].into_iter().collect();
    let a3 = st.create_message(id(0, 3), h(3), v0.clone(), s(3), bonds.clone(), &l2);
    let st = st.insert_msg(&a3);
    let b3 = st.create_message(id(1, 3), h(3), v1.clone(), s(3), bonds.clone(), &l2);
    let st = st.insert_msg(&b3);
    let c3 = st.create_message(id(2, 3), h(3), v2.clone(), s(3), bonds.clone(), &l2);
    let st = st.insert_msg(&c3);

    // Layer 4: the finalizing message sees all of layer 3 → the layer-1 fork is finalized.
    let l3: BTreeSet<_> = [a3.clone(), b3.clone(), c3.clone()].into_iter().collect();
    let a4 = st.create_message(id(0, 4), h(4), v0.clone(), s(4), bonds.clone(), &l3);

    assert!(!a4.fringe.is_empty(), "fringe should advance past the empty genesis fringe");
    assert_eq!(
        a4.fringe,
        [id(0, 1), id(1, 1), id(2, 1)].into_iter().collect(),
        "fringe should be the layer-1 fork"
    );
}

/// Scala `MultiParentCasperFinalizationSpec`: four equal-stake validators, three proposers in
/// round-robin; the finalized set advances as a super-majority attests to a common prefix.
#[test]
#[ignore = "lockstep/round-robin DAG never advances the fringe (Scala spec is also `ignore`d); see fork_structure_advances_fringe for the finalizing shape"]
fn round_robin_finalizes_common_prefix() {
    let rt = test_runtime();
    // The blessed genesis terms recurse deeper than the main test thread's stack allows, so run the
    // scenario on a 32 MiB worker thread (the same reason the node tests use `test_runtime`).
    let handle = rt.spawn(async move {
        let runtime = build_runtime_manager().await;
        let dag = build_dag().await;
        let block_store = build_block_store().await;

        // Four equal-stake validators (total 40; super-majority is > 2/3 = > 26.67, so 3 suffice).
        let ids = [
            identity("5a0bde2f5857124b1379c78535b07a278e3b9cefbcacc02e62ab3294c02765a1"),
            identity("867c21c6a3245865444d80e49cac08a1c11e23b35965b566bbe9f49bb9897511"),
            identity("5248f8913f8572d8227a3c7787b54bd8263389f7209adc1422e36bb2beb160dc"),
            identity("e33c9f1e925819d04733db4ec8539a84507c9e9abd32822059349449fe03997d"),
        ];
        let genesis = make_genesis(&runtime, &dag, &block_store, &ids, &[10, 10, 10, 10]).await;

        // Round-robin proposals across the three non-genesis validators.
        let proposers = [&ids[1], &ids[2], &ids[3]];
        let mut blocks = vec![genesis.clone()];
        for i in 0..8 {
            let b = propose(&runtime, &dag, &block_store, proposers[i % 3]).await;
            blocks.push(b);
        }

        let repr = dag.get_representation().await;
        // The finalized set is the seen-closure of the latest fringe; it must grow past the genesis.
        let finalized = repr.finalized_blocks_set();
        assert!(!finalized.is_empty(), "finalized set should be non-empty");
        assert!(
            finalized.contains(&genesis.block_hash),
            "genesis should be finalized after a super-majority round"
        );
        // The last-finalized block is well-defined.
        let _last = repr.last_finalized_block_hash().expect("last finalized block");
    });
    rt.block_on(handle).unwrap();
}
