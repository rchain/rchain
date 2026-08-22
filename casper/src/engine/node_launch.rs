//! Node launch (port of `engine/NodeLaunch.scala`).
//!
//! The genesis-from-config helpers and the `apply` mode-dispatch state machine (genesis → syncing →
//! running over the packet stream) are ported here.

use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use rchain_block_storage::approved_store::ApprovedStore;
use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_block_storage::syntax::{insert_genesis, put_approved_block, put_block};
use rchain_comm::peer_node::PeerNode;
use rchain_comm::rp::rp_conf::RPConf;
use rchain_comm::transport::transport_layer::TransportLayer;
use rchain_models::casper::protocol::casper_message::{
    BlockMessage, CasperMessage, FinalizedFringe,
};
use rchain_models::casper::protocol::packet_type_tag::ToPacket;
use rchain_rspace::state::{RSpaceExporter, RSpaceImporter};
use rchain_shared::log::{Log, LogSource};
use tokio::sync::mpsc;

use crate::blocks::block_retriever::BlockRetriever;
use crate::bonds_parser;
use crate::conf::CasperConf;
use crate::engine::node_running::NodeRunning;
use crate::engine::node_syncing::NodeSyncing;
use crate::genesis::contracts::{ProofOfStake, Registry, Validator};
use crate::genesis::Genesis;
use crate::protocol::casper_message_protocol::FinalizedFringeSerde;
use crate::protocol::comm_util::{CommUtil, ConnectionsCell};
use crate::runtime_manager::RuntimeManager;
use crate::validator_identity::ValidatorIdentity;
use crate::vault_parser;

/// A peer message (port of `NodeLaunch.PeerMessage`).
#[derive(Clone, Debug)]
pub struct PeerMessage {
    pub peer: PeerNode,
    pub message: CasperMessage,
}

/// Create the genesis block from raw config values (port of `NodeLaunch.createGenesisBlock`).
#[allow(clippy::too_many_arguments)]
pub async fn create_genesis_block(
    validator: &ValidatorIdentity,
    shard_id: &str,
    block_number: i64,
    bonds_path: &str,
    autogen_shard_size: i32,
    vaults_path: &str,
    minimum_bond: i64,
    maximum_bond: i64,
    epoch_length: i32,
    quarantine_length: i32,
    number_of_active_validators: i32,
    pos_multi_sig_public_keys: &[String],
    pos_multi_sig_quorum: i32,
    pos_vault_pub_key: &str,
    system_contract_pub_key: &str,
    runtime: &RuntimeManager,
) -> Result<BlockMessage, String> {
    // Initial REV vaults.
    let vaults = vault_parser::parse(Path::new(vaults_path))?;

    // Initial validators.
    let bonds = bonds_parser::parse_or_generate(Path::new(bonds_path), autogen_shard_size)?;
    let validators: Vec<Validator> = bonds
        .into_iter()
        .map(|(pk, stake)| Validator { pk, stake })
        .collect();

    // Run the genesis deploys and create the block.
    let genesis = Genesis {
        sender: validator.public_key.clone(),
        shard_id: shard_id.to_string(),
        block_number,
        proof_of_stake: ProofOfStake {
            minimum_bond,
            maximum_bond,
            validators,
            epoch_length,
            quarantine_length,
            number_of_active_validators,
            pos_multi_sig_public_keys: pos_multi_sig_public_keys.to_vec(),
            pos_multi_sig_quorum,
            pos_vault_pub_key: pos_vault_pub_key.to_string(),
        },
        registry: Registry {
            system_contract_pub_key: system_contract_pub_key.to_string(),
        },
        vaults,
    };

    crate::genesis::create_genesis_block(validator, &genesis, runtime).await
}

/// Create the genesis block from a [`CasperConf`] (port of
/// `NodeLaunch.createGenesisBlockFromConfig`).
pub async fn create_genesis_block_from_config(
    validator: &ValidatorIdentity,
    conf: &CasperConf,
    runtime: &RuntimeManager,
) -> Result<BlockMessage, String> {
    let gbd = &conf.genesis_block_data;
    create_genesis_block(
        validator,
        &conf.shard_name,
        gbd.genesis_block_number,
        &gbd.bonds_file,
        conf.autogen_shard_size,
        &gbd.wallets_file,
        gbd.bond_minimum,
        gbd.bond_maximum,
        gbd.epoch_length,
        gbd.quarantine_length,
        gbd.number_of_active_validators,
        &gbd.pos_multi_sig_public_keys,
        gbd.pos_multi_sig_quorum,
        &gbd.pos_vault_pub_key,
        &gbd.system_contract_pub_key,
        runtime,
    )
    .await
}

/// Wait until at least one peer connection is established (port of `waitForFirstConnection`).
async fn wait_for_first_connection(connections: &ConnectionsCell, log: &dyn Log) {
    let source = LogSource::new("casper.engine.NodeLaunch");
    loop {
        if !connections.read().await.is_empty() {
            return;
        }
        log.debug(source, "Waiting for first connection...");
        tokio::time::sleep(Duration::from_millis(250)).await;
    }
}

/// Create, store and broadcast the genesis block (port of `createStoreBroadcastGenesis`).
async fn create_store_broadcast_genesis(
    validator_identity_opt: Option<&ValidatorIdentity>,
    conf: &CasperConf,
    runtime_manager: &RuntimeManager,
    block_store: &BlockStore,
    approved_store: &ApprovedStore,
    dag: &dyn BlockDagStorage,
    comm_util: &CommUtil,
    log: &dyn Log,
) -> Result<(), String> {
    let source = LogSource::new("casper.engine.NodeLaunch");
    let validator = validator_identity_opt.ok_or_else(|| {
        "To create genesis block node must provide validator private key".to_string()
    })?;

    let genesis_block = create_genesis_block_from_config(validator, conf, runtime_manager).await?;
    log.info(
        source,
        &format!(
            "Sending genesis block {} to peers...",
            genesis_block.block_hash.to_hex()
        ),
    );

    let genesis_fringe = FinalizedFringe {
        hashes: Vec::new(),
        state_hash: genesis_block.pre_state_hash,
    };

    put_block(block_store, genesis_block.clone()).await?;
    put_approved_block(approved_store, genesis_fringe.clone()).await?;
    insert_genesis(dag, genesis_block).await?;
    comm_util
        .stream_to_peers(&FinalizedFringeSerde.mk_packet(&genesis_fringe), None)
        .await;
    Ok(())
}

/// The node launch mode dispatch (port of `NodeLaunch.apply`).
#[allow(clippy::too_many_arguments)]
pub async fn apply<I: RSpaceImporter, E: RSpaceExporter>(
    mut packet_rx: mpsc::Receiver<PeerMessage>,
    incoming_blocks: mpsc::UnboundedSender<BlockMessage>,
    conf: CasperConf,
    trim_state: bool,
    // The store-items response is served unconditionally; `disable_state_exporter` would gate it,
    // but the config flag is not yet threaded through, so it is accepted and ignored for now.
    _disable_state_exporter: bool,
    validator_identity_opt: Option<ValidatorIdentity>,
    standalone: bool,
    transport: Arc<dyn TransportLayer>,
    comm_util: Arc<CommUtil>,
    block_retriever: Arc<BlockRetriever>,
    connections: ConnectionsCell,
    rp_conf: RPConf,
    runtime_manager: Arc<RuntimeManager>,
    block_store: BlockStore,
    approved_store: ApprovedStore,
    dag: Arc<dyn BlockDagStorage>,
    importer: I,
    exporter: E,
    log: Arc<dyn Log>,
) -> Result<(), String> {
    let source = LogSource::new("casper.engine.NodeLaunch");

    let repr = dag.get_representation().await;
    if repr.dag_set.is_empty() && standalone {
        log.info(source, "Starting as genesis master, creating genesis block...");
        create_store_broadcast_genesis(
            validator_identity_opt.as_ref(),
            &conf,
            runtime_manager.as_ref(),
            &block_store,
            &approved_store,
            dag.as_ref(),
            comm_util.as_ref(),
            log.as_ref(),
        )
        .await?;
    } else if repr.dag_set.is_empty() {
        log.info(source, "Starting from bootstrap node, syncing LFS...");
        let engine = Arc::new(tokio::sync::Mutex::new(NodeSyncing::new(
            transport.clone(),
            rp_conf.clone(),
            block_store.clone(),
            dag.clone(),
            approved_store.clone(),
            comm_util.clone(),
            log.clone(),
            validator_identity_opt.clone(),
            trim_state,
            importer,
        )));
        comm_util
            .request_finalized_fringe(trim_state)
            .await
            .map_err(|e| e.to_string())?;

        // Handle packets concurrently with the syncing-finished signal.
        let finished = { engine.lock().await.finished_handle() };
        let handle_loop = async {
            while let Some(pm) = packet_rx.recv().await {
                let mut guard = engine.lock().await;
                if let Err(err) = guard.handle(&pm.peer, &pm.message).await {
                    log.warn(
                        source,
                        &format!("Error handling message from {}: {err}", pm.peer),
                    );
                }
            }
        };
        tokio::select! {
            _ = handle_loop => {}
            _ = finished.notified() => {}
        }
    } else {
        log.info(source, "Reconnecting to existing network...");
    }

    // Transition to running mode.
    let engine = NodeRunning::new(
        transport,
        rp_conf,
        block_store,
        dag,
        block_retriever,
        log.clone(),
        validator_identity_opt,
        incoming_blocks,
        exporter,
    );
    log.info(source, "Making a transition to Running state.");
    wait_for_first_connection(&connections, log.as_ref()).await;
    comm_util.send_fork_choice_tip_request().await;
    while let Some(pm) = packet_rx.recv().await {
        engine.handle(&pm.peer, &pm.message).await;
    }
    Ok(())
}
