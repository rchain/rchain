//! Node syncing state machine (port of `engine/NodeSyncing.scala`).
//!
//! Drives the Last Finalized State sync (blocks + tuple space) from the bootstrap node: it
//! accepts the finalized fringe from the bootstrap, runs the `LfsBlockRequester` and
//! `LfsTupleSpaceRequester` streams, then populates the DAG from the received blocks.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;
use std::time::Duration;

use rchain_block_storage::approved_store::{ApprovedStore, FINALIZED_FRINGE_KEY};
use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_comm::peer_node::PeerNode;
use rchain_comm::rp::rp_conf::RPConf;
use rchain_comm::transport::transport_layer::TransportLayer;
use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{
    BlockMessage, CasperMessage, FinalizedFringe, StoreItemsMessage,
};
use rchain_rspace::state::RSpaceImporter;
use rchain_shared::log::{Log, LogSource};

use super::lfs_block_requester::request_blocks;
use super::lfs_tuple_space_requester::request_tuple_space;
use crate::multi_parent_casper::DEPLOY_LIFESPAN;
use crate::protocol::comm_util::CommUtil;
use crate::validator_identity::ValidatorIdentity;

/// The node-syncing engine (port of the `NodeSyncing` class).
pub struct NodeSyncing<I: RSpaceImporter> {
    transport: Arc<dyn TransportLayer>,
    conf: RPConf,
    block_store: BlockStore,
    dag: Arc<dyn BlockDagStorage>,
    approved_store: ApprovedStore,
    comm_util: Arc<CommUtil>,
    log: Arc<dyn Log>,
    log_source: LogSource,
    #[allow(dead_code)] // reserved (Scala stores it; unused in the syncing path)
    validator_id: Option<ValidatorIdentity>,
    #[allow(dead_code)] // reserved (Scala stores it; consumed by the caller)
    trim_state: bool,
    importer: I,
    incoming_blocks_tx: tokio::sync::mpsc::Sender<BlockMessage>,
    incoming_blocks_rx: tokio::sync::mpsc::Receiver<BlockMessage>,
    tuple_space_tx: tokio::sync::mpsc::Sender<StoreItemsMessage>,
    tuple_space_rx: tokio::sync::mpsc::Receiver<StoreItemsMessage>,
    start_requester: bool,
    finished: Arc<tokio::sync::Notify>,
}

impl<I: RSpaceImporter> NodeSyncing<I> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        transport: Arc<dyn TransportLayer>,
        conf: RPConf,
        block_store: BlockStore,
        dag: Arc<dyn BlockDagStorage>,
        approved_store: ApprovedStore,
        comm_util: Arc<CommUtil>,
        log: Arc<dyn Log>,
        validator_id: Option<ValidatorIdentity>,
        trim_state: bool,
        importer: I,
    ) -> Self {
        let (incoming_blocks_tx, incoming_blocks_rx) = tokio::sync::mpsc::channel(50);
        let (tuple_space_tx, tuple_space_rx) = tokio::sync::mpsc::channel(50);
        NodeSyncing {
            transport,
            conf,
            block_store,
            dag,
            approved_store,
            comm_util,
            log,
            log_source: LogSource::new("casper.engine.NodeSyncing"),
            validator_id,
            trim_state,
            importer,
            incoming_blocks_tx,
            incoming_blocks_rx,
            tuple_space_tx,
            tuple_space_rx,
            start_requester: true,
            finished: Arc::new(tokio::sync::Notify::new()),
        }
    }

    /// A future that completes when syncing finishes (port of `finished.get`).
    pub async fn wait(&self) {
        self.finished.notified().await;
    }

    /// A cloneable handle to the syncing-finished notification, for waiting concurrently with the
    /// `handle` loop without holding the engine's mutex.
    pub fn finished_handle(&self) -> Arc<tokio::sync::Notify> {
        self.finished.clone()
    }

    /// Handle an incoming casper message (port of `handle`).
    pub async fn handle(&mut self, peer: &PeerNode, msg: &CasperMessage) -> Result<(), String> {
        match msg {
            CasperMessage::FinalizedFringe(fringe) => {
                self.on_finalized_fringe_message(peer, fringe).await
            }
            CasperMessage::StoreItemsMessage(s) => {
                self.log.info(
                    self.log_source,
                    &format!(
                        "Received StoreItems(history: {}, data: {}) from {peer}.",
                        s.history_items.len(),
                        s.data_items.len()
                    ),
                );
                let _ = self.tuple_space_tx.send(s.clone()).await;
                Ok(())
            }
            CasperMessage::BlockMessage(b) => {
                self.log.info(
                    self.log_source,
                    &format!("BlockMessage received #{} from {peer}.", b.block_number),
                );
                let _ = self.incoming_blocks_tx.send(b.clone()).await;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Handle a finalized-fringe message, starting the LFS sync once from the bootstrap node (port
    /// of `onFinalizedFringeMessage`).
    async fn on_finalized_fringe_message(
        &mut self,
        sender: &PeerNode,
        fringe: &FinalizedFringe,
    ) -> Result<(), String> {
        let sender_is_bootstrap = self
            .conf
            .bootstrap
            .as_ref()
            .map(|b| b == sender)
            .unwrap_or(false);
        if !sender_is_bootstrap {
            self.log
                .info(self.log_source, "Fringe message ignored, not received from bootstrap node.");
        }

        let start = if self.start_requester {
            if sender_is_bootstrap {
                self.start_requester = false;
                true
            } else {
                false
            }
        } else {
            false
        };

        if start {
            self.log.info(
                self.log_source,
                &format!(
                    "Received finalized fringe from bootstrap node ({}).",
                    fringe
                        .hashes
                        .iter()
                        .map(|h| h.to_hex())
                        .collect::<Vec<_>>()
                        .join(" ")
                ),
            );
            self.request_approved_state(fringe).await?;
            self.approved_store
                .put(&[(FINALIZED_FRINGE_KEY, fringe.clone())])
                .await?;
            self.log
                .info(self.log_source, "LFS state is successfully restored.");
        }
        Ok(())
    }

    /// Request the approved (last finalized) state: download the blocks and the tuple space in
    /// parallel, then populate the DAG (port of `requestApprovedState`).
    async fn request_approved_state(&mut self, fringe: &FinalizedFringe) -> Result<(), String> {
        let block_heights_before_fringe =
            i32::try_from(DEPLOY_LIFESPAN).map_err(|e| e.to_string())?;
        let block_fut = request_blocks(
            fringe,
            &mut self.incoming_blocks_rx,
            block_heights_before_fringe,
            Duration::from_secs(30),
            &self.block_store,
            self.comm_util.as_ref(),
            self.log.as_ref(),
        );
        let tuple_fut = request_tuple_space(
            fringe,
            &mut self.tuple_space_rx,
            Duration::from_secs(120),
            self.transport.as_ref(),
            &self.conf,
            &mut self.importer,
            self.log.as_ref(),
        );

        let (block_st, tuple_res) = tokio::join!(block_fut, tuple_fut);
        tuple_res.map_err(|e| e.to_string())?;

        self.log
            .info(self.log_source, "Rholang state received and saved to store.");
        self.populate_dag(block_st.lower_bound, &block_st.height_map)
            .await?;
        self.finished.notify_waiters();
        Ok(())
    }

    /// Insert the received blocks into the DAG (port of `populateDag`).
    async fn populate_dag(
        &self,
        min_height: i64,
        height_map: &BTreeMap<i64, BTreeSet<BlockHash>>,
    ) -> Result<(), String> {
        self.log
            .info(self.log_source, "Adding blocks for approved state to DAG.");

        let mut hashes: Vec<BlockHash> = height_map
            .values()
            .flat_map(|s| s.iter().copied())
            .collect();
        hashes.reverse();

        for hash in hashes {
            let block = self
                .block_store
                .get(&[hash])
                .await?
                .into_iter()
                .flatten()
                .next()
                .ok_or_else(|| format!("missing block {}", hash.to_hex()))?;
            let block_height = i64::from(block.block_number);
            if block_height >= min_height {
                self.log.info(
                    self.log_source,
                    &format!("Adding #{} {}.", block.block_number, hash.to_hex()),
                );
                let bmd = BlockMetadata::from_block(&block);
                self.dag.insert(bmd, block).await?;
            }
        }

        self.log
            .info(self.log_source, "Blocks for approved state added to DAG.");
        Ok(())
    }
}
