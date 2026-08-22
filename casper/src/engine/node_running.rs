//! NodeRunning engine (port of `engine/NodeRunning.scala`).
//!
//! The message handlers wire the transport layer to the block store / DAG / block retriever: block
//! hash broadcasts and has-block messages feed the retriever, block requests are served from the
//! store, fork-choice-tip / finalized-fringe requests are served from the DAG, and store-items
//! (LFS state-sync) requests are served from the RSpace exporter.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_comm::peer_node::PeerNode;
use rchain_comm::rp::rp_conf::RPConf;
use rchain_comm::transport::transport_layer::TransportLayer;
use rchain_comm::transport::transport_layer_syntax;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_models::block::state_hash::StateHash;
use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::{
    BlockMessage, BlockRequest, CasperMessage, FinalizedFringe, HasBlock, HasBlockRequest,
    StoreItemsMessage, StoreItemsMessageRequest,
};
use rchain_models::casper::protocol::packet_type_tag::ToPacket;
use rchain_rspace::state::RSpaceExporter;
use rchain_shared::log::{Log, LogSource};

use crate::blocks::block_receiver::not_validated;
use crate::blocks::block_retriever::{AdmitHashReason, BlockRetriever};
use crate::protocol::casper_message_protocol::{
    BlockMessageSerde, FinalizedFringeSerde, HasBlockSerde, StoreItemsMessageSerde,
};
use crate::validator_identity::ValidatorIdentity;

/// Handle a peer-broadcast block hash (port of `handleBlockHashMessage`).
pub async fn handle_block_hash_message(
    block_retriever: &BlockRetriever,
    log: &dyn Log,
    source: LogSource,
    peer: &PeerNode,
    hash: &BlockHash,
    ignore: bool,
) {
    if ignore {
        log.debug(source, &format!("Ignoring {} hash broadcast", hash.to_hex()));
    } else {
        log.debug(
            source,
            &format!("Incoming BlockHashMessage {} from {}", hash.to_hex(), peer.endpoint.host),
        );
        let _ = block_retriever
            .admit_hash(hash, Some(peer), AdmitHashReason::HashBroadcastRecieved)
            .await;
    }
}

/// Handle a peer reporting that it has a particular block (port of `handleHasBlockMessage`).
pub async fn handle_has_block_message(
    block_retriever: &BlockRetriever,
    log: &dyn Log,
    source: LogSource,
    peer: &PeerNode,
    hash: &BlockHash,
    ignore: bool,
) {
    if ignore {
        log.debug(source, &format!("Ignoring {} HasBlockMessage", hash.to_hex()));
    } else {
        log.debug(
            source,
            &format!("Incoming HasBlockMessage {} from {}", hash.to_hex(), peer.endpoint.host),
        );
        let _ = block_retriever
            .admit_hash(hash, Some(peer), AdmitHashReason::HasBlockMessageReceived)
            .await;
    }
}

/// Per-peer block-request limit (requests/second). Generous enough for sync bursts while bounding
/// the outbound bandwidth a single peer can pull.
const DEFAULT_BLOCK_REQUEST_LIMIT_PER_SEC: u32 = 100;

/// Per-peer fixed-window rate limiter for block requests (H4): bounds the outbound bandwidth a
/// single peer can pull by requesting blocks. Documented Scala deviation: Scala serves every block
/// request with no limit.
pub struct PeerRateLimiter {
    max_per_sec: u32,
    state: Mutex<BTreeMap<Vec<u8>, (Instant, u32)>>,
}

impl PeerRateLimiter {
    pub fn new(max_per_sec: u32) -> Self {
        PeerRateLimiter {
            max_per_sec,
            state: Mutex::new(BTreeMap::new()),
        }
    }

    /// Admit a request from `peer` if it is within the per-peer one-second window.
    pub fn allow(&self, peer_key: &[u8]) -> bool {
        let now = Instant::now();
        let mut state = self.state.lock().unwrap_or_else(|p| p.into_inner());
        let entry = state.entry(peer_key.to_vec()).or_insert((now, 0));
        if now.duration_since(entry.0) >= Duration::from_secs(1) {
            entry.0 = now;
            entry.1 = 0;
        }
        entry.1 += 1;
        entry.1 <= self.max_per_sec
    }
}

/// Serve a peer's request for a block (port of `handleBlockRequest`), throttled per-peer.
pub async fn handle_block_request(
    transport: &dyn TransportLayer,
    conf: &RPConf,
    block_store: &BlockStore,
    log: &dyn Log,
    source: LogSource,
    peer: &PeerNode,
    br: &BlockRequest,
    limiter: &PeerRateLimiter,
) {
    let hash = BlockHash::from_slice(&br.hash);
    if !limiter.allow(peer.key()) {
        log.info(
            source,
            &format!(
                "Received request for block {} from {peer}. Dropped: per-peer block-request rate limit exceeded.",
                hash.to_hex()
            ),
        );
        return;
    }
    let has_block = block_store
        .contains(&[hash])
        .await
        .unwrap_or_default()
        .first()
        .copied()
        .unwrap_or(false);
    if has_block {
        if let Some(block) = block_store.get(&[hash]).await.unwrap_or_default().into_iter().flatten().next() {
            transport_layer_syntax::stream_to_peer(
                transport,
                conf,
                peer,
                BlockMessageSerde.mk_packet(&block),
            )
            .await;
        }
        log.info(
            source,
            &format!("Received request for block {} from {peer}. Response sent.", hash.to_hex()),
        );
    } else {
        log.info(
            source,
            &format!(
                "Received request for block {} from {peer}. No response given since block not found.",
                hash.to_hex()
            ),
        );
    }
}

/// Respond to a peer's has-block query (port of `handleHasBlockRequest`).
pub async fn handle_has_block_request(
    transport: &dyn TransportLayer,
    conf: &RPConf,
    peer: &PeerNode,
    hbr: &HasBlockRequest,
    has_block: bool,
) {
    if has_block {
        transport_layer_syntax::send_to_peer(
            transport,
            conf,
            peer,
            HasBlockSerde.mk_packet(&HasBlock {
                hash: hbr.hash.clone(),
            }),
        )
        .await;
    }
}

/// Respond to a peer's fork-choice-tip request (port of `handleForkChoiceTipRequest`).
pub async fn handle_fork_choice_tip_request(
    transport: &dyn TransportLayer,
    conf: &RPConf,
    dag: &dyn BlockDagStorage,
    log: &dyn Log,
    source: LogSource,
    peer: &PeerNode,
) {
    log.info(source, &format!("Received ForkChoiceTipRequest from {}", peer.endpoint.host));
    let repr = dag.get_representation().await;
    let tips: Vec<BlockHash> = repr
        .dag_message_state
        .latest_msgs
        .values()
        .map(|m| m.id)
        .collect();
    for tip in &tips {
        transport_layer_syntax::send_to_peer(
            transport,
            conf,
            peer,
            HasBlockSerde.mk_packet(&HasBlock {
                hash: tip.as_bytes().to_vec(),
            }),
        )
        .await;
    }
    log.info(
        source,
        &format!(
            "Sending tips {} to {}",
            tips.iter().map(|t| t.to_hex()).collect::<Vec<_>>().join(" "),
            peer.endpoint.host
        ),
    );
}

/// Stream a finalized fringe to a peer (port of `handleFinalizedFringeRequest`).
pub async fn handle_finalized_fringe_request(
    transport: &dyn TransportLayer,
    conf: &RPConf,
    log: &dyn Log,
    source: LogSource,
    peer: &PeerNode,
    fringe: &FinalizedFringe,
) {
    log.info(source, &format!("Received FinalizedFringeRequest from {peer}"));
    transport_layer_syntax::stream_to_peer(
        transport,
        conf,
        peer,
        FinalizedFringeSerde.mk_packet(fringe),
    )
    .await;
    log.info(source, &format!("FinalizedFringe sent to {peer}"));
}

/// The running-state engine (port of the `NodeRunning` class): message handling and the
/// store-items (LFS state-sync) serving are ported.
pub struct NodeRunning<E: RSpaceExporter> {
    transport: Arc<dyn TransportLayer>,
    conf: RPConf,
    block_store: BlockStore,
    dag: Arc<dyn BlockDagStorage>,
    block_retriever: Arc<BlockRetriever>,
    log: Arc<dyn Log>,
    log_source: LogSource,
    validator_id: Option<ValidatorIdentity>,
    incoming_blocks: tokio::sync::mpsc::UnboundedSender<BlockMessage>,
    block_request_limit: Arc<PeerRateLimiter>,
    exporter: E,
}

impl<E: RSpaceExporter> NodeRunning<E> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        transport: Arc<dyn TransportLayer>,
        conf: RPConf,
        block_store: BlockStore,
        dag: Arc<dyn BlockDagStorage>,
        block_retriever: Arc<BlockRetriever>,
        log: Arc<dyn Log>,
        validator_id: Option<ValidatorIdentity>,
        incoming_blocks: tokio::sync::mpsc::UnboundedSender<BlockMessage>,
        exporter: E,
    ) -> Self {
        NodeRunning {
            transport,
            conf,
            block_store,
            dag,
            block_retriever,
            log,
            log_source: LogSource::new("casper.engine.NodeRunning"),
            validator_id,
            incoming_blocks,
            block_request_limit: Arc::new(PeerRateLimiter::new(DEFAULT_BLOCK_REQUEST_LIMIT_PER_SEC)),
            exporter,
        }
    }

    /// Serve a peer's store-items (state-sync) request from the exporter (port of
    /// `handleStoreItemsRequest`).
    async fn handle_store_items_request(&self, peer: &PeerNode, req: &StoreItemsMessageRequest) {
        // Validate-on-ingress: a negative skip/take would wrap to a huge `usize` via `as` and make
        // `get_nodes` iterate the whole trie.
        let (Ok(skip), Ok(take)) = (usize::try_from(req.skip), usize::try_from(req.take)) else {
            self.log
                .info(self.log_source, "Dropping store-items request with negative skip/take");
            return;
        };
        let nodes = self.exporter.get_nodes(&req.start_path, skip, take);
        let history_keys: Vec<Blake2b256Hash> =
            nodes.iter().filter(|n| !n.is_leaf).map(|n| n.hash).collect();
        let data_keys: Vec<Blake2b256Hash> =
            nodes.iter().filter(|n| n.is_leaf).map(|n| n.hash).collect();
        let history_items = self
            .exporter
            .get_history_items(&history_keys, |b: &[u8]| b.to_vec());
        let data_items = self
            .exporter
            .get_data_items(&data_keys, |b: &[u8]| b.to_vec());
        let last_path = nodes.last().map(|n| n.path.clone()).unwrap_or_default();

        let response = StoreItemsMessage {
            start_path: req.start_path.clone(),
            last_path,
            history_items,
            data_items,
        };
        self.log.info(
            self.log_source,
            &format!(
                "Sending {} history and {} data store items to {}",
                response.history_items.len(),
                response.data_items.len(),
                peer.endpoint.host
            ),
        );
        transport_layer_syntax::send_to_peer(
            self.transport.as_ref(),
            &self.conf,
            peer,
            StoreItemsMessageSerde.mk_packet(&response),
        )
        .await;
    }

    /// Handle an incoming casper message from a peer (port of `handle`).
    pub async fn handle(&self, peer: &PeerNode, msg: &CasperMessage) {
        match msg {
            CasperMessage::BlockHashMessage(bhm) => {
                let hash = bhm.block_hash;
                let ignore = self
                    .block_store
                    .contains(&[hash])
                    .await
                    .unwrap_or_default()
                    .first()
                    .copied()
                    .unwrap_or(false);
                handle_block_hash_message(
                    &self.block_retriever,
                    self.log.as_ref(),
                    self.log_source,
                    peer,
                    &hash,
                    ignore,
                )
                .await;
            }
            CasperMessage::BlockMessage(b) => {
                if let Some(id) = &self.validator_id {
                    if b.sender.as_bytes().as_slice() == id.public_key.bytes() {
                        self.log.warn(
                            self.log_source,
                            &format!(
                                "There is another node {peer} proposing using the same private key as you. \
                                 Or did you restart your node?"
                            ),
                        );
                    }
                }
                let known = self
                    .block_store
                    .contains(&[b.block_hash])
                    .await
                    .unwrap_or_default()
                    .first()
                    .copied()
                    .unwrap_or(false);
                if known {
                    self.log.debug(
                        self.log_source,
                        &format!(
                            "Ignoring BlockMessage #{} from {}",
                            b.block_number, peer.endpoint.host
                        ),
                    );
                } else {
                    let _ = self.incoming_blocks.send(b.clone());
                    self.log.debug(
                        self.log_source,
                        &format!("Incoming BlockMessage #{} from {}", b.block_number, peer.endpoint.host),
                    );
                }
            }
            CasperMessage::BlockRequest(br) => {
                handle_block_request(
                    self.transport.as_ref(),
                    &self.conf,
                    &self.block_store,
                    self.log.as_ref(),
                    self.log_source,
                    peer,
                    br,
                    self.block_request_limit.as_ref(),
                )
                .await;
            }
            CasperMessage::HasBlockRequest(hbr) => {
                let repr = self.dag.get_representation().await;
                let hash = BlockHash::from_slice(&hbr.hash);
                let has_block = repr.contains(&hash);
                handle_has_block_request(
                    self.transport.as_ref(),
                    &self.conf,
                    peer,
                    hbr,
                    has_block,
                )
                .await;
            }
            CasperMessage::HasBlock(hb) => {
                let hash = BlockHash::from_slice(&hb.hash);
                let known = self
                    .block_store
                    .contains(&[hash])
                    .await
                    .unwrap_or_default()
                    .first()
                    .copied()
                    .unwrap_or(false);
                if known {
                    if not_validated(&self.block_store, self.dag.as_ref(), &hash).await {
                        if let Some(block) =
                            self.block_store.get(&[hash]).await.unwrap_or_default().into_iter().flatten().next()
                        {
                            let _ = self.incoming_blocks.send(block);
                        }
                    }
                } else {
                    self.log.debug(
                        self.log_source,
                        &format!("Incoming HasBlockMessage {} from {}", hash.to_hex(), peer.endpoint.host),
                    );
                    let _ = self
                        .block_retriever
                        .admit_hash(&hash, Some(peer), AdmitHashReason::HasBlockMessageReceived)
                        .await;
                }
            }
            CasperMessage::ForkChoiceTipRequest(_) => {
                handle_fork_choice_tip_request(
                    self.transport.as_ref(),
                    &self.conf,
                    self.dag.as_ref(),
                    self.log.as_ref(),
                    self.log_source,
                    peer,
                )
                .await;
            }
            CasperMessage::FinalizedFringeRequest(_) => {
                let repr = self.dag.get_representation().await;
                let latest_fringe_hashes: BTreeSet<BlockHash> =
                    repr.latest_fringe().iter().map(|m| m.id).collect();
                if let Some(fringe_data) = repr.fringe_states.get(&latest_fringe_hashes) {
                    let fringe_response = FinalizedFringe {
                        hashes: latest_fringe_hashes.iter().copied().collect(),
                        state_hash: StateHash::from_slice(fringe_data.state_hash.as_bytes()),
                    };
                    handle_finalized_fringe_request(
                        self.transport.as_ref(),
                        &self.conf,
                        self.log.as_ref(),
                        self.log_source,
                        peer,
                        &fringe_response,
                    )
                    .await;
                    self.log.info(
                        self.log_source,
                        &format!(
                            "Sent fringe response ({}).",
                            fringe_response
                                .hashes
                                .iter()
                                .map(|h| h.to_hex())
                                .collect::<Vec<_>>()
                                .join(" ")
                        ),
                    );
                }
            }
            CasperMessage::StoreItemsMessageRequest(req) => {
                self.handle_store_items_request(peer, req).await;
            }
            CasperMessage::StoreItemsMessage(_) => {}
            CasperMessage::FinalizedFringe(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use rchain_block_storage::dag::codecs::{BlockHashCodec, BlockMessageCodec};
    use rchain_comm::errors::CommErr;
    use rchain_comm::peer_node::NodeIdentifier;
    use rchain_comm::rp::rp_conf::{ClearConnectionsConf, RPConf};
    use rchain_comm::transport::chunker::Blob;
    use rchain_comm::transport::transport_layer::TransportLayer;
    use rchain_models::comm::protocol::Protocol;
    use rchain_models::validator::Validator;
    use rchain_shared::log::NopLog;
    use rchain_shared::store::InMemoryKeyValueStore;
    use rchain_shared::typed_store::KeyValueTypedStoreCodec;
    use std::collections::{BTreeMap, BTreeSet};
    use std::time::Duration;

    use crate::protocol::comm_util::{CommUtil, ConnectionsCell};

    fn peer(name: &str, port: u16) -> PeerNode {
        PeerNode::from(
            NodeIdentifier::new(name.as_bytes().to_vec()),
            "host".to_string(),
            rchain_shared::refined::Port::new(port),
            rchain_shared::refined::Port::new(port),
        )
    }

    fn hash(byte: u8) -> BlockHash {
        BlockHash::new([byte; 32])
    }

    fn block(hash: BlockHash) -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: hash,
            block_number: 0.try_into().unwrap(),
            sender: Validator::new([1u8; 65]),
            seq_num: 0.try_into().unwrap(),
            pre_state_hash: StateHash::new([0u8; 32]),
            post_state_hash: StateHash::new([0u8; 32]),
            justifications: vec![],
            bonds: BTreeMap::new(),
            rejected_deploys: BTreeSet::new(),
            rejected_blocks: BTreeSet::new(),
            rejected_senders: BTreeSet::new(),
            state: rchain_models::casper::protocol::casper_message::RholangState::default(),
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![],
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

    fn conf(local: &PeerNode) -> RPConf {
        RPConf {
            local: local.clone(),
            network_id: "testnet".to_string(),
            bootstrap: None,
            default_timeout: Duration::from_secs(10),
            max_num_of_connections: 10,
            clear_connections: ClearConnectionsConf {
                num_of_connections_pinged: 10,
            },
        }
    }

    #[derive(Default)]
    struct MockTransport {
        sends: std::sync::Mutex<Vec<(PeerNode, Protocol)>>,
        streams: std::sync::Mutex<Vec<(Vec<PeerNode>, Blob)>>,
    }

    #[async_trait]
    impl TransportLayer for MockTransport {
        async fn send(&self, peer: &PeerNode, msg: Protocol) -> CommErr<()> {
            self.sends.lock().unwrap().push((peer.clone(), msg));
            Ok(())
        }
        async fn broadcast(&self, peers: &[PeerNode], msg: Protocol) -> Vec<CommErr<()>> {
            for peer in peers {
                self.sends.lock().unwrap().push((peer.clone(), msg.clone()));
            }
            peers.iter().map(|_| Ok(())).collect()
        }
        async fn stream(&self, peers: &[PeerNode], blob: Blob) {
            self.streams.lock().unwrap().push((peers.to_vec(), blob));
        }
    }

    #[tokio::test]
    async fn handle_block_request_streams_block_when_present() {
        let local = peer("src", 40400);
        let remote = peer("peer", 40400);
        let transport = Arc::new(MockTransport::default());
        let h = hash(1);
        let store = block_store(vec![block(h)]).await;
        let log = NopLog;

        handle_block_request(
            transport.as_ref(),
            &conf(&local),
            &store,
            &log,
            LogSource::new("test"),
            &remote,
            &BlockRequest { hash: h.as_bytes().to_vec() },
            &PeerRateLimiter::new(100),
        )
        .await;

        let streams = transport.streams.lock().unwrap();
        assert_eq!(streams.len(), 1);
        assert_eq!(streams[0].0, vec![remote.clone()]);
        assert_eq!(streams[0].1.packet.type_id, "BlockMessage");
    }

    #[tokio::test]
    async fn handle_block_request_does_not_stream_absent_block() {
        let local = peer("src", 40400);
        let remote = peer("peer", 40400);
        let transport = Arc::new(MockTransport::default());
        let store = block_store(vec![]).await;
        let log = NopLog;

        handle_block_request(
            transport.as_ref(),
            &conf(&local),
            &store,
            &log,
            LogSource::new("test"),
            &remote,
            &BlockRequest { hash: hash(1).as_bytes().to_vec() },
            &PeerRateLimiter::new(100),
        )
        .await;

        assert!(transport.streams.lock().unwrap().is_empty());
    }

    #[tokio::test]
    async fn handle_has_block_request_sends_has_block_when_present() {
        let local = peer("src", 40400);
        let remote = peer("peer", 40400);
        let transport = Arc::new(MockTransport::default());

        handle_has_block_request(
            transport.as_ref(),
            &conf(&local),
            &remote,
            &HasBlockRequest { hash: hash(1).as_bytes().to_vec() },
            true,
        )
        .await;

        let sends = transport.sends.lock().unwrap();
        assert_eq!(sends.len(), 1);
        assert_eq!(sends[0].0, remote);
        let packet = rchain_comm::rp::protocol_helper::to_packet(&sends[0].1).unwrap();
        assert_eq!(packet.type_id, "HasBlock");
    }

    #[tokio::test]
    async fn handle_has_block_request_sends_nothing_when_absent() {
        let local = peer("src", 40400);
        let remote = peer("peer", 40400);
        let transport = Arc::new(MockTransport::default());

        handle_has_block_request(
            transport.as_ref(),
            &conf(&local),
            &remote,
            &HasBlockRequest { hash: hash(1).as_bytes().to_vec() },
            false,
        )
        .await;

        assert!(transport.sends.lock().unwrap().is_empty());
    }

    #[tokio::test]
    async fn handle_has_block_message_requests_unknown_block_from_peer() {
        let local = peer("src", 40400);
        let remote = peer("peer", 40400);
        let transport = Arc::new(MockTransport::default());
        let connections: ConnectionsCell = Arc::new(tokio::sync::RwLock::new(Vec::new()));
        let comm_util = Arc::new(CommUtil::new(
            transport.clone(),
            conf(&local),
            connections,
            Arc::new(NopLog),
        ));
        let retriever = BlockRetriever::new(comm_util, Arc::new(NopLog));

        handle_has_block_message(
            &retriever,
            &NopLog,
            LogSource::new("test"),
            &remote,
            &hash(1),
            false,
        )
        .await;

        let sends = transport.sends.lock().unwrap();
        assert_eq!(sends.len(), 1);
        assert_eq!(sends[0].0, remote);
        let packet = rchain_comm::rp::protocol_helper::to_packet(&sends[0].1).unwrap();
        assert_eq!(packet.type_id, "BlockRequest");
    }
}
