//! Block retriever (port of `blocks/BlockRetriever.scala`).
//!
//! Makes sure a block is received once casper requests it. A block stays in scope until it is added
//! to the casper buffer (acknowledged via [`BlockRetriever::ack_received`]).

use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::Duration;

use rchain_comm::peer_node::PeerNode;
use rchain_models::block_hash::BlockHash;
use rchain_shared::log::{Log, LogSource};
use rchain_shared::time::current_millis;

use crate::protocol::comm_util::CommUtil;

/// Maximum number of distinct hashes the retriever tracks at once (H5): bounds the `requested` map
/// against a peer flooding `BlockHash`/`HasBlock` messages for bogus hashes.
const MAX_REQUESTED_BLOCKS: usize = 10_000;
/// Maximum waiting-list length per requested hash (H5).
const MAX_WAITING_LIST_PER_HASH: usize = 32;

/// Reason a hash was admitted (port of `AdmitHashReason`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AdmitHashReason {
    HasBlockMessageReceived,
    HashBroadcastRecieved,
    MissingDependencyRequested,
    BlockReceived,
}

/// Status of an admitted hash (port of `AdmitHashStatus`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AdmitHashStatus {
    NewSourcePeerAddedToRequest,
    NewRequestAdded,
    /// The `requested` map is at capacity; the new hash was not admitted.
    CapacityReached,
    Ignore,
}

/// Result of admitting a hash (port of `AdmitHashResult`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AdmitHashResult {
    pub status: AdmitHashStatus,
    pub broadcast_request: bool,
    pub request_block: bool,
}

/// Per-hash request state (port of `RequestState`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RequestState {
    /// Last time the block was requested.
    pub timestamp: i64,
    /// Peers that were queried for this block.
    pub peers: Vec<PeerNode>,
    /// Peers that reportedly have the block and are yet to be queried.
    pub waiting_list: Vec<PeerNode>,
}

impl RequestState {
    fn new(now: i64, source_peer: Option<PeerNode>) -> Self {
        RequestState {
            timestamp: now,
            peers: Vec::new(),
            waiting_list: source_peer.into_iter().collect(),
        }
    }
}

/// Add a new request if the hash is unknown (port of `addNewRequest`).
fn add_new_request(
    state: &BTreeMap<BlockHash, RequestState>,
    hash: &BlockHash,
    now: i64,
    source_peer: Option<PeerNode>,
) -> BTreeMap<BlockHash, RequestState> {
    if state.contains_key(hash) {
        state.clone()
    } else {
        let mut new_state = state.clone();
        new_state.insert(*hash, RequestState::new(now, source_peer));
        new_state
    }
}

/// Append a peer to an existing request's waiting list (port of `addSourcePeerToRequest`).
fn add_source_peer_to_request(
    state: &BTreeMap<BlockHash, RequestState>,
    hash: &BlockHash,
    peer: &PeerNode,
) -> BTreeMap<BlockHash, RequestState> {
    match state.get(hash) {
        None => state.clone(),
        Some(request_state) => {
            if request_state.waiting_list.len() >= MAX_WAITING_LIST_PER_HASH {
                return state.clone();
            }
            let mut updated = request_state.clone();
            updated.waiting_list.push(peer.clone());
            let mut new_state = state.clone();
            new_state.insert(*hash, updated);
            new_state
        }
    }
}

/// The pure state transition for `admit_hash` (port of the `RequestedBlocks.modify` block).
pub fn admit_hash_state(
    state: &BTreeMap<BlockHash, RequestState>,
    hash: &BlockHash,
    now: i64,
    peer: Option<&PeerNode>,
) -> (BTreeMap<BlockHash, RequestState>, AdmitHashResult) {
    let unknown_hash = !state.contains_key(hash);
    if unknown_hash {
        if state.len() >= MAX_REQUESTED_BLOCKS {
            return (
                state.clone(),
                AdmitHashResult {
                    status: AdmitHashStatus::CapacityReached,
                    broadcast_request: false,
                    request_block: false,
                },
            );
        }
        let new_state = add_new_request(state, hash, now, peer.cloned());
        let result = AdmitHashResult {
            status: AdmitHashStatus::NewRequestAdded,
            broadcast_request: peer.is_none(),
            request_block: peer.is_some(),
        };
        (new_state, result)
    } else {
        match peer {
            Some(peer) => {
                let already_known = state[hash].waiting_list.contains(peer);
                if already_known {
                    (
                        state.clone(),
                        AdmitHashResult {
                            status: AdmitHashStatus::Ignore,
                            broadcast_request: false,
                            request_block: false,
                        },
                    )
                } else {
                    let was_empty = state[hash].waiting_list.is_empty();
                    let new_state = add_source_peer_to_request(state, hash, peer);
                    let result = AdmitHashResult {
                        status: AdmitHashStatus::NewSourcePeerAddedToRequest,
                        broadcast_request: false,
                        // Request from the first peer in the waiting list; otherwise requests are
                        // triggered by the casper loop.
                        request_block: was_empty,
                    };
                    (new_state, result)
                }
            }
            None => (
                state.clone(),
                AdmitHashResult {
                    status: AdmitHashStatus::Ignore,
                    broadcast_request: false,
                    request_block: false,
                },
            ),
        }
    }
}

/// Block retriever (port of `BlockRetriever[F]`).
pub struct BlockRetriever {
    requested: tokio::sync::Mutex<BTreeMap<BlockHash, RequestState>>,
    comm_util: Arc<CommUtil>,
    log: Arc<dyn Log>,
    log_source: LogSource,
}

impl BlockRetriever {
    pub fn new(comm_util: Arc<CommUtil>, log: Arc<dyn Log>) -> Self {
        BlockRetriever {
            requested: tokio::sync::Mutex::new(BTreeMap::new()),
            comm_util,
            log,
            log_source: LogSource::new("casper.blocks.BlockRetriever"),
        }
    }

    /// Make the retriever process an incoming hash (port of `admitHash`).
    pub async fn admit_hash(
        &self,
        hash: &BlockHash,
        peer: Option<&PeerNode>,
        reason: AdmitHashReason,
    ) -> AdmitHashResult {
        let now = current_millis();
        let result = {
            let mut state = self.requested.lock().await;
            let (new_state, result) = admit_hash_state(&state, hash, now, peer);
            *state = new_state;
            result
        };

        match result.status {
            AdmitHashStatus::NewSourcePeerAddedToRequest => {
                self.log.debug(
                    self.log_source,
                    &format!(
                        "Adding {} to waiting list of {} request. Reason: {:?}",
                        peer.map(|p| p.endpoint.host.as_str()).unwrap_or(""),
                        hash.to_hex(),
                        reason
                    ),
                );
            }
            AdmitHashStatus::NewRequestAdded => {
                self.log.info(
                    self.log_source,
                    &format!(
                        "Adding {} hash to RequestedBlocks because of {:?}.",
                        hash.to_hex(),
                        reason
                    ),
                );
            }
            AdmitHashStatus::CapacityReached => {
                self.log.warn(
                    self.log_source,
                    &format!(
                        "Dropping {} hash: block-retriever request map is at capacity ({}).",
                        hash.to_hex(),
                        MAX_REQUESTED_BLOCKS
                    ),
                );
            }
            AdmitHashStatus::Ignore => {}
        }

        if result.broadcast_request {
            self.comm_util.broadcast_has_block_request(hash).await;
        }
        if result.request_block {
            if let Some(peer) = peer {
                self.comm_util.request_for_block(peer, hash).await;
            }
        }
        result
    }

    /// Acknowledge that a block was received (port of `ackReceived`).
    pub async fn ack_received(&self, hash: &BlockHash) {
        let mut state = self.requested.lock().await;
        let old = state.clone();
        state.remove(hash);
        let is_received = *state != old;
        if is_received {
            self.log
                .info(self.log_source, &format!("Block {} marked as received.", hash.to_hex()));
        }
    }

    /// Try to re-request all pending hashes whose latest request is older than `age_threshold`
    /// (port of `requestAll`).
    pub async fn request_all(&self, age_threshold: Duration) {
        let state = { self.requested.lock().await.clone() };
        let keys: Vec<BlockHash> = state.keys().copied().collect();
        if !keys.is_empty() {
            self.log.debug(
                self.log_source,
                &format!("Running BlockRetriever maintenance ({} items unexpired).", keys.len()),
            );
        }
        for hash in keys {
            let requested = state[&hash].clone();
            let expired = current_millis() - requested.timestamp > age_threshold.as_millis() as i64;
            if expired {
                self.log.debug(
                    self.log_source,
                    &format!("Casper loop: checking if should re-request {}.", hash.to_hex()),
                );
                self.try_rerequest(hash, requested).await;
            }
        }
    }

    /// Re-request a single expired hash from the next waiting peer, or broadcast for new sources
    /// (port of `tryRerequest`).
    async fn try_rerequest(&self, hash: BlockHash, requested: RequestState) {
        match requested.waiting_list.split_first() {
            Some((next_peer, waiting_list_tail)) => {
                self.log.debug(
                    self.log_source,
                    &format!(
                        "Trying {} to query for {} block. Remain waiting: {}.",
                        next_peer.endpoint.host,
                        hash.to_hex(),
                        waiting_list_tail
                            .iter()
                            .map(|p| p.endpoint.host.as_str())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                );
                self.comm_util.request_for_block(next_peer, &hash).await;
                let mut peers = requested.peers.clone();
                if !peers.contains(next_peer) {
                    peers.push(next_peer.clone());
                }
                let updated = RequestState {
                    timestamp: current_millis(),
                    peers,
                    waiting_list: waiting_list_tail.to_vec(),
                };
                let mut state = self.requested.lock().await;
                state.insert(hash, updated);
            }
            None => {
                self.log.warn(
                    self.log_source,
                    &format!(
                        "Could not retrieve requested block {} from {}. Asking peers again.",
                        hash.to_hex(),
                        requested
                            .peers
                            .iter()
                            .map(|p| p.endpoint.host.as_str())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                );
                {
                    let mut state = self.requested.lock().await;
                    state.remove(&hash);
                }
                self.comm_util.broadcast_has_block_request(&hash).await;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use rchain_comm::errors::CommErr;
    use rchain_comm::peer_node::NodeIdentifier;
    use rchain_comm::rp::rp_conf::{ClearConnectionsConf, RPConf};
    use rchain_comm::transport::transport_layer::TransportLayer;
    use rchain_models::comm::protocol::Protocol;
    use rchain_shared::log::NopLog;

    use crate::protocol::comm_util::{CommUtil, ConnectionsCell};

    fn hash(byte: u8) -> BlockHash {
        BlockHash::new([byte; 32])
    }

    fn peer(name: &str, port: u16) -> PeerNode {
        PeerNode::from(
            NodeIdentifier::new(name.as_bytes().to_vec()),
            "host".to_string(),
            rchain_shared::refined::Port::new(port),
            rchain_shared::refined::Port::new(port),
        )
    }

    fn set(items: &[u8]) -> BTreeMap<BlockHash, RequestState> {
        items
            .iter()
            .map(|b| (hash(*b), RequestState::new(0, None)))
            .collect()
    }

    #[test]
    fn admit_hash_state_adds_unknown_hash_and_broadcasts_without_peer() {
        let state = set(&[]);
        let (new_state, result) = admit_hash_state(&state, &hash(1), 0, None);
        assert!(new_state.contains_key(&hash(1)));
        assert_eq!(result.status, AdmitHashStatus::NewRequestAdded);
        assert!(result.broadcast_request);
        assert!(!result.request_block);
    }

    #[test]
    fn admit_hash_state_adds_unknown_hash_and_requests_with_peer() {
        let p = peer("peer", 40400);
        let state = set(&[]);
        let (new_state, result) = admit_hash_state(&state, &hash(1), 0, Some(&p));
        assert!(new_state.contains_key(&hash(1)));
        assert_eq!(result.status, AdmitHashStatus::NewRequestAdded);
        assert!(!result.broadcast_request);
        assert!(result.request_block);
    }

    #[test]
    fn admit_hash_state_ignores_known_hash_without_peer() {
        let state = set(&[1]);
        let (_, result) = admit_hash_state(&state, &hash(1), 0, None);
        assert_eq!(result.status, AdmitHashStatus::Ignore);
        assert!(!result.broadcast_request);
        assert!(!result.request_block);
    }

    #[test]
    fn admit_hash_state_requests_from_first_peer_when_waiting_list_empty() {
        let p = peer("peer", 40400);
        let state = set(&[1]); // known hash, empty waiting list
        let (_, result) = admit_hash_state(&state, &hash(1), 0, Some(&p));
        assert_eq!(result.status, AdmitHashStatus::NewSourcePeerAddedToRequest);
        assert!(result.request_block);
    }

    #[test]
    fn admit_hash_state_ignores_peer_already_in_waiting_list() {
        let p = peer("peer", 40400);
        let mut state = set(&[1]);
        state.get_mut(&hash(1)).unwrap().waiting_list.push(p.clone());
        let (_, result) = admit_hash_state(&state, &hash(1), 0, Some(&p));
        assert_eq!(result.status, AdmitHashStatus::Ignore);
    }

    #[test]
    fn admit_hash_state_adds_peer_without_requesting_when_list_nonempty() {
        let p = peer("peer", 40400);
        let p2 = peer("secondPeer", 40400);
        let mut state = set(&[1]);
        state.get_mut(&hash(1)).unwrap().waiting_list.push(p);
        let (new_state, result) = admit_hash_state(&state, &hash(1), 0, Some(&p2));
        assert_eq!(result.status, AdmitHashStatus::NewSourcePeerAddedToRequest);
        assert!(!result.request_block);
        assert_eq!(new_state[&hash(1)].waiting_list.len(), 2);
    }

    // --- effectful integration (mock transport) -------------------------------------------

    #[derive(Default)]
    struct MockTransport {
        sends: std::sync::Mutex<Vec<(PeerNode, Protocol)>>,
        broadcasts: std::sync::Mutex<Vec<(Vec<PeerNode>, Protocol)>>,
    }

    #[async_trait]
    impl TransportLayer for MockTransport {
        async fn send(&self, peer: &PeerNode, msg: Protocol) -> CommErr<()> {
            self.sends.lock().unwrap().push((peer.clone(), msg));
            Ok(())
        }
        async fn broadcast(&self, peers: &[PeerNode], msg: Protocol) -> Vec<CommErr<()>> {
            self.broadcasts.lock().unwrap().push((peers.to_vec(), msg));
            peers.iter().map(|_| Ok(())).collect()
        }
        async fn stream(&self, _peers: &[PeerNode], _blob: rchain_comm::transport::chunker::Blob) {
        }
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

    fn retriever(transport: Arc<MockTransport>, local: &PeerNode) -> BlockRetriever {
        let connections: ConnectionsCell = Arc::new(tokio::sync::RwLock::new(Vec::new()));
        let comm_util = Arc::new(CommUtil::new(
            transport,
            conf(local),
            connections,
            Arc::new(NopLog),
        ));
        BlockRetriever::new(comm_util, Arc::new(NopLog))
    }

    #[tokio::test]
    async fn admit_hash_with_peer_sends_block_request() {
        let transport = Arc::new(MockTransport::default());
        let local = peer("src", 40400);
        let remote = peer("peer", 40400);
        let retriever = retriever(transport.clone(), &local);

        let result = retriever.admit_hash(&hash(1), Some(&remote), AdmitHashReason::HasBlockMessageReceived).await;
        assert!(result.request_block);
        assert!(!result.broadcast_request);

        let sends = transport.sends.lock().unwrap();
        assert_eq!(sends.len(), 1);
        let (recipient, msg) = &sends[0];
        assert_eq!(recipient, &remote);
        let packet = rchain_comm::rp::protocol_helper::to_packet(msg).unwrap();
        assert_eq!(packet.type_id, "BlockRequest");
    }

    #[tokio::test]
    async fn admit_hash_without_peer_broadcasts_has_block_request() {
        let transport = Arc::new(MockTransport::default());
        let local = peer("src", 40400);
        let retriever = retriever(transport.clone(), &local);

        let result = retriever.admit_hash(&hash(1), None, AdmitHashReason::HashBroadcastRecieved).await;
        assert!(result.broadcast_request);
        assert!(!result.request_block);

        let broadcasts = transport.broadcasts.lock().unwrap();
        assert_eq!(broadcasts.len(), 1);
        let (_, msg) = &broadcasts[0];
        let packet = rchain_comm::rp::protocol_helper::to_packet(msg).unwrap();
        assert_eq!(packet.type_id, "HasBlockRequest");
    }
}
