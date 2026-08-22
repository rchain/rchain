//! Casper comm utilities (port of `protocol/CommUtil.scala`).
//!
//! `CommUtil` is a thin facade over the transport layer that broadcasts packets to (a random
//! subset of) the current connections, streams blobs, retries sends, and requests blocks. The
//! Scala `ConnectionsCell.random` becomes [`random_connections`](rchain_comm::rp::connect::random_connections).

use std::sync::Arc;
use std::time::Duration;

use rchain_comm::peer_node::PeerNode;
use rchain_comm::rp::connect::random_connections;
use rchain_comm::rp::protocol_helper;
use rchain_comm::rp::rp_conf::RPConf;
use rchain_comm::transport::chunker::Blob;
use rchain_comm::transport::transport_layer::TransportLayer;
use rchain_comm::transport::transport_layer_syntax;
use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::{
    BlockHashMessage, BlockRequest, FinalizedFringeRequest, ForkChoiceTipRequest, HasBlockRequest,
};
use rchain_models::casper::protocol::packet_type_tag::ToPacket;
use rchain_models::comm::protocol::{Packet, Protocol};
use rchain_shared::log::{Log, LogSource};

use crate::protocol::casper_message_protocol::{
    BlockHashMessageSerde, BlockRequestSerde, FinalizedFringeRequestSerde,
    ForkChoiceTipRequestSerde, HasBlockRequestSerde,
};

/// A shared, mutable list of current connections (port of `ConnectionsCell[F]`).
pub type ConnectionsCell = Arc<tokio::sync::RwLock<Vec<PeerNode>>>;

/// Maximum number of retries for a bootstrap request before giving up (M8). Documented deviation:
/// Scala's `keepOnRequestingTillRunning` retries forever.
const MAX_BOOTSTRAP_RETRIES: u32 = 10;

/// A standalone (bootstrap) node tried to send to the bootstrap node (port of
/// `StandaloneNodeSendToBootstrapError`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StandaloneNodeSendToBootstrapError;

impl std::fmt::Display for StandaloneNodeSendToBootstrapError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("standalone node cannot send to the bootstrap node")
    }
}

impl std::error::Error for StandaloneNodeSendToBootstrapError {}

/// Comm utilities (port of `CommUtil[F]`).
pub struct CommUtil {
    transport: Arc<dyn TransportLayer>,
    conf: RPConf,
    connections: ConnectionsCell,
    log: Arc<dyn Log>,
    log_source: LogSource,
}

impl CommUtil {
    pub fn new(
        transport: Arc<dyn TransportLayer>,
        conf: RPConf,
        connections: ConnectionsCell,
        log: Arc<dyn Log>,
    ) -> Self {
        CommUtil {
            transport,
            conf,
            connections,
            log,
            log_source: LogSource::new("casper.protocol.CommUtil"),
        }
    }

    /// Broadcast a packet (in one piece) to up to `scope_size` random peers (port of `sendToPeers`).
    pub async fn send_to_peers(&self, message: &Packet, scope_size: Option<usize>) {
        let max = scope_size.unwrap_or(self.conf.max_num_of_connections);
        let peers = {
            let connections = self.connections.read().await;
            random_connections(&connections, max)
        };
        let msg = protocol_helper::packet(&self.conf.local, &self.conf.network_id, message.clone());
        self.transport.broadcast(&peers, msg).await;
    }

    /// Broadcast a packet in chunks (stream) to up to `scope_size` random peers (port of
    /// `streamToPeers`).
    pub async fn stream_to_peers(&self, packet: &Packet, scope_size: Option<usize>) {
        let max = scope_size.unwrap_or(self.conf.max_num_of_connections);
        let peers = {
            let connections = self.connections.read().await;
            random_connections(&connections, max)
        };
        let blob = Blob {
            sender: self.conf.local.clone(),
            packet: packet.clone(),
        };
        self.transport.stream(&peers, blob).await;
    }

    /// Send a packet with retry until it succeeds (port of `sendWithRetry`).
    pub async fn send_with_retry(
        &self,
        message: &Packet,
        peer: &PeerNode,
        retry_after: Duration,
        msg_type_name: &str,
    ) {
        let msg = protocol_helper::packet(&self.conf.local, &self.conf.network_id, message.clone());
        self.log
            .info(self.log_source, &format!("Starting to request {msg_type_name}"));
        self.keep_on_requesting_till_running(peer, &msg, retry_after, msg_type_name)
            .await;
    }

    /// Retry sending `msg` to `peer` until it succeeds (port of `keepOnRequestingTillRunning`).
    ///
    /// Bounded to [`MAX_BOOTSTRAP_RETRIES`] attempts (M8): a dead/unreachable bootstrap must not
    /// block node startup forever (documented deviation — Scala retries indefinitely).
    async fn keep_on_requesting_till_running(
        &self,
        peer: &PeerNode,
        msg: &Protocol,
        retry_after: Duration,
        msg_type_name: &str,
    ) {
        let mut attempts = 0u32;
        loop {
            match self.transport.send(peer, msg.clone()).await {
                Ok(_) => {
                    self.log.info(
                        self.log_source,
                        &format!("Successfully sent {msg_type_name} to {peer}"),
                    );
                    break;
                }
                Err(error) => {
                    attempts += 1;
                    if attempts >= MAX_BOOTSTRAP_RETRIES {
                        self.log.error(
                            self.log_source,
                            &format!(
                                "Giving up sending {msg_type_name} to {peer} after {attempts} attempts (last error: {error:?})."
                            ),
                        );
                        break;
                    }
                    self.log.warn(
                        self.log_source,
                        &format!(
                            "Failed to send {msg_type_name} to {peer} because of {error:?}. Retrying in {retry_after:?}..."
                        ),
                    );
                    tokio::time::sleep(retry_after).await;
                }
            }
        }
    }

    /// Request a block from a peer (port of `requestForBlock`).
    pub async fn request_for_block(&self, peer: &PeerNode, hash: &BlockHash) {
        self.log.debug(
            self.log_source,
            &format!("Requesting {} from {}.", hash.to_hex(), peer.endpoint.host),
        );
        let packet = BlockRequestSerde.mk_packet(&BlockRequest {
            hash: hash.as_bytes().to_vec(),
        });
        transport_layer_syntax::send_to_peer(self.transport.as_ref(), &self.conf, peer, packet).await;
    }

    // --- CommUtil syntax extensions (port of `CommUtilOps`) --------------------------------

    /// Broadcast a block hash to peers (port of `sendBlockHash`).
    pub async fn send_block_hash(&self, hash: &BlockHash, block_creator: &[u8]) {
        let msg = BlockHashMessage {
            block_hash: *hash,
            block_creator: block_creator.to_vec(),
        };
        let packet = BlockHashMessageSerde.mk_packet(&msg);
        self.send_to_peers(&packet, None).await;
        self.log
            .info(self.log_source, &format!("Sent hash {} to peers", hash.to_hex()));
    }

    /// Broadcast a has-block request to peers (port of `broadcastHasBlockRequest`).
    pub async fn broadcast_has_block_request(&self, hash: &BlockHash) {
        let packet = HasBlockRequestSerde.mk_packet(&HasBlockRequest {
            hash: hash.as_bytes().to_vec(),
        });
        self.send_to_peers(&packet, None).await;
    }

    /// Broadcast a request for a block (port of `broadcastRequestForBlock`).
    pub async fn broadcast_request_for_block(&self, hash: &BlockHash, scope_size: Option<usize>) {
        let packet = BlockRequestSerde.mk_packet(&BlockRequest {
            hash: hash.as_bytes().to_vec(),
        });
        self.send_to_peers(&packet, scope_size).await;
    }

    /// Request the fork-choice tip from peers (port of `sendForkChoiceTipRequest`).
    pub async fn send_fork_choice_tip_request(&self) {
        let packet = ForkChoiceTipRequestSerde.mk_packet(&ForkChoiceTipRequest);
        self.send_to_peers(&packet, None).await;
        self.log
            .info(self.log_source, "Requested fork tip from peers");
    }

    /// Request the finalized fringe from the bootstrap node (port of `requestFinalizedFringe`).
    pub async fn request_finalized_fringe(
        &self,
        trim_state: bool,
    ) -> Result<(), StandaloneNodeSendToBootstrapError> {
        let bootstrap = self
            .conf
            .bootstrap
            .clone()
            .ok_or(StandaloneNodeSendToBootstrapError)?;
        let msg = FinalizedFringeRequest {
            identifier: String::new(),
            trim_state,
        };
        let packet = FinalizedFringeRequestSerde.mk_packet(&msg);
        self.send_with_retry(&packet, &bootstrap, Duration::from_secs(10), "FinalizedFringeRequest")
            .await;
        Ok(())
    }
}
