//! The transport-layer abstraction.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/TransportLayer.scala`.

use async_trait::async_trait;
use rchain_models::comm::protocol::Protocol;

use crate::errors::CommErr;
use crate::peer_node::PeerNode;
use crate::transport::chunker::Blob;

/// The transport interface (port of `TransportLayer[F]`): send a protocol message to a peer,
/// broadcast to many peers, or stream a blob to many peers.
#[async_trait]
pub trait TransportLayer: Send + Sync {
    async fn send(&self, peer: &PeerNode, msg: Protocol) -> CommErr<()>;

    async fn broadcast(&self, peers: &[PeerNode], msg: Protocol) -> Vec<CommErr<()>>;

    async fn stream(&self, peers: &[PeerNode], blob: Blob);
}
