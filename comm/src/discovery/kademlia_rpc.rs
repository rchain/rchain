//! Kademlia RPC abstraction.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/discovery/KademliaRPC.scala`.

use async_trait::async_trait;

use crate::peer_node::PeerNode;

/// The Kademlia discovery RPC (port of `KademliaRPC[F]`).
#[async_trait]
pub trait KademliaRpc: Send + Sync {
    /// Ping a peer; `true` if it answers on the same network.
    async fn ping(&self, node: &PeerNode) -> bool;

    /// Look up the peers closest to `key` as known by `peer`.
    async fn lookup(&self, key: &[u8], peer: &PeerNode) -> Vec<PeerNode>;
}
