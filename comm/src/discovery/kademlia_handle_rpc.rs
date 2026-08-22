//! Pure Kademlia RPC handlers.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/discovery/KademliaHandleRPC.scala`.

use crate::discovery::kademlia_store::KademliaStore;
use crate::peer_node::PeerNode;

/// Handle an inbound ping (port of `handlePing`).
pub fn handle_ping(store: &dyn KademliaStore, peer: PeerNode) {
    store.update_last_seen(peer);
}

/// Handle an inbound lookup (port of `handleLookup`).
pub fn handle_lookup(store: &dyn KademliaStore, peer: PeerNode, id: &[u8]) -> Vec<PeerNode> {
    store.update_last_seen(peer);
    store.lookup(id)
}
