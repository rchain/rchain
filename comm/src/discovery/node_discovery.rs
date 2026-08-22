//! Node discovery abstraction.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/discovery/NodeDiscovery.scala`.

use std::sync::Arc;

use async_trait::async_trait;

use crate::discovery::kademlia_node_discovery;
use crate::discovery::{KademliaRpc, KademliaStore};
use crate::peer_node::{NodeIdentifier, PeerNode};

/// The node discovery interface (port of `NodeDiscovery[F]`).
#[async_trait]
pub trait NodeDiscovery: Send + Sync {
    async fn discover(&self);
    fn peers(&self) -> Vec<PeerNode>;
}

/// The Kademlia-backed discovery (port of `NodeDiscoveryInstances.kademlia`).
pub struct KademliaNodeDiscovery {
    id: NodeIdentifier,
    store: Arc<dyn KademliaStore>,
    rpc: Arc<dyn KademliaRpc>,
}

impl KademliaNodeDiscovery {
    pub fn new(id: NodeIdentifier, store: Arc<dyn KademliaStore>, rpc: Arc<dyn KademliaRpc>) -> Self {
        KademliaNodeDiscovery { id, store, rpc }
    }
}

#[async_trait]
impl NodeDiscovery for KademliaNodeDiscovery {
    async fn discover(&self) {
        kademlia_node_discovery::discover(&self.id, self.store.as_ref(), self.rpc.as_ref()).await;
    }

    fn peers(&self) -> Vec<PeerNode> {
        kademlia_node_discovery::peers(self.store.as_ref())
    }
}
