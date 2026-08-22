//! Kademlia routing-table store.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/discovery/KademliaStore.scala`. The store is
//! synchronous (the `F[_]` effect and gauge metrics are dropped), wrapping the synchronous
//! `PeerTable`.

use std::sync::Arc;

use crate::discovery::peer_table::{PeerTable, REDUNDANCY};
use crate::peer_node::{NodeIdentifier, PeerNode};

/// The Kademlia store (port of `KademliaStore[F]`).
pub trait KademliaStore: Send + Sync {
    fn peers(&self) -> Vec<PeerNode>;
    fn sparseness(&self) -> Vec<usize>;
    fn update_last_seen(&self, peer_node: PeerNode);
    fn lookup(&self, key: &[u8]) -> Vec<PeerNode>;
    fn find(&self, key: &[u8]) -> Option<PeerNode>;
    fn remove(&self, key: &[u8]);
}

/// Build a `PeerTable`-backed store (port of `KademliaStoreInstances.table`).
pub fn table(id: &NodeIdentifier) -> Arc<dyn KademliaStore> {
    Arc::new(TableKademliaStore {
        table: PeerTable::new(id.key().to_vec(), REDUNDANCY),
    })
}

struct TableKademliaStore {
    table: PeerTable<PeerNode>,
}

impl KademliaStore for TableKademliaStore {
    fn peers(&self) -> Vec<PeerNode> {
        self.table.peers()
    }

    fn sparseness(&self) -> Vec<usize> {
        self.table.sparseness()
    }

    fn update_last_seen(&self, peer_node: PeerNode) {
        self.table.update_last_seen(peer_node);
    }

    fn lookup(&self, key: &[u8]) -> Vec<PeerNode> {
        self.table.lookup(key)
    }

    fn find(&self, key: &[u8]) -> Option<PeerNode> {
        self.table.find(key)
    }

    fn remove(&self, key: &[u8]) {
        self.table.remove(key);
    }
}
