//! Kademlia iterative node discovery.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/discovery/KademliaNodeDiscovery.scala`.

use std::collections::HashSet;

use rand::seq::SliceRandom;

use crate::discovery::{KademliaRpc, KademliaStore};
use crate::peer_node::{NodeIdentifier, PeerNode};

/// Return up to `limit` candidate peers (port of `KademliaNodeDiscovery.discover`).
pub async fn discover(id: &NodeIdentifier, store: &dyn KademliaStore, rpc: &dyn KademliaRpc) {
    let mut peers = store.peers();
    peers.shuffle(&mut rand::thread_rng());
    let dists = store.sparseness();
    let result = find(10, &dists, peers, HashSet::new(), id, store, rpc).await;
    for peer in result {
        store.update_last_seen(peer);
    }
}

/// Return the store's peers (port of `KademliaNodeDiscovery.peers`).
pub fn peers(store: &dyn KademliaStore) -> Vec<PeerNode> {
    store.peers()
}

async fn find(
    limit: usize,
    dists: &[usize],
    mut peer_set: Vec<PeerNode>,
    mut potentials: HashSet<PeerNode>,
    id: &NodeIdentifier,
    store: &dyn KademliaStore,
    rpc: &dyn KademliaRpc,
) -> Vec<PeerNode> {
    let mut i = 0;
    while !peer_set.is_empty() && potentials.len() < limit && i < dists.len() {
        let dist = dists[i];
        let mut target = id.key().to_vec();
        let byte_index = dist / 8;
        let different_bit = 1u8 << (dist % 8);
        target[byte_index] ^= different_bit;

        let head = peer_set.remove(0);
        let found = rpc.lookup(&target, &head).await;
        for p in filter(found, &potentials, id, store) {
            potentials.insert(p);
        }
        i += 1;
    }
    potentials.into_iter().collect()
}

fn filter(
    peers: Vec<PeerNode>,
    potentials: &HashSet<PeerNode>,
    id: &NodeIdentifier,
    store: &dyn KademliaStore,
) -> Vec<PeerNode> {
    peers
        .into_iter()
        .filter(|p| !potentials.contains(p) && p.id.key() != id.key())
        .filter(|p| store.find(p.id.key()).is_none())
        .collect()
}
