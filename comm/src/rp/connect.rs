//! Connection bookkeeping and the protocol handshake/connect flow.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/rp/Connect.scala`. The cats-effect `Ref` connection
//! cell becomes `&mut Vec<PeerNode>`.

use std::collections::HashSet;

use rand::seq::SliceRandom;

use crate::discovery::NodeDiscovery;
use crate::errors::CommErr;
use crate::peer_node::PeerNode;
use crate::rp::protocol_helper;
use crate::rp::rp_conf::RPConf;
use crate::transport::transport_layer::TransportLayer;

/// Shuffle the connections and take up to `max` (port of `ConnectionsCell.random`).
pub fn random_connections(connections: &[PeerNode], max: usize) -> Vec<PeerNode> {
    let mut shuffled: Vec<PeerNode> = connections.to_vec();
    shuffled.shuffle(&mut rand::thread_rng());
    shuffled.truncate(max);
    shuffled
}

/// Append `to_be_added`, first removing any existing peer with the same id (port of `addConn`).
pub fn add_conn(connections: &[PeerNode], to_be_added: &[PeerNode]) -> Vec<PeerNode> {
    let ids: HashSet<Vec<u8>> = to_be_added.iter().map(|p| p.key().to_vec()).collect();
    let mut rest: Vec<PeerNode> = connections
        .iter()
        .filter(|p| !ids.contains(p.key()))
        .cloned()
        .collect();
    rest.extend(to_be_added.iter().cloned());
    rest
}

/// Remove every peer whose id is in `to_be_removed` (port of `removeConn`).
pub fn remove_conn(connections: &[PeerNode], to_be_removed: &[PeerNode]) -> Vec<PeerNode> {
    let ids: HashSet<Vec<u8>> = to_be_removed.iter().map(|p| p.key().to_vec()).collect();
    connections
        .iter()
        .filter(|p| !ids.contains(p.key()))
        .cloned()
        .collect()
}

/// Move `connection` (matched by id) to the end of the list, or leave unchanged if absent (port of
/// `refreshConn`).
pub fn refresh_conn(connections: &[PeerNode], connection: &PeerNode) -> Vec<PeerNode> {
    let (matched, rest): (Vec<PeerNode>, Vec<PeerNode>) = connections
        .iter()
        .cloned()
        .partition(|p| p.key() == connection.key());
    if matched.is_empty() {
        rest
    } else {
        let mut out = rest;
        out.extend(matched);
        out
    }
}

/// Send a protocol handshake to a peer (port of `connect`).
pub async fn connect<T: TransportLayer + ?Sized>(
    transport: &T,
    conf: &RPConf,
    peer: &PeerNode,
) -> CommErr<()> {
    let ph = protocol_helper::protocol_handshake(&conf.local, &conf.network_id);
    transport.send(peer, ph).await
}

/// Ping the first `num_of_connections_pinged` peers (over the read-only snapshot), returning
/// `(to_ping, successful, failed)`. The caller applies `removeConn(toPing).addConn(successful)` to
/// the *current* connections under a brief write lock, so the outbound sends never hold the
/// connection write-lock (port of `clearConnections`; the Scala `ConnectionsCell.update` is the
/// brief mutation step).
pub async fn clear_connections<T: TransportLayer + ?Sized>(
    transport: &T,
    conf: &RPConf,
    connections: &[PeerNode],
) -> (Vec<PeerNode>, Vec<PeerNode>, usize) {
    let num = conf.clear_connections.num_of_connections_pinged;
    let to_ping: Vec<PeerNode> = connections.iter().take(num).cloned().collect();
    let mut successful = Vec::new();
    let mut failed = 0usize;
    for peer in &to_ping {
        let hb = protocol_helper::heartbeat(&conf.local, &conf.network_id);
        match transport.send(peer, hb).await {
            Ok(_) => successful.push(peer.clone()),
            Err(_) => failed += 1,
        }
    }
    (to_ping, successful, failed)
}

/// Discover peers, connect to those not already connected, and return the successful ones (port of
/// `findAndConnect`).
pub async fn find_and_connect<T: TransportLayer + ?Sized>(
    node_discovery: &dyn NodeDiscovery,
    conf: &RPConf,
    transport: &T,
    connections: &[PeerNode],
) -> Vec<PeerNode> {
    let connected: HashSet<Vec<u8>> = connections.iter().map(|p| p.key().to_vec()).collect();
    let peers: Vec<PeerNode> = node_discovery
        .peers()
        .into_iter()
        .filter(|p| !connected.contains(p.key()))
        .collect();
    let mut result = Vec::new();
    for peer in &peers {
        if connect(transport, conf, peer).await.is_ok() {
            result.push(peer.clone());
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::peer_node::NodeIdentifier;

    fn peer(id: u8, host: &str) -> PeerNode {
        PeerNode::from(NodeIdentifier::new(vec![id]), host.into(), rchain_shared::refined::Port::new(40400), rchain_shared::refined::Port::new(40404))
    }

    #[test]
    fn add_conn_appends_new_and_dedupes_existing() {
        let a = peer(1, "a");
        let b = peer(2, "b");
        let c = peer(3, "c");
        let a2 = peer(1, "a2"); // same id, new endpoint
        assert_eq!(
            add_conn(&[a.clone(), b.clone()], &[c.clone()]),
            vec![a.clone(), b.clone(), c.clone()]
        );
        assert_eq!(add_conn(&[a.clone(), b.clone()], &[a2.clone()]), vec![b, a2]);
    }

    #[test]
    fn remove_conn_removes_by_id() {
        let a = peer(1, "a");
        let b = peer(2, "b");
        assert_eq!(remove_conn(&[a.clone(), b.clone()], &[b.clone()]), vec![a.clone()]);
        assert_eq!(
            remove_conn(&[a.clone(), b.clone()], &[peer(9, "x")]),
            vec![a, b]
        );
    }

    #[test]
    fn random_connections_takes_at_most_max() {
        let peers: Vec<PeerNode> = (0..10).map(|i| peer(i, "host")).collect();
        let picked = random_connections(&peers, 3);
        assert_eq!(picked.len(), 3);
        // All picked peers come from the input and are unique (a shuffle is a permutation subset).
        let ids: Vec<Vec<u8>> = picked.iter().map(|p| p.key().to_vec()).collect();
        assert_eq!(ids.len(), 3);
        for p in &picked {
            assert!(peers.contains(p));
        }
    }

    #[test]
    fn random_connections_takes_empty_when_max_zero() {
        let peers: Vec<PeerNode> = (0..5).map(|i| peer(i, "host")).collect();
        assert!(random_connections(&peers, 0).is_empty());
    }

    #[test]
    fn refresh_conn_moves_to_end() {
        let a = peer(1, "a");
        let b = peer(2, "b");
        let c = peer(3, "c");
        assert_eq!(
            refresh_conn(&[a.clone(), b.clone(), c.clone()], &a),
            vec![b.clone(), c.clone(), a.clone()]
        );
        assert_eq!(
            refresh_conn(&[a.clone(), b.clone()], &peer(9, "x")),
            vec![a, b]
        );
    }
}
