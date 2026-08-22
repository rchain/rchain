//! Transport-layer convenience methods.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/TransportLayerSyntax.scala`. The cats-mtl
//! `RPConfAsk` reader and the casper `ToPacket` typeclass are simplified to explicit `&RPConf` +
//! `Packet` parameters.

use rchain_models::comm::protocol::Packet;

use crate::peer_node::PeerNode;
use crate::rp::protocol_helper;
use crate::rp::rp_conf::RPConf;
use crate::transport::chunker::Blob;
use crate::transport::transport_layer::TransportLayer;

/// Stream a blob to a single peer (port of `stream1`).
pub async fn stream1<T: TransportLayer + ?Sized>(transport: &T, peer: &PeerNode, blob: Blob) {
    transport.stream(&[peer.clone()], blob).await;
}

/// Send a packet to a peer, wrapped in a protocol message (port of `sendToPeer`).
pub async fn send_to_peer<T: TransportLayer + ?Sized>(
    transport: &T,
    conf: &RPConf,
    peer: &PeerNode,
    packet: Packet,
) {
    let msg = protocol_helper::packet(&conf.local, &conf.network_id, packet);
    let _ = transport.send(peer, msg).await;
}

/// Stream a packet to a peer in chunks (port of `streamToPeer`).
pub async fn stream_to_peer<T: TransportLayer + ?Sized>(
    transport: &T,
    conf: &RPConf,
    peer: &PeerNode,
    packet: Packet,
) {
    let blob = Blob {
        sender: conf.local.clone(),
        packet,
    };
    stream1(transport, peer, blob).await;
}

/// Send a packet to the configured bootstrap peer (port of `sendToBootstrap`).
pub async fn send_to_bootstrap<T: TransportLayer + ?Sized>(
    transport: &T,
    conf: &RPConf,
    packet: Packet,
) {
    if let Some(bootstrap) = &conf.bootstrap {
        send_to_peer(transport, conf, bootstrap, packet).await;
    }
}
