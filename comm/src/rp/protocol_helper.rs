//! Protocol message builders (pure helpers).
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/rp/ProtocolHelper.scala`.

use rchain_models::comm::protocol::{
    protocol, Disconnect, Header, Heartbeat, Node, Packet, Protocol, ProtocolHandshake,
    ProtocolHandshakeResponse,
};

use crate::errors::{CommErr, CommError};
use crate::peer_node::PeerNode;
use crate::transport::chunker::Blob;

pub fn to_protocol_bytes(x: &str) -> Vec<u8> {
    x.as_bytes().to_vec()
}

pub fn header(src: &PeerNode, network_id: &str) -> Header {
    Header {
        sender: Some(src.to_node()),
        network_id: network_id.to_string(),
    }
}

pub fn node(n: &PeerNode) -> Node {
    n.to_node()
}

pub fn sender(proto: &Protocol) -> CommErr<PeerNode> {
    let header = proto.header.as_ref().ok_or(CommError::HeaderNotAvailable)?;
    let sender = header.sender.as_ref().ok_or(CommError::SenderNotAvailable)?;
    PeerNode::from_node(sender)
}

pub fn to_peer_node(n: &Node) -> CommErr<PeerNode> {
    PeerNode::from_node(n)
}

pub fn protocol(src: &PeerNode, network_id: &str) -> Protocol {
    Protocol {
        header: Some(header(src, network_id)),
        message: None,
    }
}

pub fn protocol_handshake(src: &PeerNode, network_id: &str) -> Protocol {
    Protocol {
        message: Some(protocol::Message::ProtocolHandshake(ProtocolHandshake::default())),
        ..protocol(src, network_id)
    }
}

pub fn to_protocol_handshake(proto: &Protocol) -> CommErr<ProtocolHandshake> {
    match &proto.message {
        Some(protocol::Message::ProtocolHandshake(h)) => Ok(h.clone()),
        other => Err(CommError::UnknownProtocolError(format!(
            "Was expecting ProtocolHandshake, got {other:?}"
        ))),
    }
}

pub fn protocol_handshake_response(src: &PeerNode, network_id: &str) -> Protocol {
    Protocol {
        message: Some(protocol::Message::ProtocolHandshakeResponse(
            ProtocolHandshakeResponse::default(),
        )),
        ..protocol(src, network_id)
    }
}

pub fn heartbeat(src: &PeerNode, network_id: &str) -> Protocol {
    Protocol {
        message: Some(protocol::Message::Heartbeat(Heartbeat {})),
        ..protocol(src, network_id)
    }
}

pub fn to_heartbeat(proto: &Protocol) -> CommErr<Heartbeat> {
    match &proto.message {
        Some(protocol::Message::Heartbeat(h)) => Ok(*h),
        other => Err(CommError::UnknownProtocolError(format!(
            "Was expecting Heartbeat, got {other:?}"
        ))),
    }
}

pub fn packet(src: &PeerNode, network_id: &str, packet: Packet) -> Protocol {
    Protocol {
        message: Some(protocol::Message::Packet(packet)),
        ..protocol(src, network_id)
    }
}

pub fn to_packet(proto: &Protocol) -> CommErr<Packet> {
    match &proto.message {
        Some(protocol::Message::Packet(p)) => Ok(p.clone()),
        other => Err(CommError::UnknownProtocolError(format!(
            "Was expecting Packet, got {other:?}"
        ))),
    }
}

pub fn disconnect(src: &PeerNode, network_id: &str) -> Protocol {
    Protocol {
        message: Some(protocol::Message::Disconnect(Disconnect {})),
        ..protocol(src, network_id)
    }
}

pub fn to_disconnect(proto: &Protocol) -> CommErr<Disconnect> {
    match &proto.message {
        Some(protocol::Message::Disconnect(d)) => Ok(*d),
        other => Err(CommError::UnknownProtocolError(format!(
            "Was expecting Disconnect, got {other:?}"
        ))),
    }
}

/// Build a `Blob` from a sender, type id and raw content (port of `ProtocolHelper.blob`).
pub fn blob(sender: PeerNode, type_id: String, content: Vec<u8>) -> Blob {
    Blob {
        sender,
        packet: Packet {
            type_id,
            content,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::peer_node::NodeIdentifier;

    fn peer() -> PeerNode {
        PeerNode::from(NodeIdentifier::new(vec![1, 2, 3]), "host".into(), rchain_shared::refined::Port::new(40400), rchain_shared::refined::Port::new(40404))
    }

    #[test]
    fn handshake_round_trips() {
        let proto = protocol_handshake(&peer(), "testnet");
        assert_eq!(sender(&proto), Ok(peer()));
        assert_eq!(to_protocol_handshake(&proto).unwrap().nonce, Vec::<u8>::new());
    }

    #[test]
    fn packet_round_trips() {
        let p = Packet {
            type_id: "BlockMessage".to_string(),
            content: vec![1, 2, 3],
        };
        let proto = packet(&peer(), "testnet", p.clone());
        assert_eq!(to_packet(&proto).unwrap(), p);
    }
}

