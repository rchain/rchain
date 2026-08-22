//! Peer identity.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/PeerNode.scala`.

use rchain_models::comm::protocol::Node;
use rchain_shared::base16;
use rchain_shared::refined::Port;

/// A node identifier (a raw public key, hex-encoded in its string form).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeIdentifier {
    key: Vec<u8>,
}

impl NodeIdentifier {
    pub fn new(key: Vec<u8>) -> Self {
        NodeIdentifier { key }
    }

    /// Parse a hex string into a node identifier (the Scala `NodeIdentifier.apply(name)`).
    ///
    /// Rejects odd-length and non-hex input (the previous form silently dropped non-hex bytes to
    /// `0`, corrupting the peer identity).
    pub fn from_hex(name: &str) -> Result<Self, String> {
        if name.len() % 2 != 0 {
            return Err(format!("odd-length hex key: {name}"));
        }
        let mut key = Vec::with_capacity(name.len() / 2);
        for pair in name.as_bytes().chunks(2) {
            let s = std::str::from_utf8(pair).map_err(|_| format!("invalid hex key: {name}"))?;
            let byte = u8::from_str_radix(s, 16).map_err(|_| format!("invalid hex key: {name}"))?;
            key.push(byte);
        }
        Ok(NodeIdentifier { key })
    }

    pub fn key(&self) -> &[u8] {
        &self.key
    }

    pub fn s_key(&self) -> String {
        base16::encode(&self.key)
    }
}

impl std::fmt::Display for NodeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.s_key())
    }
}

/// A network endpoint (host + tcp/udp ports).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Endpoint {
    pub host: String,
    pub tcp_port: Port,
    pub udp_port: Port,
}

/// A peer node (identifier + endpoint).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PeerNode {
    pub id: NodeIdentifier,
    pub endpoint: Endpoint,
}

impl PeerNode {
    pub fn key(&self) -> &[u8] {
        self.id.key()
    }

    pub fn s_key(&self) -> String {
        self.id.s_key()
    }

    /// The `rnode://` address string (port of `PeerNode.toAddress`).
    pub fn to_address(&self) -> String {
        format!(
            "rnode://{}@{}?protocol={}&discovery={}",
            self.s_key(),
            self.endpoint.host,
            u16::from(self.endpoint.tcp_port),
            u16::from(self.endpoint.udp_port)
        )
    }

    pub fn from(id: NodeIdentifier, host: String, protocol: Port, discovery: Port) -> PeerNode {
        PeerNode {
            id,
            endpoint: Endpoint {
                host,
                tcp_port: protocol,
                udp_port: discovery,
            },
        }
    }

    /// Build a peer from a routing `Node` (port of `PeerNode.from(node)`).
    pub fn from_node(node: &Node) -> Result<PeerNode, crate::errors::CommError> {
        Ok(PeerNode {
            id: NodeIdentifier::new(node.id.clone()),
            endpoint: Endpoint {
                host: String::from_utf8_lossy(&node.host).to_string(),
                tcp_port: Port::try_from(node.tcp_port).map_err(|e| {
                    crate::errors::CommError::ParseError(format!("invalid tcp port: {e}"))
                })?,
                udp_port: Port::try_from(node.udp_port).map_err(|e| {
                    crate::errors::CommError::ParseError(format!("invalid udp port: {e}"))
                })?,
            },
        })
    }

    /// Build a routing `Node` from this peer (port of `ProtocolHelper.node(peer)`).
    pub fn to_node(&self) -> Node {
        Node {
            id: self.key().to_vec(),
            host: self.endpoint.host.as_bytes().to_vec(),
            tcp_port: u32::from(self.endpoint.tcp_port),
            udp_port: u32::from(self.endpoint.udp_port),
        }
    }

    /// Parse an `rnode://` address (port of `PeerNode.fromAddress`).
    pub fn from_address(s: &str) -> Result<PeerNode, crate::errors::CommError> {
        let err = || crate::errors::CommError::ParseError(format!("bad address: {s}"));
        let rest = s.strip_prefix("rnode://").ok_or_else(err)?;
        let (key_hex, rest) = rest.split_once('@').ok_or_else(err)?;
        let (host, query) = rest.split_once('?').ok_or_else(err)?;
        let mut protocol = None;
        let mut discovery = None;
        for pair in query.split('&') {
            let (k, v) = pair.split_once('=').ok_or_else(err)?;
            match k {
                "protocol" => protocol = v.parse::<u16>().ok(),
                "discovery" => discovery = v.parse::<u16>().ok(),
                _ => {}
            }
        }
        let protocol = Port::new(protocol.ok_or_else(err)?);
        let discovery = Port::new(discovery.ok_or_else(err)?);
        Ok(PeerNode::from(
            NodeIdentifier::from_hex(key_hex).map_err(|e| crate::errors::CommError::ParseError(e))?,
            host.to_string(),
            protocol,
            discovery,
        ))
    }
}

impl std::fmt::Display for PeerNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_address())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn node_identifier_hex_round_trips() {
        let id = NodeIdentifier::new(vec![0xde, 0xad, 0xbe, 0xef]);
        assert_eq!(id.s_key(), "deadbeef");
        assert_eq!(NodeIdentifier::from_hex("deadbeef").unwrap(), id);
    }

    #[test]
    fn to_address_formats_rnode_uri() {
        let peer = PeerNode::from(NodeIdentifier::new(vec![1, 2, 3]), "example.com".into(), rchain_shared::refined::Port::new(40400), rchain_shared::refined::Port::new(40404));
        assert_eq!(
            peer.to_address(),
            "rnode://010203@example.com?protocol=40400&discovery=40404"
        );
    }

    #[test]
    fn from_address_round_trips() {
        let peer = PeerNode::from(
            NodeIdentifier::new(vec![0xde, 0xad]),
            "example.com".into(),
            rchain_shared::refined::Port::new(40400),
            rchain_shared::refined::Port::new(40404),
        );
        assert_eq!(PeerNode::from_address(&peer.to_address()).unwrap(), peer);
    }

    #[test]
    fn from_hex_rejects_malformed_input() {
        assert!(NodeIdentifier::from_hex("abc").is_err(), "odd-length must be rejected");
        assert!(NodeIdentifier::from_hex("zz").is_err(), "non-hex must be rejected");
    }

    #[test]
    fn from_address_rejects_malformed_uris() {
        assert!(PeerNode::from_address("not-an-rnode-uri").is_err());
        assert!(PeerNode::from_address("rnode://zz@example.com?protocol=1&discovery=2").is_err());
        assert!(PeerNode::from_address("rnode://0102@example.com").is_err(), "missing query");
    }
}
