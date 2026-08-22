//! Server messages.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/messages.scala`.

use rchain_models::comm::protocol::Protocol;
use rchain_shared::refined::WireLen;

use crate::peer_node::PeerNode;

/// A message delivered by the transport server (port of `ServerMessage`).
pub trait ServerMessage {}

/// A complete (non-streamed) protocol message (port of `Send`).
#[derive(Clone, Debug, PartialEq)]
pub struct Send {
    pub msg: Protocol,
}

impl ServerMessage for Send {}

/// A reassembled streamed message (port of `StreamMessage`).
#[derive(Clone, Debug, PartialEq)]
pub struct StreamMessage {
    pub sender: PeerNode,
    pub type_id: String,
    pub key: String,
    pub compressed: bool,
    pub content_length: WireLen,
}

impl ServerMessage for StreamMessage {}
