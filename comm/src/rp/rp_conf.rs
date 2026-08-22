//! RP (protocol) configuration.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/rp/RPConf.scala`.

use std::time::Duration;

use crate::peer_node::PeerNode;

/// Connection-clear configuration (port of `ClearConnectionsConf`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClearConnectionsConf {
    pub num_of_connections_pinged: usize,
}

/// The rp configuration (port of `RPConf`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RPConf {
    pub local: PeerNode,
    pub network_id: String,
    pub bootstrap: Option<PeerNode>,
    pub default_timeout: Duration,
    pub max_num_of_connections: usize,
    pub clear_connections: ClearConnectionsConf,
}
