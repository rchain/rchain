//! Faithful Rust port of the RChain `comm` module (peer-to-peer networking).
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/`. This crate ports the full networking stack:
//! peer identity, the Kademlia `PeerTable` + gRPC discovery RPC, the gRPC/TLS `TransportLayer`
//! (client/server/receiver with mutual TLS and node-id trust), the message buffers/`PacketOps`/
//! `StreamHandler`, the rp `Connect`/`HandleMessages` layers, and `WhoAmI`/`UPnP` external-IP
//! discovery (port-forwarding orchestration + IPv4 classification). The weupnp SSDP/SOAP gateway
//! discovery protocol is implemented in `upnp` (`gateway.rs`: SSDP M-SEARCH, device-description
//! fetch, and SOAP port-forwarding).

pub mod discovery;
pub mod errors;
pub mod peer_node;
pub mod rp;
pub mod transport;
pub mod upnp;
pub mod who_am_i;
