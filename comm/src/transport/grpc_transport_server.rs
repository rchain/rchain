//! Server-side transport layer wrapper.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/GrpcTransportServer.scala`.

use std::sync::Arc;

use rchain_models::comm::protocol::Protocol;

use crate::peer_node::PeerNode;
use crate::transport::chunker::Blob;
use crate::transport::communication_response::CommunicationResponse;
use crate::transport::grpc_transport_receiver::{self, BoxFuture};
use crate::transport::hostname_trust_manager;

/// The server-side transport (port of `TransportLayerServer` / `GrpcTransportServer`).
pub struct TransportLayerServer {
    local: PeerNode,
    network_id: String,
    port: u16,
    tls: Arc<rustls::ServerConfig>,
    max_stream_message_size: i64,
}

impl TransportLayerServer {
    pub fn new(
        local: PeerNode,
        network_id: String,
        port: u16,
        cert_pem: &str,
        key_pem: &str,
        max_stream_message_size: i64,
    ) -> Result<Self, String> {
        let tls = hostname_trust_manager::server_config(cert_pem, key_pem)?;
        Ok(TransportLayerServer {
            local,
            network_id,
            port,
            tls,
            max_stream_message_size,
        })
    }

    /// Serve the transport, dispatching inbound protocol messages to `dispatch` and reassembled
    /// streamed blobs to `handle_streamed`.
    pub async fn serve<D, S>(&self, dispatch: D, handle_streamed: S) -> Result<(), String>
    where
        D: Fn(Protocol) -> BoxFuture<CommunicationResponse> + Send + Sync + 'static,
        S: Fn(Blob) -> BoxFuture<()> + Send + Sync + 'static,
    {
        grpc_transport_receiver::serve(
            self.local.clone(),
            self.network_id.clone(),
            self.port,
            self.tls.clone(),
            self.max_stream_message_size,
            Arc::new(dispatch),
            Arc::new(handle_streamed),
        )
        .await
    }
}
