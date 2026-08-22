//! Client-side transport layer (gRPC over mutual TLS).
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/GrpcTransportClient.scala`. The
//! per-peer `StreamObservable` (bounded key-only queue) is simplified to a direct per-peer stream
//! task; the packet cache/`PacketOps` round-trip is preserved for `stream`.

use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use futures::stream::FuturesUnordered;
use futures::StreamExt;
use http::Uri;
use hyper_util::rt::TokioIo;
use rchain_models::comm::protocol::transport_layer_client::TransportLayerClient;
use rchain_models::comm::protocol::Protocol;
use rustls::pki_types::ServerName;
use tokio::net::TcpStream;
use tokio_rustls::TlsConnector;
use tonic::transport::{Channel, Endpoint};
use tower::service_fn;

use crate::errors::{CommErr, CommError};
use crate::peer_node::PeerNode;
use crate::transport::chunker::Blob;
use crate::transport::grpc_transport;
use crate::transport::hostname_trust_manager;
use crate::transport::transport_layer::TransportLayer;

/// The default send timeout (port of `GrpcTransportClient.DefaultSendTimeout`).
pub const DEFAULT_SEND_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(5);

/// The gRPC transport client (port of `GrpcTransportClient`).
#[derive(Clone)]
pub struct GrpcTransportClient {
    network_id: Arc<String>,
    packet_chunk_size: usize,
    max_message_size: usize,
    tls: Arc<rustls::ClientConfig>,
    channels: Arc<tokio::sync::Mutex<HashMap<PeerNode, Channel>>>,
}

impl GrpcTransportClient {
    pub fn new(
        network_id: String,
        cert_pem: &str,
        key_pem: &str,
        max_message_size: usize,
        packet_chunk_size: usize,
        _client_queue_size: usize,
    ) -> Result<Self, String> {
        let tls = hostname_trust_manager::client_config(cert_pem, key_pem)?;
        Ok(GrpcTransportClient {
            network_id: Arc::new(network_id),
            packet_chunk_size,
            max_message_size,
            tls,
            channels: Arc::new(tokio::sync::Mutex::new(HashMap::new())),
        })
    }

    /// Create a TLS channel to `peer`, verifying the server cert against the peer's node id.
    async fn create_channel(&self, peer: &PeerNode) -> Result<Channel, CommError> {
        let endpoint = Endpoint::from_shared(format!(
            "https://{}:{}",
            peer.endpoint.host,
            u16::from(peer.endpoint.tcp_port)
        ))
        .map_err(|e| CommError::ParseError(e.to_string()))?;

        let connector = TlsConnector::from(self.tls.clone());
        let server_name = ServerName::try_from(peer.id.to_string())
            .map_err(|e| CommError::ParseError(e.to_string()))?;
        let host = peer.endpoint.host.clone();
        let port = peer.endpoint.tcp_port;

        Ok(endpoint.connect_with_connector_lazy(service_fn(move |_: Uri| {
            let connector = connector.clone();
            let server_name = server_name.clone();
            let host = host.clone();
            async move {
                let tcp = TcpStream::connect((host.as_str(), u16::from(port))).await?;
                let tls = connector.connect(server_name, tcp).await?;
                Ok::<_, std::io::Error>(TokioIo::new(tls))
            }
        })))
    }

    async fn get_channel(&self, peer: &PeerNode) -> Result<Channel, CommError> {
        let mut channels = self.channels.lock().await;
        if let Some(channel) = channels.get(peer) {
            return Ok(channel.clone());
        }
        let channel = self.create_channel(peer).await?;
        channels.insert(peer.clone(), channel.clone());
        Ok(channel)
    }
}

#[async_trait]
impl TransportLayer for GrpcTransportClient {
    async fn send(&self, peer: &PeerNode, msg: Protocol) -> CommErr<()> {
        let channel = self.get_channel(peer).await?;
        let mut client = TransportLayerClient::new(channel)
            .max_decoding_message_size(self.max_message_size);
        tokio::time::timeout(DEFAULT_SEND_TIMEOUT, grpc_transport::send(&mut client, peer, msg))
            .await
            .map_err(|_| CommError::TimeOut)?
    }

    async fn broadcast(&self, peers: &[PeerNode], msg: Protocol) -> Vec<CommErr<()>> {
        let sends: FuturesUnordered<_> = peers
            .iter()
            .map(|peer| self.send(peer, msg.clone()))
            .collect();
        sends.collect().await
    }

    async fn stream(&self, peers: &[PeerNode], blob: Blob) {
        let tasks: Vec<_> = peers
            .iter()
            .map(|peer| {
                let this = self.clone();
                let peer = peer.clone();
                let blob = blob.clone();
                tokio::spawn(async move {
                    let channel = match this.get_channel(&peer).await {
                        Ok(c) => c,
                        Err(_) => return,
                    };
                    let mut client = TransportLayerClient::new(channel)
                        .max_decoding_message_size(this.max_message_size);
                    let _ = grpc_transport::stream(
                        &mut client,
                        &peer,
                        &this.network_id,
                        &blob,
                        this.packet_chunk_size,
                    )
                    .await;
                })
            })
            .collect();
        for task in tasks {
            let _ = task.await;
        }
    }
}
