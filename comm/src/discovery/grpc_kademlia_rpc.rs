//! gRPC Kademlia RPC client (plaintext).
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/discovery/GrpcKademliaRPC.scala`.

use std::time::Duration;

use async_trait::async_trait;
use rchain_models::comm::discovery::kademlia_rpc_service_client::KademliaRpcServiceClient;
use rchain_models::comm::discovery::{Lookup, Ping};
use tonic::transport::{Channel, Endpoint};

use crate::discovery::{to_node, to_peer_node, KademliaRpc};
use crate::peer_node::PeerNode;

/// The gRPC Kademlia client (port of `GrpcKademliaRPC`).
pub struct GrpcKademliaRpc {
    local: PeerNode,
    network_id: String,
    timeout: Duration,
}

impl GrpcKademliaRpc {
    pub fn new(local: PeerNode, network_id: String, timeout: Duration) -> Self {
        GrpcKademliaRpc {
            local,
            network_id,
            timeout,
        }
    }

    async fn client(&self, peer: &PeerNode) -> Result<KademliaRpcServiceClient<Channel>, String> {
        let endpoint = Endpoint::from_shared(format!(
            "http://{}:{}",
            peer.endpoint.host,
            u16::from(peer.endpoint.udp_port)
        ))
        .map_err(|e| e.to_string())?;
        let channel = endpoint.connect().await.map_err(|e| e.to_string())?;
        Ok(KademliaRpcServiceClient::new(channel))
    }
}

#[async_trait]
impl KademliaRpc for GrpcKademliaRpc {
    async fn ping(&self, peer: &PeerNode) -> bool {
        let ping = Ping {
            sender: Some(to_node(&self.local)),
            network_id: self.network_id.clone(),
        };
        let result = async {
            let mut client = self.client(peer).await?;
            client
                .send_ping(ping)
                .await
                .map(|r| r.into_inner())
                .map_err(|e| e.to_string())
        };
        match tokio::time::timeout(self.timeout, result).await {
            Ok(Ok(pong)) => pong.network_id == self.network_id,
            _ => false,
        }
    }

    async fn lookup(&self, key: &[u8], peer: &PeerNode) -> Vec<PeerNode> {
        let lookup = Lookup {
            id: key.to_vec(),
            sender: Some(to_node(&self.local)),
            network_id: self.network_id.clone(),
        };
        let result = async {
            let mut client = self.client(peer).await?;
            client
                .send_lookup(lookup)
                .await
                .map(|r| r.into_inner())
                .map_err(|e| e.to_string())
        };
        match tokio::time::timeout(self.timeout, result).await {
            Ok(Ok(response)) if response.network_id == self.network_id => {
                response
                    .nodes
                    .iter()
                    .filter_map(|n| to_peer_node(n).ok())
                    .collect()
            }
            _ => Vec::new(),
        }
    }
}
