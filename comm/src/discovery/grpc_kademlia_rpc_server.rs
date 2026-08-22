//! gRPC Kademlia RPC server (plaintext).
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/discovery/GrpcKademliaRPCServer.scala`.

use std::future::Future;
use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::Arc;

use async_trait::async_trait;
use rchain_models::comm::discovery::kademlia_rpc_service_server::{
    KademliaRpcService, KademliaRpcServiceServer,
};
use rchain_models::comm::discovery::{Lookup, LookupResponse, Ping, Pong};
use rchain_shared::rate_limiter::RateLimiter;
use tonic::{Request, Response, Status};

use crate::discovery::{to_node, to_peer_node};
use crate::peer_node::PeerNode;

type BoxFuture<T> = Pin<Box<dyn Future<Output = T> + Send>>;

/// Rate limit (requests/second) on the plaintext Kademlia RPC (documented Scala deviation: Scala
/// has no limit). Bounds sybil/routing-table pollution and peer-enumeration amplification from the
/// unauthenticated `0.0.0.0:40404` surface.
const DEFAULT_KADEMLIA_RATE_LIMIT_PER_SEC: u64 = 100;

/// The Kademlia RPC service (port of `GrpcKademliaRPCServer`).
pub struct GrpcKademliaRpcServer {
    network_id: String,
    ping_handler: Arc<dyn Fn(PeerNode) -> BoxFuture<()> + Send + Sync>,
    lookup_handler: Arc<dyn Fn(PeerNode, Vec<u8>) -> BoxFuture<Vec<PeerNode>> + Send + Sync>,
    rate_limiter: Arc<RateLimiter>,
}

impl GrpcKademliaRpcServer {
    pub fn new<F, G>(network_id: String, ping_handler: F, lookup_handler: G) -> Self
    where
        F: Fn(PeerNode) -> BoxFuture<()> + Send + Sync + 'static,
        G: Fn(PeerNode, Vec<u8>) -> BoxFuture<Vec<PeerNode>> + Send + Sync + 'static,
    {
        GrpcKademliaRpcServer {
            network_id,
            ping_handler: Arc::new(ping_handler),
            lookup_handler: Arc::new(lookup_handler),
            rate_limiter: Arc::new(RateLimiter::new(DEFAULT_KADEMLIA_RATE_LIMIT_PER_SEC)),
        }
    }
}

#[async_trait]
impl KademliaRpcService for GrpcKademliaRpcServer {
    async fn send_ping(&self, request: Request<Ping>) -> Result<Response<Pong>, Status> {
        if !self.rate_limiter.allow() {
            return Err(Status::resource_exhausted("kademlia rate limit exceeded"));
        }
        let ping = request.into_inner();
        if ping.network_id == self.network_id {
            if let Some(sender) = ping.sender.as_ref() {
                if let Ok(peer) = to_peer_node(sender) {
                    (self.ping_handler)(peer).await;
                }
            }
        }
        Ok(Response::new(Pong {
            network_id: self.network_id.clone(),
        }))
    }

    async fn send_lookup(
        &self,
        request: Request<Lookup>,
    ) -> Result<Response<LookupResponse>, Status> {
        if !self.rate_limiter.allow() {
            return Err(Status::resource_exhausted("kademlia rate limit exceeded"));
        }
        let lookup = request.into_inner();
        let nodes = if lookup.network_id == self.network_id {
            match lookup.sender.as_ref().and_then(|s| to_peer_node(s).ok()) {
                Some(sender) => {
                    let peers = (self.lookup_handler)(sender, lookup.id).await;
                    peers.iter().map(to_node).collect()
                }
                None => Vec::new(),
            }
        } else {
            Vec::new()
        };
        Ok(Response::new(LookupResponse {
            nodes,
            network_id: self.network_id.clone(),
        }))
    }
}

/// Serve the Kademlia RPC on the given port (plaintext).
pub async fn serve(addr: SocketAddr, service: GrpcKademliaRpcServer) -> Result<(), String> {
    tonic::transport::Server::builder()
        .add_service(KademliaRpcServiceServer::new(service))
        .serve(addr)
        .await
        .map_err(|e| e.to_string())
}
