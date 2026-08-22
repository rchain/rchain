//! gRPC service adapters (port of `api/DeployGrpcServiceV1.scala`,
//! `api/ProposeGrpcServiceV1.scala`, `api/ReplGrpcService.scala`, and `runtime/GrpcServices.scala`).
//!
//! The monix-gRPC transport is mapped to plain async service structs over `BlockApi`/`RhoRuntime`;
//! the protobuf `oneof` responses collapse to `Result<_, ServiceError>` (the Scala `Either`). The
//! tonic (gRPC) bindings for these adapters live in [`tonic`].

mod deploy_grpc_service_v1;
mod propose_grpc_service_v1;
mod repl_grpc_service;
pub mod tonic;

pub use deploy_grpc_service_v1::DeployGrpcServiceV1;
pub use propose_grpc_service_v1::ProposeGrpcServiceV1;
pub use repl_grpc_service::{CmdRequest, EvalRequest, ReplGrpcService, ReplResponse};

use std::sync::Arc;

use rchain_casper::api::block_api::BlockApi;
use rchain_casper::api::block_report_api::BlockReportApi;
use rchain_rholang::runtime::RhoRuntime;
use rchain_shared::rate_limiter::RateLimiter;

/// The trio of node gRPC services (port of `GrpcServices`).
pub struct GrpcServices {
    pub deploy: DeployGrpcServiceV1,
    pub propose: ProposeGrpcServiceV1,
    pub repl: ReplGrpcService,
}

impl GrpcServices {
    /// Build the service trio from the block APIs + runtime (port of `GrpcServices.build`).
    pub fn build(
        block_api: Arc<dyn BlockApi>,
        block_report_api: Arc<BlockReportApi>,
        runtime: Arc<RhoRuntime>,
    ) -> GrpcServices {
        let repl = ReplGrpcService::new(runtime);
        let deploy = DeployGrpcServiceV1::new(block_api.clone(), block_report_api);
        let propose = ProposeGrpcServiceV1::new(block_api);
        GrpcServices {
            deploy,
            propose,
            repl,
        }
    }

}

/// The deploy request rate limit (requests/second) applied to the external (unauthenticated) gRPC
/// server. Documented Scala deviation: Scala binds the deploy service to `0.0.0.0` with no limit.
pub const DEFAULT_API_RATE_LIMIT_PER_SEC: u64 = 100;

/// Serve the external gRPC service (deploy) on `addr` (port of the external `GrpcServer`).
pub async fn serve_deploy(
    deploy: DeployGrpcServiceV1,
    addr: std::net::SocketAddr,
    max_message_size: usize,
) -> Result<(), String> {
    use rchain_models::proto::casper::deploy_service_server::DeployServiceServer;
    use ::tonic::service::interceptor::InterceptedService;

    let limiter = Arc::new(RateLimiter::new(DEFAULT_API_RATE_LIMIT_PER_SEC));
    let server = DeployServiceServer::new(deploy).max_decoding_message_size(max_message_size);
    let service = InterceptedService::new(server, move |req: ::tonic::Request<()>| {
        if limiter.allow() {
            Ok(req)
        } else {
            Err(::tonic::Status::resource_exhausted("deploy rate limit exceeded"))
        }
    });

    ::tonic::transport::Server::builder()
        .add_service(service)
        .serve(addr)
        .await
        .map_err(|e| e.to_string())
}

/// Serve the internal gRPC services (propose + repl) on `addr` (port of the internal `GrpcServer`).
pub async fn serve_internal(
    propose: ProposeGrpcServiceV1,
    repl: ReplGrpcService,
    addr: std::net::SocketAddr,
    max_message_size: usize,
) -> Result<(), String> {
    use rchain_models::proto::casper::propose_service_server::ProposeServiceServer;
    use rchain_models::proto::repl::repl_server::ReplServer;

    ::tonic::transport::Server::builder()
        .add_service(ProposeServiceServer::new(propose).max_decoding_message_size(max_message_size))
        .add_service(ReplServer::new(repl).max_decoding_message_size(max_message_size))
        .serve(addr)
        .await
        .map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rate_limiter_throttles_over_capacity() {
        let limiter = RateLimiter::new(3);
        // The first `max_per_sec` requests within the same window are admitted…
        assert!(limiter.allow());
        assert!(limiter.allow());
        assert!(limiter.allow());
        // …and the next is rejected (the 4 calls happen well within one second).
        assert!(!limiter.allow());
    }
}
