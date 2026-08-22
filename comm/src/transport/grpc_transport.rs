//! gRPC request/response helpers and error mapping.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/GrpcTransport.scala`.

use rchain_models::comm::protocol::transport_layer_client::TransportLayerClient;
use rchain_models::comm::protocol::{tl_response, Protocol, TlRequest, TlResponse};
use tonic::{Code, Response, Status};

use crate::errors::{CommErr, CommError};
use crate::peer_node::PeerNode;
use crate::transport::chunker::{chunk_it, Blob};

/// Map a `TLResponse` to a unit result (port of `processResponse`).
fn process_response(peer: &PeerNode, response: Result<Response<TlResponse>, Status>) -> CommErr<()> {
    process_error(peer, response).and_then(|tlr| match tlr.into_inner().payload {
        Some(tl_response::Payload::Ack(_)) => Ok(()),
        Some(tl_response::Payload::InternalServerError(ise)) => Err(
            CommError::InternalCommunicationError(format!(
                "Got response: {}",
                String::from_utf8_lossy(&ise.error)
            )),
        ),
        None => Err(CommError::ProtocolException("Malformed response".to_string())),
    })
}

/// Map a tonic `Status` to a `CommError` (port of `processError`).
fn process_error<R>(peer: &PeerNode, response: Result<R, Status>) -> CommErr<R> {
    response.map_err(|status| match status.code() {
        Code::DeadlineExceeded => CommError::TimeOut,
        Code::Unavailable => CommError::PeerUnavailable(peer.clone()),
        Code::ResourceExhausted => CommError::MessageTooLarge(peer.clone()),
        Code::PermissionDenied => CommError::WrongNetwork(peer.clone(), status.message().to_string()),
        _ => CommError::ProtocolException(status.message().to_string()),
    })
}

/// Send a protocol message (unary) to a peer (port of `GrpcTransport.send`).
pub async fn send(
    client: &mut TransportLayerClient<tonic::transport::Channel>,
    peer: &PeerNode,
    msg: Protocol,
) -> CommErr<()> {
    let result = client.send(TlRequest { protocol: Some(msg) }).await;
    process_response(peer, result)
}

/// Stream a blob (client-streaming) to a peer (port of `GrpcTransport.stream`).
pub async fn stream(
    client: &mut TransportLayerClient<tonic::transport::Channel>,
    peer: &PeerNode,
    network_id: &str,
    blob: &Blob,
    packet_chunk_size: usize,
) -> CommErr<()> {
    let chunks = chunk_it(network_id, blob, packet_chunk_size)
        .map_err(CommError::InternalCommunicationError)?;
    let result = client.stream(tokio_stream::iter(chunks)).await;
    process_response(peer, result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_models::comm::protocol::tl_response;
    use crate::peer_node::NodeIdentifier;

    fn peer() -> PeerNode {
        PeerNode::from(NodeIdentifier::new(vec![1, 2, 3]), "host".into(), rchain_shared::refined::Port::new(40400), rchain_shared::refined::Port::new(40404))
    }

    fn ack() -> TlResponse {
        TlResponse {
            payload: Some(tl_response::Payload::Ack(Default::default())),
        }
    }

    fn internal_error(msg: &str) -> TlResponse {
        TlResponse {
            payload: Some(tl_response::Payload::InternalServerError(
                rchain_models::comm::protocol::InternalServerError {
                    error: msg.as_bytes().to_vec(),
                },
            )),
        }
    }

    #[test]
    fn process_response_ack_is_ok() {
        let r: Result<Response<TlResponse>, Status> = Ok(Response::new(ack()));
        assert_eq!(process_response(&peer(), r), Ok(()));
    }

    #[test]
    fn process_response_internal_error_maps() {
        let r: Result<Response<TlResponse>, Status> = Ok(Response::new(internal_error("boom")));
        assert_eq!(
            process_response(&peer(), r),
            Err(CommError::InternalCommunicationError(
                "Got response: boom".to_string()
            ))
        );
    }

    #[test]
    fn process_error_maps_status_codes() {
        let p = peer();
        let unavailable: Result<TlResponse, Status> =
            Err(Status::unavailable("down"));
        assert_eq!(
            process_error(&p, unavailable),
            Err(CommError::PeerUnavailable(p.clone()))
        );

        let deadline: Result<TlResponse, Status> =
            Err(Status::deadline_exceeded("slow"));
        assert_eq!(process_error(&p, deadline), Err(CommError::TimeOut));

        let too_large: Result<TlResponse, Status> =
            Err(Status::resource_exhausted("big"));
        assert_eq!(
            process_error(&p, too_large),
            Err(CommError::MessageTooLarge(p.clone()))
        );

        let wrong_net: Result<TlResponse, Status> =
            Err(Status::permission_denied("Wrong network id"));
        assert_eq!(
            process_error(&p, wrong_net),
            Err(CommError::WrongNetwork(p.clone(), "Wrong network id".to_string()))
        );
    }

    #[tokio::test]
    async fn send_round_trips_over_socket() {
        use std::sync::Arc;
        use std::time::Duration;

        use crate::rp::protocol_helper;
        use crate::transport::communication_response::CommunicationResponse;
        use crate::transport::generate_certificate_if_absent::generate_certificate;
        use crate::transport::grpc_transport_client::GrpcTransportClient;
        use crate::transport::grpc_transport_server::TransportLayerServer;
        use crate::transport::hostname_trust_manager::public_address_of_cert;
        use crate::transport::transport_layer::TransportLayer;
        use rustls::pki_types::CertificateDer;

        fn cert_node_id(cert_pem: &str) -> String {
            let (_, pem) = x509_parser::pem::parse_x509_pem(cert_pem.as_bytes()).unwrap();
            let der = CertificateDer::from(pem.contents);
            rchain_shared::base16::encode(&public_address_of_cert(&der).unwrap())
        }

        let (server_cert, server_key) = generate_certificate().unwrap();
        let (client_cert, client_key) = generate_certificate().unwrap();
        let port = std::net::TcpListener::bind("127.0.0.1:0")
            .unwrap()
            .local_addr()
            .unwrap()
            .port();

        // The server peer id is its cert's CN (base16 of the public address).
        let server_id = cert_node_id(&server_cert);
        let server_peer = PeerNode::from(
            NodeIdentifier::from_hex(&server_id).unwrap(),
            "127.0.0.1".to_string(),
            rchain_shared::refined::Port::new(port),
            rchain_shared::refined::Port::new(port),
        );

        let server = TransportLayerServer::new(
            server_peer.clone(),
            "testnet".to_string(),
            port,
            &server_cert,
            &server_key,
            16 * 1024 * 1024,
        )
        .unwrap();

        let (tx, rx) = tokio::sync::oneshot::channel();
        let tx = Arc::new(std::sync::Mutex::new(Some(tx)));
        tokio::spawn(async move {
            let dispatch_tx = tx.clone();
            let _ = server
                .serve(
                    move |protocol: Protocol| {
                        let tx = dispatch_tx.clone();
                        Box::pin(async move {
                            let sender = tx.lock().unwrap_or_else(|p| p.into_inner()).take();
                            if let Some(sender) = sender {
                                let _ = sender.send(protocol);
                            }
                            CommunicationResponse::handled_without_message()
                        })
                    },
                    |_blob| Box::pin(async {}),
                )
                .await;
        });

        // Give the server a moment to bind before the client connects.
        tokio::time::sleep(Duration::from_millis(200)).await;

        let client = GrpcTransportClient::new(
            "testnet".to_string(),
            &client_cert,
            &client_key,
            16 * 1024 * 1024,
            64 * 1024,
            100,
        )
        .unwrap();

        let heartbeat = protocol_helper::heartbeat(&server_peer, "testnet");
        client.send(&server_peer, heartbeat.clone()).await.unwrap();

        let received = tokio::time::timeout(Duration::from_secs(5), rx)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(received, heartbeat);
    }
}
