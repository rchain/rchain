//! Server-side gRPC transport receiver (the `TransportLayer` service).
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/GrpcTransportReceiver.scala`. The per-peer
//! `LimitedBufferObservable` dispatch queues are simplified to a direct spawned dispatch; the
//! streamed-message circuit breaker and `PacketOps` cache round-trip are preserved.

use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};

use async_trait::async_trait;
use futures::channel::mpsc;
use futures::SinkExt;
use rchain_models::comm::protocol::transport_layer_server;
use rchain_models::comm::protocol::{
    chunk, tl_response, Ack, Chunk, InternalServerError, Protocol, TlRequest, TlResponse,
};
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};
use tokio::net::TcpListener;
use tokio_rustls::TlsAcceptor;
use tonic::{Request, Response, Status, Streaming};

use crate::peer_node::PeerNode;
use crate::rp::protocol_helper;
use crate::transport::chunker::Blob;
use crate::transport::communication_response::CommunicationResponse;
use crate::transport::packet_ops::{self, PacketCache};
use crate::transport::stream_handler::{self, Circuit, StreamError, Streamed};

/// A boxed `Send` future (helper alias for handler closures).
pub type BoxFuture<T> = Pin<Box<dyn Future<Output = T> + Send>>;

/// An incoming TLS connection, implementing tonic's `Connected`.
struct TlsIo(tokio_rustls::server::TlsStream<tokio::net::TcpStream>);

impl tonic::transport::server::Connected for TlsIo {
    type ConnectInfo = ();
    fn connect_info(&self) -> Self::ConnectInfo {}
}

impl AsyncRead for TlsIo {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        Pin::new(&mut self.0).poll_read(cx, buf)
    }
}

impl AsyncWrite for TlsIo {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        Pin::new(&mut self.0).poll_write(cx, buf)
    }
    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Pin::new(&mut self.0).poll_flush(cx)
    }
    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Pin::new(&mut self.0).poll_shutdown(cx)
    }
}

fn ack(local: &PeerNode, network_id: &str) -> TlResponse {
    TlResponse {
        payload: Some(tl_response::Payload::Ack(Ack {
            header: Some(protocol_helper::header(local, network_id)),
        })),
    }
}

fn internal_server_error(msg: &str) -> TlResponse {
    TlResponse {
        payload: Some(tl_response::Payload::InternalServerError(InternalServerError {
            error: protocol_helper::to_protocol_bytes(msg),
        })),
    }
}

fn stream_error_message(error: &StreamError) -> String {
    match error {
        StreamError::WrongNetworkId => "Could not receive stream! Wrong network id.".to_string(),
        StreamError::MaxSizeReached => "Max message size was reached.".to_string(),
        StreamError::NotFullMessage(s) => {
            format!("Received not full stream message, will not process. {s}")
        }
        StreamError::Unexpected(t) => format!("Could not receive stream! {t}"),
    }
}

/// Bound on concurrent inbound-message dispatches (the Scala per-peer `LimitedBufferObservable`
/// bounded-queue analog). A flood that cannot acquire a slot is rejected with `ResourceExhausted`
/// rather than spawning an unbounded number of tasks.
const MAX_CONCURRENT_DISPATCH: usize = 1024;
/// Bound on concurrent inbound TLS handshakes (M1). A stalled client handshake must not serialize
/// every subsequent inbound connection, so each handshake is spawned and its result fed through a
/// bounded channel.
const MAX_CONCURRENT_HANDSHAKES: usize = 128;

/// The inbound gRPC `TransportLayer` service (port of the `RoutingGrpcMonix.TransportLayer` impl).
pub struct GrpcTransportReceiver {
    local: PeerNode,
    network_id: String,
    max_stream_message_size: i64,
    dispatch: Arc<dyn Fn(Protocol) -> BoxFuture<CommunicationResponse> + Send + Sync>,
    handle_streamed: Arc<dyn Fn(Blob) -> BoxFuture<()> + Send + Sync>,
    dispatch_slots: Arc<tokio::sync::Semaphore>,
}

#[async_trait]
impl transport_layer_server::TransportLayer for GrpcTransportReceiver {
    async fn send(
        &self,
        request: Request<TlRequest>,
    ) -> Result<Response<TlResponse>, Status> {
        let protocol = request
            .into_inner()
            .protocol
            .ok_or_else(|| Status::invalid_argument("missing protocol"))?;

        // SslSessionServerInterceptor equivalent: reject wrong-network senders.
        if let Some(header) = &protocol.header {
            if header.network_id != self.network_id {
                let nid = if header.network_id.is_empty() {
                    "<empty>"
                } else {
                    &header.network_id
                };
                return Err(Status::permission_denied(format!(
                    "Wrong network id '{nid}'. This node runs on network '{}'",
                    self.network_id
                )));
            }
        }

        let permit = match self.dispatch_slots.clone().try_acquire_owned() {
            Ok(p) => p,
            Err(_) => return Err(Status::resource_exhausted("dispatch queue full")),
        };
        let dispatch = self.dispatch.clone();
        tokio::spawn(async move {
            let _permit = permit;
            let _ = (dispatch)(protocol).await;
        });
        Ok(Response::new(ack(&self.local, &self.network_id)))
    }

    async fn stream(
        &self,
        request: Request<Streaming<Chunk>>,
    ) -> Result<Response<TlResponse>, Status> {
        let mut incoming = request.into_inner();
        let mut chunks = Vec::new();
        // Enforce the size cap *while* draining, so a peer cannot stream an unbounded number of
        // chunks before the circuit breaker runs in `stream_handler::collect`.
        let mut received: i64 = 0;
        while let Some(chunk) = incoming.message().await? {
            if let Some(chunk::Content::Data(d)) = &chunk.content {
                received += d.content_data.len() as i64;
                if received > self.max_stream_message_size {
                    return Ok(Response::new(internal_server_error(&stream_error_message(
                        &StreamError::MaxSizeReached,
                    ))));
                }
            }
            chunks.push(chunk);
        }

        let mut cache = PacketCache::new();
        let key = packet_ops::create_cache_entry("packet_send/", &mut cache);
        let init = Streamed::new(key);

        let network_id = self.network_id.clone();
        let max_stream_message_size = self.max_stream_message_size;
        let breaker = move |streamed: &Streamed| {
            if let Some(header) = &streamed.header {
                if header.network_id != network_id {
                    return Circuit::Opened(StreamError::WrongNetworkId);
                }
            }
            if streamed.read_so_far > max_stream_message_size {
                return Circuit::Opened(StreamError::MaxSizeReached);
            }
            Circuit::Closed
        };

        let collected = stream_handler::collect(&init, &chunks, &breaker, &mut cache)
            .and_then(|stmd| stream_handler::to_result(&stmd));

        match collected {
            Ok(msg) => match stream_handler::restore(
                &msg,
                &mut cache,
                usize::try_from(self.max_stream_message_size).unwrap_or(usize::MAX),
            ) {
                Ok(blob) => {
                    let handle = self.handle_streamed.clone();
                    tokio::spawn(async move {
                        (handle)(blob).await;
                    });
                    Ok(Response::new(ack(&self.local, &self.network_id)))
                }
                Err(e) => Ok(Response::new(internal_server_error(&e))),
            },
            Err(e) => Ok(Response::new(internal_server_error(&stream_error_message(&e)))),
        }
    }
}

/// Bind a mutual-TLS listener and serve the transport receiver.
pub async fn serve(
    local: PeerNode,
    network_id: String,
    port: u16,
    tls: Arc<rustls::ServerConfig>,
    max_stream_message_size: i64,
    dispatch: Arc<dyn Fn(Protocol) -> BoxFuture<CommunicationResponse> + Send + Sync>,
    handle_streamed: Arc<dyn Fn(Blob) -> BoxFuture<()> + Send + Sync>,
) -> Result<(), String> {
    // Faithful to Scala: the protocol server binds to `0.0.0.0` (the `protocol-server.host` config
    // is the *advertised* address, not the bind address). The bind is left as-is; the fix here is
    // concurrent (not serialized) TLS handshakes.
    let listener = TcpListener::bind(("0.0.0.0", port))
        .await
        .map_err(|e| e.to_string())?;
    let acceptor = TlsAcceptor::from(tls);

    // Concurrent TLS handshakes (M1): accept connections in a tight loop, hand off each handshake to
    // a spawned task, and feed the accepted TLS streams to tonic through a bounded channel. A slow
    // handshake no longer blocks the accept loop.
    let (tx, rx) = mpsc::channel::<Result<TlsIo, std::io::Error>>(MAX_CONCURRENT_HANDSHAKES);
    tokio::spawn(async move {
        loop {
            let tcp = match listener.accept().await {
                Ok((tcp, _)) => tcp,
                Err(_) => break,
            };
            let acceptor = acceptor.clone();
            let tx = tx.clone();
            tokio::spawn(async move {
                let mut tx = tx;
                if let Ok(tls) = acceptor.accept(tcp).await {
                    let _ = tx.send(Ok(TlsIo(tls))).await;
                }
            });
        }
    });
    let incoming = rx;

    let service = GrpcTransportReceiver {
        local,
        network_id,
        max_stream_message_size,
        dispatch,
        handle_streamed,
        dispatch_slots: Arc::new(tokio::sync::Semaphore::new(MAX_CONCURRENT_DISPATCH)),
    };

    tonic::transport::Server::builder()
        .add_service(transport_layer_server::TransportLayerServer::new(service))
        .serve_with_incoming(incoming)
        .await
        .map_err(|e| e.to_string())
}
