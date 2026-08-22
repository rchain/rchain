//! Request → response ADT for the protocol dispatch layer.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/CommunicationResponse.scala`.

use rchain_models::comm::protocol::Protocol;

use crate::errors::CommError;

/// The result of handling an inbound protocol message (port of `CommunicationResponse`).
#[derive(Clone, Debug, PartialEq)]
pub enum CommunicationResponse {
    HandledWithMessage(Protocol),
    HandledWithoutMessage,
    NotHandled(CommError),
}

impl CommunicationResponse {
    pub fn handled_with_message(protocol: Protocol) -> Self {
        CommunicationResponse::HandledWithMessage(protocol)
    }

    pub fn handled_without_message() -> Self {
        CommunicationResponse::HandledWithoutMessage
    }

    pub fn not_handled(error: CommError) -> Self {
        CommunicationResponse::NotHandled(error)
    }
}
