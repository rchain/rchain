//! Communication errors.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/errors.scala`.

use crate::peer_node::PeerNode;

/// A communication error (port of the sealed `CommError`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CommError {
    UnknownCommError(String),
    DatagramSizeError(i32),
    HeaderNotAvailable,
    ProtocolException(String),
    UnknownProtocolError(String),
    PublicKeyNotAvailable(PeerNode),
    ParseError(String),
    EncryptionHandshakeIncorrectlySigned,
    BootstrapNotProvided,
    PeerNodeNotFound(PeerNode),
    PeerUnavailable(PeerNode),
    WrongNetwork(PeerNode, String),
    MessageTooLarge(PeerNode),
    CouldNotConnectToBootstrap,
    InternalCommunicationError(String),
    TimeOut,
    UpstreamNotAvailable,
    UnexpectedMessage(String),
    SenderNotAvailable,
    PongNotReceivedForPing(PeerNode),
    UnableToStorePacket(String),
    UnableToRestorePacket(String),
}

/// The `CommErr` result alias (port of `CommError.CommErr`).
pub type CommErr<A> = Result<A, CommError>;

impl CommError {
    /// The human-readable message (port of `CommError.errorMessage`).
    pub fn message(&self) -> String {
        match self {
            CommError::PeerUnavailable(_) => "Peer is currently unavailable".to_string(),
            CommError::MessageTooLarge(p) => {
                format!("Message rejected by peer {p} because it was too large")
            }
            CommError::PongNotReceivedForPing(_) => {
                "Peer is behind a firewall and can't be accessed from outside".to_string()
            }
            CommError::CouldNotConnectToBootstrap => {
                "Node could not connect to bootstrap node".to_string()
            }
            CommError::TimeOut => "Timeout".to_string(),
            CommError::InternalCommunicationError(msg) => {
                format!("Internal communication error. {msg}")
            }
            CommError::UnknownProtocolError(msg) => format!("Unknown protocol error. {msg}"),
            CommError::UnableToStorePacket(p) => {
                format!("Could not serialize packet {p}.")
            }
            CommError::UnableToRestorePacket(p) => {
                format!("Could not deserialize packet {p}.")
            }
            CommError::ProtocolException(msg) => format!("Protocol error. {msg}"),
            other => format!("{other:?}"),
        }
    }
}

impl std::fmt::Display for CommError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message())
    }
}

impl std::error::Error for CommError {}
