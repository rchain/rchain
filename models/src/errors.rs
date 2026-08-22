//! Model (de)serialization errors.
//!
//! Hard error type for the models crate's protobuf/packet decode boundary. Free-form prost decode
//! messages are carried as `String`; the structural cases (a missing field/variant, a mismatched
//! packet type tag) are typed.

use std::fmt;

/// An error decoding a model from its protobuf or packet representation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ModelsError {
    /// Protobuf/byte decoding failed.
    Decode(String),
    /// A protobuf message was malformed (a missing field or unexpected variant).
    Malformed(&'static str),
    /// A packet's type tag did not match the expected tag.
    PacketTypeMismatch { got: String, expected: String },
    /// A fixed-width value had the wrong byte length (validate-on-ingress).
    Length { got: usize, expected: usize },
}

impl fmt::Display for ModelsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModelsError::Decode(m) => write!(f, "decode error: {m}"),
            ModelsError::Malformed(m) => write!(f, "malformed: {m}"),
            ModelsError::PacketTypeMismatch { got, expected } => {
                write!(f, "Got {got} packet - need {expected} packet")
            }
            ModelsError::Length { got, expected } => {
                write!(f, "expected {expected} bytes, got {got}")
            }
        }
    }
}

impl std::error::Error for ModelsError {}

impl From<rchain_crypto::errors::CryptoError> for ModelsError {
    fn from(e: rchain_crypto::errors::CryptoError) -> Self {
        match e {
            rchain_crypto::errors::CryptoError::InvalidLength { expected, actual } => {
                ModelsError::Length {
                    got: actual,
                    expected,
                }
            }
            other => ModelsError::Decode(other.to_string()),
        }
    }
}
