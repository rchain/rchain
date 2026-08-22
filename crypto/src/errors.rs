//! Cryptographic errors.
//!
//! Hard error type for the crypto crate's declared partiality boundaries (fixed-width coercion,
//! key-material validation, box encryption/decryption, hex decoding). Mirrors the typed-fix column
//! of `spec/TYPE-SYSTEM.md` §3.2.

use std::fmt;

/// A cryptographic error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CryptoError {
    /// Secret/public key material is invalid for the algorithm.
    InvalidKey,
    /// A fixed-width coercion received the wrong length.
    InvalidLength { expected: usize, actual: usize },
    /// Box (XSalsa20-Poly1305) encryption/decryption failed.
    EncryptionFailed,
    /// A hex string was malformed or not the expected length.
    InvalidHex,
}

impl fmt::Display for CryptoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CryptoError::InvalidKey => write!(f, "invalid key material"),
            CryptoError::InvalidLength { expected, actual } => {
                write!(f, "invalid length: expected {expected} bytes, got {actual}")
            }
            CryptoError::EncryptionFailed => write!(f, "box encryption failed"),
            CryptoError::InvalidHex => write!(f, "invalid hex string"),
        }
    }
}

impl std::error::Error for CryptoError {}

/// Convenience result alias.
pub type Result<A> = std::result::Result<A, CryptoError>;
