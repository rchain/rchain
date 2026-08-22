//! A private key.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/PrivateKey.scala`.

/// A private key, as a raw byte array.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct PrivateKey(pub Vec<u8>);

impl PrivateKey {
    /// Construct from raw bytes.
    pub fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    /// The raw key bytes.
    pub fn bytes(&self) -> &[u8] {
        &self.0
    }
}
