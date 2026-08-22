//! The `SignaturesAlg` abstraction.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/signatures/SignaturesAlg.scala`.

use crate::errors::CryptoError;
use crate::private_key::PrivateKey;
use crate::public_key::PublicKey;

/// A digital signature algorithm.
pub trait SignaturesAlg {
    /// Verify `signature` over `data` against the public key `pub_key`.
    fn verify(&self, data: &[u8], signature: &[u8], pub_key: &[u8]) -> bool;

    /// Sign `data` with the secret key `sec`.
    fn sign(&self, data: &[u8], sec: &[u8]) -> Result<Vec<u8>, CryptoError>;

    /// Compute the public key corresponding to `sec`.
    fn to_public(&self, sec: &PrivateKey) -> Result<PublicKey, CryptoError>;

    /// Generate a fresh (private, public) key pair.
    fn new_key_pair(&self) -> (PrivateKey, PublicKey);

    /// The algorithm name.
    fn name(&self) -> &'static str;

    /// The signature length in bytes.
    fn sig_length(&self) -> usize;
}

/// Resolve an algorithm by (case-insensitive) name.
///
/// Ed25519 is deliberately disabled (RCHAIN-3560); only `"secp256k1"` and `"secp256k1:eth"` are
/// available, matching the Scala `SignaturesAlg.apply`.
pub fn from_algorithm(algorithm: &str) -> Option<&'static dyn SignaturesAlg> {
    match algorithm.to_ascii_lowercase().as_str() {
        // case Ed25519.name => Some(Ed25519) — disabled
        "secp256k1" => Some(&super::secp256k1::Secp256k1),
        "secp256k1:eth" => Some(&super::secp256k1_eth::Secp256k1Eth),
        _ => None,
    }
}
