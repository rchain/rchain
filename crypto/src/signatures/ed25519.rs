//! Ed25519 signatures.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/signatures/Ed25519.scala`. The Scala wraps
//! kalium (libsodium); the Rust port wraps `ed25519-dalek`, using the non-strict `verify` to match
//! libsodium's malleable-signature acceptance.

use super::signatures_alg::SignaturesAlg;
use crate::errors::CryptoError;
use crate::private_key::PrivateKey;
use crate::public_key::PublicKey;
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;

/// The Ed25519 algorithm.
pub struct Ed25519;

impl Ed25519 {
    /// Compute the 32-byte public key from a 32-byte secret (seed) key.
    pub fn to_public_bytes(sec: &[u8]) -> Result<Vec<u8>, CryptoError> {
        let arr: [u8; 32] = sec.try_into().map_err(|_| CryptoError::InvalidLength {
            expected: 32,
            actual: sec.len(),
        })?;
        Ok(SigningKey::from_bytes(&arr).verifying_key().to_bytes().to_vec())
    }

    /// Sign `data` with a 32-byte secret (seed) key, returning a 64-byte signature.
    pub fn sign_bytes(data: &[u8], sec: &[u8]) -> Result<Vec<u8>, CryptoError> {
        let arr: [u8; 32] = sec.try_into().map_err(|_| CryptoError::InvalidLength {
            expected: 32,
            actual: sec.len(),
        })?;
        Ok(SigningKey::from_bytes(&arr).sign(data).to_bytes().to_vec())
    }

    /// Verify a 64-byte signature against a 32-byte public key.
    pub fn verify_bytes(data: &[u8], signature: &[u8], pub_key: &[u8]) -> bool {
        let Ok(pk) = <[u8; 32]>::try_from(pub_key) else {
            return false;
        };
        let Ok(vk) = VerifyingKey::from_bytes(&pk) else {
            return false;
        };
        let Ok(sig) = Signature::from_slice(signature) else {
            return false;
        };
        vk.verify(data, &sig).is_ok()
    }
}

impl SignaturesAlg for Ed25519 {
    fn verify(&self, data: &[u8], signature: &[u8], pub_key: &[u8]) -> bool {
        Ed25519::verify_bytes(data, signature, pub_key)
    }

    fn sign(&self, data: &[u8], sec: &[u8]) -> Result<Vec<u8>, CryptoError> {
        Ed25519::sign_bytes(data, sec)
    }

    fn to_public(&self, sec: &PrivateKey) -> Result<PublicKey, CryptoError> {
        Ok(PublicKey::new(Ed25519::to_public_bytes(sec.bytes())?))
    }

    fn new_key_pair(&self) -> (PrivateKey, PublicKey) {
        let signing = SigningKey::generate(&mut OsRng);
        let sec = signing.to_bytes().to_vec();
        let pub_key = signing.verifying_key().to_bytes().to_vec();
        (PrivateKey::new(sec), PublicKey::new(pub_key))
    }

    fn name(&self) -> &'static str {
        "ed25519"
    }

    fn sig_length(&self) -> usize {
        64
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::base16;

    #[test]
    fn computes_public_key_from_secret_key() {
        let sec = base16::unsafe_decode(
            "b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd",
        );
        assert_eq!(
            base16::encode(&Ed25519::to_public_bytes(&sec).expect("compute ed25519 public key")),
            "77f48b59caeda77751ed138b0ec667ff50f8768c25d48309a8f386a2bad187fb"
        );
    }

    #[test]
    fn verifies_the_given_signature() {
        let data = base16::unsafe_decode(
            "916c7d1d268fc0e77c1bef238432573c39be577bbea0998936add2b50a653171ce18a542b0b7f96c1691a3be6031522894a8634183eda38798a0c5d5d79fbd01dd04a8646d71873b77b221998a81922d8105f892316369d5224c9983372d2313c6b1f4556ea26ba49d46e8b561e0fc76633ac9766e68e21fba7edca93c4c7460376d7f3ac22ff372c18f613f2ae2e856af40",
        );
        let sig = base16::unsafe_decode(
            "6bd710a368c1249923fc7a1610747403040f0cc30815a00f9ff548a896bbda0b4eb2ca19ebcf917f0f34200a9edbad3901b64ab09cc5ef7b9bcc3c40c0ff7509",
        );
        let pub_key = base16::unsafe_decode(
            "77f48b59caeda77751ed138b0ec667ff50f8768c25d48309a8f386a2bad187fb",
        );
        assert!(Ed25519::verify_bytes(&data, &sig, &pub_key));
    }

    #[test]
    fn creates_a_signature() {
        let data = base16::unsafe_decode(
            "916c7d1d268fc0e77c1bef238432573c39be577bbea0998936add2b50a653171ce18a542b0b7f96c1691a3be6031522894a8634183eda38798a0c5d5d79fbd01dd04a8646d71873b77b221998a81922d8105f892316369d5224c9983372d2313c6b1f4556ea26ba49d46e8b561e0fc76633ac9766e68e21fba7edca93c4c7460376d7f3ac22ff372c18f613f2ae2e856af40",
        );
        let expected = base16::unsafe_decode(
            "6bd710a368c1249923fc7a1610747403040f0cc30815a00f9ff548a896bbda0b4eb2ca19ebcf917f0f34200a9edbad3901b64ab09cc5ef7b9bcc3c40c0ff7509",
        );
        let sec = base16::unsafe_decode(
            "b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd",
        );
        assert_eq!(Ed25519::sign_bytes(&data, &sec).expect("ed25519 sign"), expected);
    }
}
