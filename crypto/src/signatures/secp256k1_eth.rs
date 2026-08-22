//! Ethereum personal signatures over secp256k1.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/signatures/Secp256k1Eth.scala`. Identical to
//! `Secp256k1` except the signature format is raw 64-byte RS rather than DER.

use super::secp256k1::Secp256k1;
use super::signatures_alg::SignaturesAlg;
use crate::errors::CryptoError;
use crate::private_key::PrivateKey;
use crate::public_key::PublicKey;
use crate::util::certificate_helper;

/// The secp256k1 "eth" algorithm.
pub struct Secp256k1Eth;

impl SignaturesAlg for Secp256k1Eth {
    fn verify(&self, data: &[u8], signature_rs: &[u8], pub_key: &[u8]) -> bool {
        match certificate_helper::encode_signature_rs_to_der(signature_rs) {
            Ok(der) => Secp256k1::verify_bytes(data, &der, pub_key),
            // DER conversion error silently returns false (only for empty input, per the Scala).
            Err(_) => false,
        }
    }

    fn sign(&self, data: &[u8], sec: &[u8]) -> Result<Vec<u8>, CryptoError> {
        let der = Secp256k1::sign_bytes(data, sec)?;
        Ok(certificate_helper::decode_signature_der_to_rs(&der).unwrap_or_default())
    }

    fn to_public(&self, sec: &PrivateKey) -> Result<PublicKey, CryptoError> {
        Ok(PublicKey::new(Secp256k1::to_public_bytes(sec.bytes())?))
    }

    fn new_key_pair(&self) -> (PrivateKey, PublicKey) {
        Secp256k1.new_key_pair()
    }

    fn name(&self) -> &'static str {
        "secp256k1:eth"
    }

    fn sig_length(&self) -> usize {
        Secp256k1.sig_length()
    }
}
