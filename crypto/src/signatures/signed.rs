//! A signed value.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/signatures/Signed.scala`.

use crate::errors::CryptoError;
use crate::hash::{blake2b256, keccak256};
use crate::private_key::PrivateKey;
use crate::public_key::PublicKey;
use crate::signatures::signatures_alg::SignaturesAlg;
use rchain_shared::serialize::Serialize;

/// A value `A` together with its signature and the signer's public key.
pub struct Signed<A> {
    pub data: A,
    pub pk: PublicKey,
    pub sig: Vec<u8>,
    pub sig_algorithm: &'static dyn SignaturesAlg,
}

impl<A: Serialize<A>> Signed<A> {
    /// Sign `data` with `sig_algorithm` and `sk`.
    pub fn new(
        data: A,
        sig_algorithm: &'static dyn SignaturesAlg,
        sk: &PrivateKey,
    ) -> Result<Self, CryptoError> {
        let serialized = <A as Serialize<A>>::encode(&data);
        let hash = signature_hash(sig_algorithm.name(), &serialized);
        let sig = sig_algorithm.sign(&hash, sk.bytes())?;
        let pk = sig_algorithm.to_public(sk)?;
        Ok(Self {
            data,
            pk,
            sig,
            sig_algorithm,
        })
    }

    /// Reconstruct a `Signed` from its parts, verifying the signature. Returns `None` on failure.
    pub fn from_signed_data(
        data: A,
        pk: PublicKey,
        sig: Vec<u8>,
        sig_algorithm: &'static dyn SignaturesAlg,
    ) -> Option<Self> {
        let serialized = <A as Serialize<A>>::encode(&data);
        let hash = signature_hash(sig_algorithm.name(), &serialized);
        if sig_algorithm.verify(&hash, &sig, pk.bytes()) {
            Some(Self {
                data,
                pk,
                sig,
                sig_algorithm,
            })
        } else {
            None
        }
    }
}

/// The hash that a signature is computed over, per algorithm.
pub fn signature_hash(sig_alg_name: &str, serialized_data: &[u8]) -> Vec<u8> {
    if sig_alg_name == "secp256k1:eth" {
        let mut prefix = eth_prefix(serialized_data.len());
        prefix.extend_from_slice(serialized_data);
        keccak256::hash(&prefix)
    } else {
        blake2b256::hash(serialized_data)
    }
}

fn eth_prefix(msg_length: usize) -> Vec<u8> {
    format!("\u{19}Ethereum Signed Message:\n{msg_length}").into_bytes()
}
