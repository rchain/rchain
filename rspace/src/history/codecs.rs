//! Codecs for history store keys/values.
//!
//! Mirrors `rspace/.../hashing/Blake2b256Hash.codecBlake2b256Hash`.

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::typed_store::Codec;

/// A fixed 32-byte codec for `Blake2b256Hash`.
#[derive(Default)]
pub struct Blake2b256HashCodec;

impl Codec<Blake2b256Hash> for Blake2b256HashCodec {
    fn encode(&self, value: &Blake2b256Hash) -> Vec<u8> {
        value.to_byte_array().to_vec()
    }

    fn decode(&self, bytes: &[u8]) -> Result<Blake2b256Hash, String> {
        if bytes.len() != 32 {
            return Err(format!("expected 32 bytes, got {}", bytes.len()));
        }
        Ok(Blake2b256Hash::from_byte_array(bytes))
    }
}
