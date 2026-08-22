//! A 32-byte Blake2b256 hash wrapper.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/hashing/Blake2b256Hash.scala`, hoisted into
//! `crypto` so that `models` can depend on it without depending on `rspace` (per AGENTS.md).
//! The scodec codecs are deferred.
//!
//! The 32-byte storage is the shared [`Hash32`](rchain_shared::refined::Hash32) newtype; this type
//! adds the Blake2b256 digest constructors (`create`/`create_many`) on top.

use rchain_shared::base16;
use rchain_shared::refined::Hash32;

use crate::errors::CryptoError;

/// The length of a `Blake2b256Hash` in bytes.
pub const LENGTH: usize = 32;

/// Convert a digest `Vec<u8>` to a fixed array (provably total: Blake2b256 always emits `LENGTH`
/// bytes).
fn digest_to_array(digest: Vec<u8>) -> [u8; LENGTH] {
    let mut arr = [0u8; LENGTH];
    arr.copy_from_slice(&digest);
    arr
}

/// A 32-byte Blake2b256 hash.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Blake2b256Hash(Hash32);

impl Blake2b256Hash {
    /// Hash `bytes` and wrap the result.
    pub fn create(bytes: &[u8]) -> Self {
        Self(Hash32::new(digest_to_array(super::blake2b256::hash(bytes))))
    }

    /// Hash the concatenation of `parts` and wrap the result.
    pub fn create_many(parts: &[&[u8]]) -> Self {
        Self(Hash32::new(digest_to_array(super::blake2b256::hash_many(parts))))
    }

    /// Wrap an existing 32-byte array without hashing.
    pub fn from_bytes(bytes: [u8; LENGTH]) -> Self {
        Self(Hash32::new(bytes))
    }

    /// Wrap a byte slice, requiring it to be exactly 32 bytes.
    pub fn from_byte_array(bytes: &[u8]) -> Self {
        assert_eq!(
            bytes.len(),
            LENGTH,
            "Expected {} but got {}",
            LENGTH,
            bytes.len()
        );
        let mut arr = [0u8; LENGTH];
        arr.copy_from_slice(bytes);
        Self(Hash32::new(arr))
    }

    /// Parse a hex string, ignoring non-hex characters (the Scala `fromHex` / `unsafeDecode`).
    pub fn from_hex(string: &str) -> Self {
        Self::from_byte_array(&base16::unsafe_decode(string))
    }

    /// Parse a hex string, failing on invalid input or an incorrect length.
    pub fn from_hex_either(string: &str) -> Result<Self, CryptoError> {
        match base16::decode(string) {
            Some(bytes) if bytes.len() == LENGTH => Ok(Self::from_byte_array(&bytes)),
            _ => Err(CryptoError::InvalidHex),
        }
    }

    /// The underlying 32 bytes.
    pub fn as_bytes(&self) -> &[u8; LENGTH] {
        self.0.as_bytes()
    }

    /// The underlying 32 bytes as a slice.
    pub fn to_byte_array(&self) -> [u8; LENGTH] {
        self.0.to_byte_array()
    }

    /// Hex-encode the hash.
    pub fn to_hex(&self) -> String {
        self.0.to_hex()
    }
}

/// Checked counterpart of [`Blake2b256Hash::from_byte_array`] for validate-on-ingress (a wrong-length
/// wire/store key returns `Err` instead of panicking).
impl TryFrom<&[u8]> for Blake2b256Hash {
    type Error = CryptoError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if bytes.len() != LENGTH {
            return Err(CryptoError::InvalidLength {
                expected: LENGTH,
                actual: bytes.len(),
            });
        }
        Ok(Self::from_byte_array(bytes))
    }
}

/// Unwrap/re-wrap interop with the shared storage type.
impl From<Hash32> for Blake2b256Hash {
    fn from(h: Hash32) -> Self {
        Blake2b256Hash(h)
    }
}

impl From<Blake2b256Hash> for Hash32 {
    fn from(h: Blake2b256Hash) -> Self {
        h.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_hashes_input() {
        let h = Blake2b256Hash::create(b"abc");
        assert_eq!(
            h.to_hex(),
            "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
        );
    }

    #[test]
    fn create_many_matches_concatenation() {
        assert_eq!(
            Blake2b256Hash::create_many(&[b"ab", b"c"]),
            Blake2b256Hash::create(b"abc")
        );
    }

    #[test]
    fn from_hex_round_trips() {
        let h = Blake2b256Hash::create(b"abc");
        assert_eq!(Blake2b256Hash::from_hex(&h.to_hex()), h);
    }

    #[test]
    fn from_hex_either_rejects_bad_input() {
        assert!(Blake2b256Hash::from_hex_either("zz").is_err());
        assert!(Blake2b256Hash::from_hex_either("0e5751c026").is_err());
    }

    #[test]
    fn orders_lexicographically() {
        let a = Blake2b256Hash::from_bytes([0u8; 32]);
        let b = Blake2b256Hash::from_bytes([1u8; 32]);
        assert!(a < b);
    }
}
