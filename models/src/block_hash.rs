//! A block hash.
//!
//! Mirrors `models/src/main/scala/coop/rchain/models/BlockHash.scala`.
//!
//! The 32-byte storage is the shared [`Hash32`](rchain_shared::refined::Hash32) newtype.

use rchain_shared::base16;
use rchain_shared::refined::Hash32;

use crate::errors::ModelsError;

/// The length of a `BlockHash` in bytes.
pub const LENGTH: usize = 32;

/// A 32-byte block hash.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BlockHash(Hash32);

impl BlockHash {
    /// Wrap a 32-byte array.
    pub fn new(bytes: [u8; LENGTH]) -> Self {
        Self(Hash32::new(bytes))
    }

    /// Wrap a 32-byte slice (panics if not exactly [`LENGTH`] bytes).
    pub fn from_slice(bytes: &[u8]) -> Self {
        assert_eq!(bytes.len(), LENGTH, "expected {LENGTH} bytes");
        let mut arr = [0u8; LENGTH];
        arr.copy_from_slice(bytes);
        Self(Hash32::new(arr))
    }

    /// The underlying 32 bytes.
    pub fn as_bytes(&self) -> &[u8; LENGTH] {
        self.0.as_bytes()
    }

    /// Hex-encode the hash.
    pub fn to_hex(&self) -> String {
        self.0.to_hex()
    }

    /// Parse a full 32-byte hex string (panics if it decodes to a different length).
    pub fn from_hex(s: &str) -> Self {
        Self::from_slice(&base16::unsafe_decode(s))
    }

    /// Parse a full 32-byte hex string, rejecting non-hex or wrong-length input (validate-on-ingress
    /// counterpart of [`BlockHash::from_hex`]).
    pub fn try_from_hex(s: &str) -> Result<Self, ModelsError> {
        let bytes = base16::try_decode(s).map_err(ModelsError::Decode)?;
        Self::try_from(bytes.as_slice())
    }

    /// Whether the hash begins with `prefix` (used by `DagRepresentation.find`).
    pub fn starts_with(&self, prefix: &[u8]) -> bool {
        self.0.starts_with(prefix)
    }
}

impl TryFrom<&[u8]> for BlockHash {
    type Error = ModelsError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if bytes.len() != LENGTH {
            return Err(ModelsError::Length {
                got: bytes.len(),
                expected: LENGTH,
            });
        }
        Ok(Self::from_slice(bytes))
    }
}

impl From<Hash32> for BlockHash {
    fn from(h: Hash32) -> Self {
        BlockHash(h)
    }
}

impl From<BlockHash> for Hash32 {
    fn from(h: BlockHash) -> Self {
        h.0
    }
}

/// Total conversion from the canonical digest type (both are fixed 32-byte wrappers).
impl From<rchain_crypto::hash::blake2b256_hash::Blake2b256Hash> for BlockHash {
    fn from(h: rchain_crypto::hash::blake2b256_hash::Blake2b256Hash) -> Self {
        Self(h.into())
    }
}

impl From<BlockHash> for rchain_crypto::hash::blake2b256_hash::Blake2b256Hash {
    fn from(h: BlockHash) -> Self {
        h.0.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hex_round_trips() {
        let h = BlockHash::from_slice(&[0xab; 32]);
        assert_eq!(BlockHash::from_hex(&h.to_hex()), h);
    }

    #[test]
    fn length_is_32() {
        assert_eq!(LENGTH, 32);
    }

    #[test]
    fn orders_lexicographically() {
        let a = BlockHash::new([0u8; 32]);
        let b = BlockHash::new([1u8; 32]);
        assert!(a < b);
    }

    #[test]
    fn starts_with_prefix() {
        let mut bytes = [0u8; 32];
        bytes[..4].copy_from_slice(&[0xde, 0xad, 0xbe, 0xef]);
        let h = BlockHash::new(bytes);
        assert!(h.starts_with(&[0xde, 0xad]));
        assert!(!h.starts_with(&[0xde, 0xae]));
    }

    #[test]
    fn try_from_rejects_wrong_length() {
        assert!(BlockHash::try_from(&[0u8; 31][..]).is_err());
        assert!(BlockHash::try_from(&[0u8; 33][..]).is_err());
        assert!(BlockHash::try_from(&[0u8; 32][..]).is_ok());
    }

    #[test]
    fn try_from_hex_rejects_malformed() {
        assert!(BlockHash::try_from_hex("zz").is_err());
        assert!(BlockHash::try_from_hex("abcd").is_err()); // wrong length
        let h = BlockHash::new([0xab; 32]);
        assert_eq!(BlockHash::try_from_hex(&h.to_hex()).unwrap(), h);
    }
}
