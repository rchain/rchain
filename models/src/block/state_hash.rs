//! A state hash.
//!
//! Mirrors `models/src/main/scala/coop/rchain/models/block/StateHash.scala`.
//!
//! The 32-byte storage is the shared [`Hash32`](rchain_shared::refined::Hash32) newtype.

use rchain_shared::refined::Hash32;

use crate::errors::ModelsError;

/// The length of a `StateHash` in bytes.
pub const LENGTH: usize = 32;

/// A 32-byte state hash.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct StateHash(Hash32);

impl StateHash {
    pub fn new(bytes: [u8; LENGTH]) -> Self {
        Self(Hash32::new(bytes))
    }

    pub fn from_slice(bytes: &[u8]) -> Self {
        assert_eq!(bytes.len(), LENGTH, "expected {LENGTH} bytes");
        let mut arr = [0u8; LENGTH];
        arr.copy_from_slice(bytes);
        Self(Hash32::new(arr))
    }

    pub fn as_bytes(&self) -> &[u8; LENGTH] {
        self.0.as_bytes()
    }
}

impl TryFrom<&[u8]> for StateHash {
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

/// Total conversion from the canonical digest type (both are fixed 32-byte wrappers).
impl From<rchain_crypto::hash::blake2b256_hash::Blake2b256Hash> for StateHash {
    fn from(h: rchain_crypto::hash::blake2b256_hash::Blake2b256Hash) -> Self {
        Self(h.into())
    }
}

impl From<StateHash> for rchain_crypto::hash::blake2b256_hash::Blake2b256Hash {
    fn from(h: StateHash) -> Self {
        h.0.into()
    }
}

impl From<Hash32> for StateHash {
    fn from(h: Hash32) -> Self {
        StateHash(h)
    }
}

impl From<StateHash> for Hash32 {
    fn from(h: StateHash) -> Self {
        h.0
    }
}
