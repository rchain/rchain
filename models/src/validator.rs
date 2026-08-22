//! A validator identity.
//!
//! Mirrors `models/src/main/scala/coop/rchain/models/Validator.scala`.

use rchain_shared::base16;

use crate::errors::ModelsError;

/// The length of a `Validator` in bytes (an uncompressed secp256k1 public key).
pub const LENGTH: usize = 65;

/// A 65-byte validator identity.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Validator([u8; LENGTH]);

impl Validator {
    pub fn new(bytes: [u8; LENGTH]) -> Self {
        Self(bytes)
    }

    pub fn from_slice(bytes: &[u8]) -> Self {
        assert_eq!(bytes.len(), LENGTH, "expected {LENGTH} bytes");
        let mut arr = [0u8; LENGTH];
        arr.copy_from_slice(bytes);
        Self(arr)
    }

    pub fn as_bytes(&self) -> &[u8; LENGTH] {
        &self.0
    }
}

impl TryFrom<&[u8]> for Validator {
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

// `Validator` serializes as lowercase hex (the Scala `ByteString` → `buildStringNoLimit`).
impl serde::Serialize for Validator {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&base16::encode(self.as_bytes()))
    }
}

impl<'de> serde::Deserialize<'de> for Validator {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        let bytes =
            base16::decode(&s).ok_or_else(|| serde::de::Error::custom("invalid validator hex"))?;
        if bytes.len() != LENGTH {
            return Err(serde::de::Error::custom("invalid validator length"));
        }
        let mut arr = [0u8; LENGTH];
        arr.copy_from_slice(&bytes);
        Ok(Validator(arr))
    }
}
