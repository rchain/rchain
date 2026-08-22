//! The cold store: hash → leaf bytes.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/history/ColdStore.scala`.

use std::sync::Arc;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::typed_store::{Codec, KeyValueTypedStore};

use crate::serializers::scodec_serialize::{BitReader, BitWriter};

/// A persisted leaf (port of `PersistedData`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PersistedData {
    JoinsLeaf(Vec<u8>),
    DataLeaf(Vec<u8>),
    ContinuationsLeaf(Vec<u8>),
    /// A native system-contract leaf (registry / PoS / vault); the trie prefix disambiguates which.
    NativeLeaf(Vec<u8>),
}

/// The cold-store typed store type (port of `ColdKeyValueStore`).
pub type ColdKeyValueStore = Arc<dyn KeyValueTypedStore<Blake2b256Hash, PersistedData>>;

/// Encode a leaf: a 2-bit tag (`uint2`) + an `int64`-length-prefixed payload.
pub fn encode_persisted_data(value: &PersistedData) -> Vec<u8> {
    let (tag, bytes) = match value {
        PersistedData::JoinsLeaf(b) => (0u64, b),
        PersistedData::DataLeaf(b) => (1u64, b),
        PersistedData::ContinuationsLeaf(b) => (2u64, b),
        PersistedData::NativeLeaf(b) => (3u64, b),
    };
    let mut w = BitWriter::new();
    w.write_bits(tag, 2);
    w.write_bits(bytes.len() as u64, 64);
    for &b in bytes {
        w.write_bits(b as u64, 8);
    }
    w.finish()
}

/// Decode a leaf (inverse of [`encode_persisted_data`]).
pub fn decode_persisted_data(bytes: &[u8]) -> Result<PersistedData, String> {
    let mut r = BitReader::new(bytes);
    let tag = r.read_bits(2);
    let len = r.read_bits(64) as usize;
    let data = r.read_bytes_bits(len);
    match tag {
        0 => Ok(PersistedData::JoinsLeaf(data)),
        1 => Ok(PersistedData::DataLeaf(data)),
        2 => Ok(PersistedData::ContinuationsLeaf(data)),
        3 => Ok(PersistedData::NativeLeaf(data)),
        _ => Err(format!("unknown persisted-data tag {tag}")),
    }
}

/// A `Codec` for `PersistedData` (port of `codecPersistedData`).
#[derive(Default)]
pub struct PersistedDataCodec;

impl Codec<PersistedData> for PersistedDataCodec {
    fn encode(&self, value: &PersistedData) -> Vec<u8> {
        encode_persisted_data(value)
    }

    fn decode(&self, bytes: &[u8]) -> Result<PersistedData, String> {
        decode_persisted_data(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn persisted_data_round_trips() {
        for leaf in [
            PersistedData::JoinsLeaf(vec![1, 2, 3]),
            PersistedData::DataLeaf(vec![]),
            PersistedData::ContinuationsLeaf(vec![0xff; 40]),
            PersistedData::NativeLeaf(vec![0xab, 0xcd]),
        ] {
            let encoded = encode_persisted_data(&leaf);
            assert_eq!(decode_persisted_data(&encoded).unwrap(), leaf);
        }
    }
}
