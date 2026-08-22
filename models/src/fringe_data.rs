//! Finalized-fringe data.
//!
//! Mirrors `models/src/main/scala/coop/rchain/models/FringeData.scala`. `Hash` is overridden to
//! hash only `fringe_hash` (mirrors the Scala), and `fringe_hash` is computed over the *sorted*
//! fringe (Law 18: fringe identity order-independent). Wire `from_proto`/`to_proto` are deferred
//! to the prost layer.

use std::collections::BTreeSet;
use std::hash::{Hash, Hasher};

use prost::Message as _;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::block_hash::BlockHash;
use crate::proto::casper::FringeDataProto;

/// Fringe data (fringe identity + rejected deploy/block/sender data).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FringeData {
    pub fringe_hash: Blake2b256Hash,
    pub fringe: BTreeSet<BlockHash>,
    pub fringe_diff: BTreeSet<BlockHash>,
    pub state_hash: Blake2b256Hash,
    pub rejected_deploys: BTreeSet<Vec<u8>>,
    pub rejected_blocks: BTreeSet<BlockHash>,
    pub rejected_senders: BTreeSet<Vec<u8>>,
}

// FringeData is uniquely identified by the hash of its fringe hashes (per the Scala).
impl Hash for FringeData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.fringe_hash.hash(state);
    }
}

impl FringeData {
    /// Hash of the (sorted) fringe — used as the fringe store primary key (Law 18).
    pub fn fringe_hash_of(fringe: &BTreeSet<BlockHash>) -> Blake2b256Hash {
        let parts: Vec<&[u8]> = fringe.iter().map(|h| h.as_bytes() as &[u8]).collect();
        Blake2b256Hash::create_many(&parts)
    }

    pub fn from_proto(b: &FringeDataProto) -> Result<Self, crate::errors::ModelsError> {
        Ok(FringeData {
            fringe_hash: Blake2b256Hash::try_from(b.fringe_hash.as_slice())?,
            fringe: b
                .fringe
                .iter()
                .map(|f| BlockHash::try_from(f.as_slice()))
                .collect::<Result<BTreeSet<BlockHash>, crate::errors::ModelsError>>()?,
            fringe_diff: b
                .fringe_diff
                .iter()
                .map(|f| BlockHash::try_from(f.as_slice()))
                .collect::<Result<BTreeSet<BlockHash>, crate::errors::ModelsError>>()?,
            state_hash: Blake2b256Hash::try_from(b.state_hash.as_slice())?,
            rejected_deploys: b.rejected_deploys.iter().cloned().collect(),
            rejected_blocks: b
                .rejected_blocks
                .iter()
                .map(|f| BlockHash::try_from(f.as_slice()))
                .collect::<Result<BTreeSet<BlockHash>, crate::errors::ModelsError>>()?,
            rejected_senders: b.rejected_senders.iter().cloned().collect(),
        })
    }

    pub fn to_proto(&self) -> FringeDataProto {
        FringeDataProto {
            fringe_hash: self.fringe_hash.as_bytes().to_vec(),
            fringe: self.fringe.iter().map(|f| f.as_bytes().to_vec()).collect(),
            fringe_diff: self.fringe_diff.iter().map(|f| f.as_bytes().to_vec()).collect(),
            state_hash: self.state_hash.as_bytes().to_vec(),
            rejected_deploys: self.rejected_deploys.iter().cloned().collect(),
            rejected_blocks: self.rejected_blocks.iter().map(|f| f.as_bytes().to_vec()).collect(),
            rejected_senders: self.rejected_senders.iter().cloned().collect(),
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.to_proto().encode_to_vec()
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, crate::errors::ModelsError> {
        let proto = FringeDataProto::decode(bytes).map_err(|e| crate::errors::ModelsError::Decode(e.to_string()))?;
        FringeData::from_proto(&proto)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hash(byte: u8) -> BlockHash {
        let mut bytes = [0u8; 32];
        bytes[0] = byte;
        BlockHash::new(bytes)
    }

    #[test]
    fn law18_fringe_hash_is_order_independent() {
        // `fringe_hash` is over the sorted fringe, so it is independent of input order.
        let h1 = hash(1);
        let h2 = hash(2);
        let h3 = hash(3);
        let fringe: BTreeSet<BlockHash> = [h3, h1, h2].into_iter().collect();
        let expected = {
            let parts: Vec<&[u8]> = [&h1, &h2, &h3].iter().map(|h| h.as_bytes() as &[u8]).collect();
            Blake2b256Hash::create_many(&parts)
        };
        assert_eq!(FringeData::fringe_hash_of(&fringe), expected);
    }

    #[test]
    fn fringe_data_round_trips() {
        let fd = FringeData {
            fringe_hash: Blake2b256Hash::from_bytes([1u8; 32]),
            fringe: [hash(2), hash(1)].into_iter().collect(),
            fringe_diff: [hash(3)].into_iter().collect(),
            state_hash: Blake2b256Hash::from_bytes([4u8; 32]),
            rejected_deploys: [vec![1u8]].into_iter().collect(),
            rejected_blocks: [hash(5)].into_iter().collect(),
            rejected_senders: [vec![2u8]].into_iter().collect(),
        };
        let decoded = FringeData::from_bytes(&fd.to_bytes()).unwrap();
        assert_eq!(decoded, fd);
    }
}
