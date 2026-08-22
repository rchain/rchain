//! Block metadata.
//!
//! Mirrors `models/src/main/scala/coop/rchain/models/BlockMetadata.scala`. `Hash` is overridden to
//! hash only `block_hash` (mirrors the Scala). Wire `from_proto`/`to_proto` are deferred to the
//! prost layer.

use std::collections::{BTreeMap, BTreeSet};
use std::hash::{Hash, Hasher};

use prost::Message as _;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::refined::{BlockHeight, NonNegI64, SeqNum};

use crate::block::state_hash::StateHash;
use crate::block_hash::BlockHash;
use crate::casper::protocol::casper_message::BlockMessage;
use crate::proto::casper::{BlockMetadataProto, BondProto};
use crate::validator::Validator;

/// A block's metadata (the block-storage DAG index entry).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockMetadata {
    pub block_hash: BlockHash,
    pub block_num: BlockHeight,
    pub sender: Validator,
    pub seq_num: SeqNum,
    pub justifications: BTreeSet<BlockHash>,
    pub bonds_map: BTreeMap<Validator, NonNegI64>,
    pub validated: bool,
    pub validation_failed: bool,
    pub fringe: BTreeSet<BlockHash>,
    pub fringe_state_hash: StateHash,
    pub member_of_fringe: Option<Blake2b256Hash>,
}

// BlockMetadata is uniquely identified by its block hash (per the Scala).
impl Hash for BlockMetadata {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.block_hash.hash(state);
    }
}

impl BlockMetadata {
    pub fn from_proto(b: &BlockMetadataProto) -> Result<Self, crate::errors::ModelsError> {
        Ok(BlockMetadata {
            block_hash: BlockHash::try_from(b.block_hash.as_slice())?,
            block_num: BlockHeight::try_from(b.block_num)
                .map_err(|_| crate::errors::ModelsError::Malformed("negative block number"))?,
            sender: Validator::try_from(b.sender.as_slice())?,
            seq_num: SeqNum::try_from(b.seq_num)
                .map_err(|_| crate::errors::ModelsError::Malformed("negative sequence number"))?,
            justifications: b
                .justifications
                .iter()
                .map(|j| BlockHash::try_from(j.as_slice()))
                .collect::<Result<BTreeSet<BlockHash>, crate::errors::ModelsError>>()?,
            bonds_map: b
                .bonds
                .iter()
                .map(|bond| {
                    let stake = NonNegI64::try_from(bond.stake)
                        .map_err(|_| crate::errors::ModelsError::Malformed("negative bond stake"))?;
                    Ok((Validator::try_from(bond.validator.as_slice())?, stake))
                })
                .collect::<Result<_, crate::errors::ModelsError>>()?,
            validated: b.validated,
            validation_failed: b.validation_failed,
            fringe: b
                .fringe
                .iter()
                .map(|f| BlockHash::try_from(f.as_slice()))
                .collect::<Result<BTreeSet<BlockHash>, crate::errors::ModelsError>>()?,
            // The Scala `StateHash` is a `ByteString`, so `fromBlock` can produce an empty
            // `fringeStateHash`; the Rust fixed-width `StateHash` maps that to zero-fill (the empty
            // value is a transient pre-validation placeholder, never a real hash).
            fringe_state_hash: if b.fringe_state_hash.is_empty() {
                StateHash::new([0u8; 32])
            } else {
                StateHash::try_from(b.fringe_state_hash.as_slice())?
            },
            member_of_fringe: if b.member_of_fringe.is_empty() {
                None
            } else {
                Some(Blake2b256Hash::try_from(b.member_of_fringe.as_slice())?)
            },
        })
    }

    pub fn to_proto(&self) -> BlockMetadataProto {
        BlockMetadataProto {
            block_hash: self.block_hash.as_bytes().to_vec(),
            block_num: i64::from(self.block_num),
            sender: self.sender.as_bytes().to_vec(),
            seq_num: i64::from(self.seq_num),
            justifications: self
                .justifications
                .iter()
                .map(|j| j.as_bytes().to_vec())
                .collect(),
            bonds: self
                .bonds_map
                .iter()
                .map(|(validator, stake)| BondProto {
                    validator: validator.as_bytes().to_vec(),
                    stake: i64::from(*stake),
                })
                .collect(),
            validated: self.validated,
            validation_failed: self.validation_failed,
            fringe: self.fringe.iter().map(|f| f.as_bytes().to_vec()).collect(),
            fringe_state_hash: self.fringe_state_hash.as_bytes().to_vec(),
            member_of_fringe: self
                .member_of_fringe
                .as_ref()
                .map(|h| h.as_bytes().to_vec())
                .unwrap_or_default(),
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.to_proto().encode_to_vec()
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, crate::errors::ModelsError> {
        let proto = BlockMetadataProto::decode(bytes).map_err(|e| crate::errors::ModelsError::Decode(e.to_string()))?;
        BlockMetadata::from_proto(&proto)
    }

    /// Build metadata from a block message (port of `BlockMetadata.fromBlock`).
    pub fn from_block(b: &BlockMessage) -> Self {
        BlockMetadata {
            block_hash: b.block_hash,
            block_num: b.block_number,
            sender: b.sender,
            seq_num: b.seq_num,
            justifications: b.justifications.iter().copied().collect(),
            bonds_map: b.bonds.clone(),
            validated: false,
            validation_failed: false,
            fringe: BTreeSet::new(),
            fringe_state_hash: StateHash::new([0u8; 32]),
            member_of_fringe: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block_hash(byte: u8) -> BlockHash {
        BlockHash::new([byte; 32])
    }
    fn validator(byte: u8) -> Validator {
        Validator::new([byte; 65])
    }

    #[test]
    fn law18_block_metadata_round_trips() {
        let meta = BlockMetadata {
            block_hash: block_hash(1),
            block_num: 3.try_into().unwrap(),
            sender: validator(2),
            seq_num: 1.try_into().unwrap(),
            justifications: [block_hash(2), block_hash(1)].into_iter().collect(),
            bonds_map: BTreeMap::from([(validator(2), 100.try_into().unwrap())]),
            validated: true,
            validation_failed: false,
            fringe: [block_hash(5)].into_iter().collect(),
            fringe_state_hash: StateHash::new([9u8; 32]),
            member_of_fringe: Some(Blake2b256Hash::from_bytes([8u8; 32])),
        };
        let decoded = BlockMetadata::from_bytes(&meta.to_bytes()).unwrap();
        assert_eq!(decoded, meta);
    }

    #[test]
    fn from_block_extracts_identity_fields() {
        let block = crate::casper::protocol::casper_message::BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: block_hash(1),
            block_number: 4.try_into().unwrap(),
            sender: validator(2),
            seq_num: 3.try_into().unwrap(),
            pre_state_hash: StateHash::new([0u8; 32]),
            post_state_hash: StateHash::new([0u8; 32]),
            justifications: vec![block_hash(2)],
            bonds: BTreeMap::from([(validator(2), 100.try_into().unwrap())]),
            rejected_deploys: Default::default(),
            rejected_blocks: Default::default(),
            rejected_senders: Default::default(),
            state: Default::default(),
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![],
        };
        let meta = BlockMetadata::from_block(&block);
        assert_eq!(meta.block_hash, block_hash(1));
        assert_eq!(meta.block_num, 4.try_into().unwrap());
        assert_eq!(meta.sender, validator(2));
        assert_eq!(meta.seq_num, 3.try_into().unwrap());
        assert_eq!(meta.justifications, [block_hash(2)].into_iter().collect());
        assert_eq!(meta.bonds_map, BTreeMap::from([(validator(2), 100.try_into().unwrap())]));
        assert!(!meta.validated);
    }
}
