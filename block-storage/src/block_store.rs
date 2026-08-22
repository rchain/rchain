//! Block store.
//!
//! Mirrors `block-storage/src/main/scala/coop/rchain/blockstorage/BlockStore.scala`. `BlockStore`
//! is a typed store `BlockHash → BlockMessage` whose values are LZ4-with-length-compressed protobuf
//! (the Scala `LZ4CompressorWithLength` / `LZ4DecompressorWithLength`).

use std::sync::Arc;

use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::BlockMessage;
use rchain_shared::store_manager::KeyValueStoreManager;
use rchain_shared::typed_store::{KeyValueTypedStore, KeyValueTypedStoreCodec};

use crate::dag::codecs::{BlockHashCodec, BlockMessageCodec};
use crate::errors::StorageError;

/// A typed store from block hash to block message (port of `BlockStore[F]`).
pub type BlockStore = Arc<dyn KeyValueTypedStore<BlockHash, BlockMessage>>;

/// Length-prefixed LZ4 compression (the Scala `LZ4CompressorWithLength`).
pub fn compress_bytes(bytes: &[u8]) -> Vec<u8> {
    lz4_flex::compress_prepend_size(bytes)
}

/// Length-prefixed LZ4 decompression (the Scala `LZ4DecompressorWithLength`).
pub fn decompress_bytes(bytes: &[u8]) -> Result<Vec<u8>, StorageError> {
    lz4_flex::decompress_size_prepended(bytes).map_err(|_| StorageError::DecompressionError)
}

/// Serialize a block message to its stored byte form (LZ4 over protobuf).
pub fn block_message_to_bytes(block: &BlockMessage) -> Vec<u8> {
    compress_bytes(&block.to_bytes())
}

/// Deserialize a block message from its stored byte form.
pub fn bytes_to_block_message(bytes: &[u8]) -> Result<BlockMessage, String> {
    let decompressed = decompress_bytes(bytes).map_err(|e| e.to_string())?;
    BlockMessage::from_bytes(&decompressed).map_err(|e| e.to_string())
}

/// Open the block store from a store manager (port of `BlockStore.apply[F](kvm)`).
pub async fn create(kvm: &dyn KeyValueStoreManager) -> Result<BlockStore, String> {
    let store = kvm.store("blocks").await?;
    Ok(Arc::new(KeyValueTypedStoreCodec::new(
        store,
        Arc::new(BlockHashCodec),
        Arc::new(BlockMessageCodec),
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{BTreeMap, BTreeSet};

    use rchain_models::block_hash::BlockHash;
    use rchain_models::casper::protocol::casper_message::RholangState;
    use rchain_models::validator::Validator;
    use rchain_shared::store_manager::InMemoryStoreManager;

    fn block() -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: BlockHash::new([1u8; 32]),
            block_number: 0.try_into().unwrap(),
            sender: Validator::new([2u8; 65]),
            seq_num: 0.try_into().unwrap(),
            pre_state_hash: rchain_models::block::state_hash::StateHash::new([0u8; 32]),
            post_state_hash: rchain_models::block::state_hash::StateHash::new([0u8; 32]),
            justifications: vec![],
            bonds: BTreeMap::new(),
            rejected_deploys: BTreeSet::new(),
            rejected_blocks: BTreeSet::new(),
            rejected_senders: BTreeSet::new(),
            state: RholangState::default(),
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![],
        }
    }

    #[test]
    fn lz4_block_message_round_trips() {
        let b = block();
        let bytes = block_message_to_bytes(&b);
        assert_eq!(bytes_to_block_message(&bytes).unwrap(), b);
    }

    #[tokio::test]
    async fn block_store_round_trips() {
        let kvm = InMemoryStoreManager::default();
        let store = create(&kvm).await.unwrap();
        let b = block();
        store.put(&[(b.block_hash, b.clone())]).await.unwrap();
        assert_eq!(store.get(&[b.block_hash]).await.unwrap(), vec![Some(b)]);
    }
}
