//! Block metadata store — the in-memory DAG index over the persisted metadata store (port of
//! `block-storage/dag/BlockMetadataStore.scala`).

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use rchain_block_storage::dag::metadata_store::{
    add_block_to_dag_state, block_metadata_to_info, recreate_in_memory_state, validate_dag_state,
    BlockInfo, DagState,
};
use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_shared::refined::BlockHeight;
use rchain_shared::typed_store::KeyValueTypedStore;

/// The block metadata store: a persisted `KeyValueTypedStore` plus an in-memory [`DagState`] index
/// rebuilt on startup.
pub struct BlockMetadataStore {
    store: Arc<dyn KeyValueTypedStore<BlockHash, BlockMetadata>>,
    dag_state: tokio::sync::RwLock<DagState>,
}

impl BlockMetadataStore {
    /// Rebuild the in-memory DAG index from the persisted store (port of `BlockMetadataStore.apply`).
    pub async fn create(
        store: Arc<dyn KeyValueTypedStore<BlockHash, BlockMetadata>>,
    ) -> Result<Self, String> {
        let blocks = store.to_map().await?;
        let info_map: BTreeMap<BlockHash, BlockInfo> = blocks
            .iter()
            .map(|(hash, meta)| (*hash, block_metadata_to_info(meta)))
            .collect();
        let dag_state = recreate_in_memory_state(&info_map);
        Ok(BlockMetadataStore {
            store,
            dag_state: tokio::sync::RwLock::new(dag_state),
        })
    }

    /// Insert a block's metadata into both the in-memory index and the persisted store.
    pub async fn add(&self, block: BlockMetadata) -> Result<(), String> {
        let info = block_metadata_to_info(&block);
        {
            let mut state = self.dag_state.write().await;
            *state = add_block_to_dag_state(&info, &state);
            validate_dag_state(&state);
        }
        self.store.put(&[(block.block_hash, block)]).await?;
        Ok(())
    }

    pub async fn get(&self, hash: &BlockHash) -> Result<Option<BlockMetadata>, String> {
        let vals = self.store.get(&[*hash]).await?;
        Ok(vals.into_iter().next().flatten())
    }

    /// Look up a block's metadata, failing if it is absent (port of `getUnsafe`).
    pub async fn get_unchecked(&self, hash: &BlockHash) -> Result<BlockMetadata, String> {
        self.get(hash)
            .await?
            .ok_or_else(|| format!("BlockMetadataStore is missing key {}", hash.to_hex()))
    }

    pub async fn contains(&self, hash: &BlockHash) -> bool {
        self.dag_state.read().await.dag_set.contains(hash)
    }

    pub async fn dag_set(&self) -> BTreeSet<BlockHash> {
        self.dag_state.read().await.dag_set.clone()
    }

    pub async fn child_map_data(&self) -> BTreeMap<BlockHash, BTreeSet<BlockHash>> {
        self.dag_state.read().await.child_map.clone()
    }

    pub async fn height_map(&self) -> BTreeMap<BlockHeight, BTreeSet<BlockHash>> {
        self.dag_state.read().await.height_map.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_block_storage::dag::codecs::{BlockHashCodec, BlockMetadataCodec};
    use rchain_shared::store::{InMemoryKeyValueStore, KeyValueStore};
    use rchain_shared::typed_store::KeyValueTypedStoreCodec;

    type Shared = Arc<tokio::sync::Mutex<Box<dyn KeyValueStore + Send + Sync>>>;

    fn metadata_store() -> Arc<dyn KeyValueTypedStore<BlockHash, BlockMetadata>> {
        let shared: Shared = Arc::new(tokio::sync::Mutex::new(Box::new(
            InMemoryKeyValueStore::default(),
        )));
        Arc::new(KeyValueTypedStoreCodec::new(
            shared,
            Arc::new(BlockHashCodec),
            Arc::new(BlockMetadataCodec),
        ))
    }

    fn hash(byte: u8) -> BlockHash {
        let mut bytes = [0u8; 32];
        bytes[0] = byte;
        BlockHash::new(bytes)
    }

    fn meta(hash: BlockHash, parents: &[BlockHash], block_num: i64) -> BlockMetadata {
        BlockMetadata {
            block_hash: hash,
            block_num: rchain_shared::refined::BlockHeight::try_from(block_num).unwrap(),
            sender: rchain_models::validator::Validator::new([0u8; 65]),
            seq_num: 0.try_into().unwrap(),
            justifications: parents.iter().copied().collect(),
            bonds_map: BTreeMap::new(),
            validated: true,
            validation_failed: false,
            fringe: BTreeSet::new(),
            fringe_state_hash: rchain_models::block::state_hash::StateHash::new([0u8; 32]),
            member_of_fringe: None,
        }
    }

    #[tokio::test]
    async fn add_and_lookup_round_trip() {
        let store = BlockMetadataStore::create(metadata_store()).await.unwrap();
        let genesis = meta(hash(0), &[], 0);
        store.add(genesis.clone()).await.unwrap();

        assert!(store.contains(&hash(0)).await);
        assert_eq!(store.get(&hash(0)).await.unwrap(), Some(genesis.clone()));
        assert_eq!(store.get(&hash(1)).await.unwrap(), None);

        // get_unchecked panics (errors) on a missing key.
        assert!(store.get_unchecked(&hash(9)).await.is_err());
    }

    #[tokio::test]
    async fn dag_state_tracks_child_and_height_maps() {
        let store = BlockMetadataStore::create(metadata_store()).await.unwrap();
        let genesis = meta(hash(0), &[], 0);
        let child = meta(hash(1), &[hash(0)], 1);
        store.add(genesis).await.unwrap();
        store.add(child).await.unwrap();

        let child_map = store.child_map_data().await;
        assert_eq!(child_map[&hash(0)], [hash(1)].into_iter().collect());
        assert!(child_map[&hash(1)].is_empty());

        let height_map = store.height_map().await;
        assert_eq!(
            height_map[&rchain_shared::refined::BlockHeight::try_from(0).unwrap()],
            [hash(0)].into_iter().collect()
        );
        assert_eq!(
            height_map[&rchain_shared::refined::BlockHeight::try_from(1).unwrap()],
            [hash(1)].into_iter().collect()
        );
    }
}
