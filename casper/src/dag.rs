//! Block DAG key-value storage (port of `casper/dag/BlockDagKeyValueStorage.scala`).

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use async_trait::async_trait;

use rchain_block_storage::dag::dag_storage::{BlockDagStorage, DeployId};
use rchain_block_storage::dag::finalizer::Message;
use rchain_block_storage::dag::message_map;
use rchain_block_storage::dag::message_state::DagMessageState;
use rchain_block_storage::dag::representation::DagRepresentation;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{BlockMessage, SignedDeployData};
use rchain_models::fringe_data::FringeData;
use rchain_models::validator::Validator;
use rchain_shared::typed_store::KeyValueTypedStore;

use crate::block_metadata_store::BlockMetadataStore;
use crate::merging::BlockIndex;

/// Upper bound on the number of distinct deploys held in the pending pool. Reached by the
/// `add_deploy` chokepoint so a remote flood cannot exhaust the deploy store (documented Scala
/// deviation — the Scala pool is unbounded).
pub const MAX_POOLED_DEPLOYS: usize = 10_000;

/// Build a `Message` from a `BlockMetadata` given the current message map (port of
/// `BlockDagKeyValueStorage.messageFromBlockMetadata`). Returns `None` when a justification is
/// absent from `msg_map` (the Scala `Map.apply` throws).
pub fn message_from_block_metadata(
    block: &BlockMetadata,
    msg_map: &BTreeMap<BlockHash, Message<BlockHash, Validator>>,
) -> Option<Message<BlockHash, Validator>> {
    let mut seen: BTreeSet<BlockHash> = BTreeSet::new();
    for p in &block.justifications {
        seen.extend(msg_map.get(p)?.seen.iter().copied());
    }
    seen.insert(block.block_hash);
    Some(Message {
        id: block.block_hash,
        height: block.block_num,
        sender: block.sender,
        sender_seq: block.seq_num,
        bonds_map: block.bonds_map.clone(),
        parents: block.justifications.clone(),
        fringe: block.fringe.clone(),
        seen,
    })
}

/// The concrete block DAG storage (port of `BlockDagKeyValueStorage`). Fringe pruning (the
/// `BlockIndex` cache) and deploy-pool expiry run on finalization.
pub struct BlockDagKeyValueStorage {
    representation: tokio::sync::RwLock<DagRepresentation>,
    lock: tokio::sync::Mutex<()>,
    block_metadata_store: Arc<BlockMetadataStore>,
    fringe_data_store: Arc<dyn KeyValueTypedStore<Blake2b256Hash, FringeData>>,
    deploy_index: Arc<dyn KeyValueTypedStore<DeployId, BlockHash>>,
    deploy_store: Arc<dyn KeyValueTypedStore<DeployId, SignedDeployData>>,
}

impl BlockDagKeyValueStorage {
    /// Rebuild the in-memory DAG representation from the stores (port of `BlockDagKeyValueStorage.create`).
    pub async fn create(
        block_metadata_store: Arc<BlockMetadataStore>,
        fringe_data_store: Arc<dyn KeyValueTypedStore<Blake2b256Hash, FringeData>>,
        deploy_index: Arc<dyn KeyValueTypedStore<DeployId, BlockHash>>,
        deploy_store: Arc<dyn KeyValueTypedStore<DeployId, SignedDeployData>>,
    ) -> Result<Self, String> {
        let dag_set = block_metadata_store.dag_set().await;
        let child_map = block_metadata_store.child_map_data().await;
        let height_map = block_metadata_store.height_map().await;

        let mut dag_msg_state = DagMessageState::<BlockHash, Validator>::empty();
        let mut fringe_states: BTreeMap<BTreeSet<BlockHash>, FringeData> = BTreeMap::new();

        for hash in height_map.values().flatten() {
            if dag_msg_state.msg_map.contains_key(hash) {
                continue;
            }
            let block = block_metadata_store.get_unchecked(hash).await?;
            let msg = message_from_block_metadata(&block, &dag_msg_state.msg_map)
                .ok_or_else(|| "justification not present in message map".to_string())?;
            dag_msg_state = dag_msg_state.insert_msg(&msg);
            if !fringe_states.contains_key(&msg.fringe) {
                let fringe_hash = FringeData::fringe_hash_of(&msg.fringe);
                let fd = fringe_data_store
                    .get(&[fringe_hash])
                    .await?
                    .into_iter()
                    .next()
                    .flatten();
                if let Some(fd) = fd {
                    fringe_states.insert(msg.fringe.clone(), fd);
                }
            }
        }

        let representation = DagRepresentation {
            dag_set,
            child_map,
            height_map,
            dag_message_state: dag_msg_state,
            fringe_states,
        };

        Ok(BlockDagKeyValueStorage {
            representation: tokio::sync::RwLock::new(representation),
            lock: tokio::sync::Mutex::new(()),
            block_metadata_store,
            fringe_data_store,
            deploy_index,
            deploy_store,
        })
    }

    /// Expire deploys from the pool whose `valid_after_block_number` is older than the deploy
    /// lifespan (port of `removeExpiredFromPool`). Without this the pool grows without bound and
    /// stale deploys are re-proposed.
    async fn expire_deploys(&self, latest_block_number: i64) -> Result<(), String> {
        let pooled = self.deploy_store.to_map().await?;
        let expired: Vec<DeployId> = pooled
            .iter()
            .filter(|(_, d)| {
                latest_block_number - d.data.valid_after_block_number
                    > crate::multi_parent_casper::DEPLOY_LIFESPAN
            })
            .map(|(id, _)| id.clone())
            .collect();
        if !expired.is_empty() {
            self.deploy_store.delete(&expired).await?;
        }
        Ok(())
    }
}

#[async_trait]
impl BlockDagStorage for BlockDagKeyValueStorage {
    async fn get_representation(&self) -> DagRepresentation {
        self.representation.read().await.clone()
    }

    async fn insert(&self, block_metadata: BlockMetadata, block: BlockMessage) -> Result<(), String> {
        let _guard = self.lock.lock().await;
        if self.block_metadata_store.contains(&block_metadata.block_hash).await {
            return Ok(());
        }

        // H-1: equivocation detection — a second, distinct block by the same sender reusing a
        // `seq_num`. Reject it before any partial write so an equivocating validator can neither
        // enter the DAG nor stall finalization.
        {
            let repr = self.representation.read().await;
            let equivocating = repr.dag_message_state.msg_map.values().any(|m| {
                m.sender == block_metadata.sender && m.sender_seq == block_metadata.seq_num
            });
            if equivocating {
                return Err(
                    "equivocation detected: sender produced two blocks with the same sequence number"
                        .to_string(),
                );
            }
        }

        // Add block metadata to the index.
        self.block_metadata_store.add(block_metadata.clone()).await?;

        // Index each deploy to this block.
        let deploy_hashes: Vec<DeployId> = block
            .state
            .deploys
            .iter()
            .map(|d| d.deploy.sig.clone())
            .collect();
        if !deploy_hashes.is_empty() {
            let pairs: Vec<(DeployId, BlockHash)> = deploy_hashes
                .iter()
                .map(|h| (h.clone(), block.block_hash))
                .collect();
            self.deploy_index.put(&pairs).await?;
        }

        // Compute fringe diff and store fringe data.
        let dag_state = self.representation.read().await.dag_message_state.clone();
        let fringe_hash = FringeData::fringe_hash_of(&block_metadata.fringe);

        let mut justifications: BTreeSet<Message<BlockHash, Validator>> = BTreeSet::new();
        for j in &block_metadata.justifications {
            let msg = dag_state
                .msg_map
                .get(j)
                .ok_or_else(|| "justification not present in message map".to_string())?;
            justifications.insert(msg.clone());
        }
        let prev_fringe = message_map::latest_fringe(&dag_state.msg_map, &justifications);
        let mut fringe_seen: BTreeSet<BlockHash> = BTreeSet::new();
        for f in &block_metadata.fringe {
            let msg = dag_state
                .msg_map
                .get(f)
                .ok_or_else(|| "fringe block not present in message map".to_string())?;
            fringe_seen.extend(msg.seen.iter().copied());
        }
        let prev_seen: BTreeSet<BlockHash> =
            prev_fringe.iter().flat_map(|m| m.seen.iter().copied()).collect();
        let fringe_diff: BTreeSet<BlockHash> =
            fringe_seen.difference(&prev_seen).copied().collect();

        let fringe_data = FringeData {
            fringe_hash,
            fringe: block_metadata.fringe.clone(),
            fringe_diff: fringe_diff.clone(),
            state_hash: Blake2b256Hash::from_byte_array(block_metadata.fringe_state_hash.as_bytes()),
            rejected_deploys: block.rejected_deploys.clone(),
            rejected_blocks: block.rejected_blocks.clone(),
            rejected_senders: block.rejected_senders.clone(),
        };
        self.fringe_data_store
            .put(&[(fringe_hash, fringe_data.clone())])
            .await?;

        // Mark the newly-finalized blocks' metadata with their member fringe.
        for h in &fringe_diff {
            let meta = self.block_metadata_store.get_unchecked(h).await?;
            let updated = BlockMetadata {
                member_of_fringe: Some(fringe_hash),
                ..meta
            };
            self.block_metadata_store.add(updated).await?;
        }

        // Update the in-memory DAG representation.
        let dag_set = self.block_metadata_store.dag_set().await;
        let child_map = self.block_metadata_store.child_map_data().await;
        let height_map = self.block_metadata_store.height_map().await;
        let mut prune_cache_ids: Vec<BlockHash> = Vec::new();
        let latest_block_number: i64;
        {
            let mut repr = self.representation.write().await;
            let msg = message_from_block_metadata(&block_metadata, &repr.dag_message_state.msg_map)
                .ok_or_else(|| "justification not present in message map".to_string())?;
            // H-2: a validation-failed block is recorded in the map (for `neglectedInvalidBlock`
            // and justification-regression) but must not become the sender's latest message.
            repr.dag_message_state = if block_metadata.validation_failed {
                repr.dag_message_state.insert_msg_without_latest(&msg)
            } else {
                repr.dag_message_state.insert_msg(&msg)
            };
            repr.fringe_states.insert(msg.fringe.clone(), fringe_data);
            repr.dag_set = dag_set;
            repr.child_map = child_map;
            repr.height_map = height_map;

            // H-4: when finalization advanced, collect the block-index cache entries prunable
            // below the newly-finalized fringe.
            if !fringe_diff.is_empty() {
                let msg_map = &repr.dag_message_state.msg_map;
                let latest_msgs: BTreeSet<_> =
                    repr.dag_message_state.latest_msgs.values().cloned().collect();
                let lowest = message_map::lowest_fringe(msg_map, &latest_msgs);
                let lowest_ids: BTreeSet<BlockHash> = lowest.iter().map(|m| m.id).collect();
                let prunable = message_map::prune_fringe(msg_map, &lowest_ids, &repr.child_map);
                prune_cache_ids = prunable.iter().map(|m| m.id).collect();
            }
            latest_block_number = repr.latest_block_number();
        }

        BlockIndex::prune_cache(&prune_cache_ids);
        self.expire_deploys(latest_block_number).await?;
        Ok(())
    }

    async fn lookup(&self, block_hash: &BlockHash) -> Result<Option<BlockMetadata>, String> {
        self.block_metadata_store.get(block_hash).await
    }

    async fn lookup_by_deploy_id(&self, deploy_id: &DeployId) -> Result<Option<BlockHash>, String> {
        let vals = self.deploy_index.get(&[deploy_id.clone()]).await?;
        Ok(vals.into_iter().next().flatten())
    }

    async fn add_deploy(&self, deploy: SignedDeployData) -> Result<(), String> {
        // Soft DoS bound: reject before writing once the pool reaches `MAX_POOLED_DEPLOYS`
        // distinct deploys (documented Scala deviation — the Scala pool is unbounded).
        if self.deploy_store.count().await? >= MAX_POOLED_DEPLOYS {
            return Err("deploy pool is full".to_string());
        }
        self.deploy_store.put(&[(deploy.sig.clone(), deploy)]).await?;
        Ok(())
    }

    async fn pooled_deploys(&self) -> Result<BTreeMap<DeployId, SignedDeployData>, String> {
        self.deploy_store.to_map().await
    }

    async fn contains_deploy_in_pool(&self, deploy_id: &DeployId) -> Result<bool, String> {
        let vals = self.deploy_store.contains(&[deploy_id.clone()]).await?;
        Ok(vals.into_iter().next().unwrap_or(false))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_block_storage::dag::codecs::{
        Blake2b256HashCodec, BlockHashCodec, BlockMetadataCodec, FringeDataCodec,
        SignedDeployDataCodec,
    };
    use rchain_models::block::state_hash::StateHash;
    use rchain_shared::refined::BlockHeight;
    use rchain_shared::store::{InMemoryKeyValueStore, KeyValueStore};
    use rchain_shared::typed_store::{BytesCodec, KeyValueTypedStoreCodec};

    type Shared = Arc<tokio::sync::Mutex<Box<dyn KeyValueStore + Send + Sync>>>;

    fn in_memory() -> Shared {
        Arc::new(tokio::sync::Mutex::new(Box::new(
            InMemoryKeyValueStore::default(),
        )))
    }

    fn hash(byte: u8) -> BlockHash {
        let mut bytes = [0u8; 32];
        bytes[0] = byte;
        BlockHash::new(bytes)
    }

    fn meta(hash: BlockHash, parents: &[BlockHash], block_num: i64) -> BlockMetadata {
        BlockMetadata {
            block_hash: hash,
            block_num: BlockHeight::try_from(block_num).unwrap(),
            sender: Validator::new([0u8; 65]),
            seq_num: 0.try_into().unwrap(),
            justifications: parents.iter().copied().collect(),
            bonds_map: BTreeMap::new(),
            validated: true,
            validation_failed: false,
            fringe: BTreeSet::new(),
            fringe_state_hash: StateHash::new([0u8; 32]),
            member_of_fringe: None,
        }
    }

    fn block(hash: BlockHash) -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: hash,
            block_number: 0.try_into().unwrap(),
            sender: Validator::new([0u8; 65]),
            seq_num: 0.try_into().unwrap(),
            pre_state_hash: StateHash::new([0u8; 32]),
            post_state_hash: StateHash::new([0u8; 32]),
            justifications: vec![],
            bonds: BTreeMap::new(),
            rejected_deploys: BTreeSet::new(),
            rejected_blocks: BTreeSet::new(),
            rejected_senders: BTreeSet::new(),
            state: rchain_models::casper::protocol::casper_message::RholangState::default(),
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![],
        }
    }

    async fn build_storage() -> Arc<BlockDagKeyValueStorage> {
        let metadata_store = Arc::new(
            BlockMetadataStore::create(Arc::new(KeyValueTypedStoreCodec::new(
                in_memory(),
                Arc::new(BlockHashCodec),
                Arc::new(BlockMetadataCodec),
            )))
            .await
            .unwrap(),
        );
        let fringe_store: Arc<dyn KeyValueTypedStore<Blake2b256Hash, FringeData>> =
            Arc::new(KeyValueTypedStoreCodec::new(
                in_memory(),
                Arc::new(Blake2b256HashCodec),
                Arc::new(FringeDataCodec),
            ));
        let deploy_index: Arc<dyn KeyValueTypedStore<DeployId, BlockHash>> =
            Arc::new(KeyValueTypedStoreCodec::new(
                in_memory(),
                Arc::new(BytesCodec),
                Arc::new(BlockHashCodec),
            ));
        let deploy_store: Arc<dyn KeyValueTypedStore<DeployId, SignedDeployData>> =
            Arc::new(KeyValueTypedStoreCodec::new(
                in_memory(),
                Arc::new(BytesCodec),
                Arc::new(SignedDeployDataCodec),
            ));
        Arc::new(
            BlockDagKeyValueStorage::create(metadata_store, fringe_store, deploy_index, deploy_store)
                .await
                .unwrap(),
        )
    }

    #[tokio::test]
    async fn insert_and_lookup_round_trip() {
        let storage = build_storage().await;
        let genesis_hash = hash(0);
        storage
            .insert(meta(genesis_hash, &[], 0), block(genesis_hash))
            .await
            .unwrap();

        assert_eq!(
            storage.lookup(&genesis_hash).await.unwrap().unwrap().block_hash,
            genesis_hash
        );
        assert!(storage.lookup(&hash(9)).await.unwrap().is_none());
        assert!(storage.get_representation().await.contains(&genesis_hash));
    }

    #[tokio::test]
    async fn insert_rejects_equivocation_same_seq_num() {
        let storage = build_storage().await;
        let first = hash(1);
        let second = hash(2);

        storage
            .insert(meta(first, &[], 0), block(first))
            .await
            .unwrap();

        // A second, distinct block by the same sender reusing `seq_num` is rejected (H-1), before
        // any partial write.
        let err = storage.insert(meta(second, &[], 0), block(second)).await;
        assert!(err.is_err(), "equivocation must be rejected");
        assert!(
            err.unwrap_err().contains("equivocation"),
            "error should mention equivocation"
        );

        // The first block's metadata is still present and unchanged.
        let stored = storage.lookup(&first).await.unwrap().unwrap();
        assert_eq!(stored.block_hash, first);
    }

    #[tokio::test]
    async fn deploy_pool_round_trip() {
        let storage = build_storage().await;
        let deploy = SignedDeployData {
            data: rchain_models::casper::protocol::casper_message::DeployData {
                term: "Nil".to_string(),
                timestamp: 0,
                phlo_price: 1,
                phlo_limit: 1,
                valid_after_block_number: 0,
                shard_id: "root".to_string(),
            },
            deployer: vec![1, 2, 3],
            sig: vec![9, 9, 9],
            sig_algorithm: "secp256k1".to_string(),
        };
        storage.add_deploy(deploy.clone()).await.unwrap();
        assert!(storage.contains_deploy_in_pool(&deploy.sig).await.unwrap());
        assert_eq!(storage.lookup_by_deploy_id(&deploy.sig).await.unwrap(), None);

        let pooled = storage.pooled_deploys().await.unwrap();
        assert_eq!(pooled[&deploy.sig], deploy);
    }

    fn deploy_with_id(id: usize) -> SignedDeployData {
        SignedDeployData {
            data: rchain_models::casper::protocol::casper_message::DeployData {
                term: "Nil".to_string(),
                timestamp: 0,
                phlo_price: 1,
                phlo_limit: 1,
                valid_after_block_number: 0,
                shard_id: "root".to_string(),
            },
            deployer: vec![0],
            sig: id.to_le_bytes().to_vec(),
            sig_algorithm: "secp256k1".to_string(),
        }
    }

    #[tokio::test]
    async fn add_deploy_rejects_when_pool_full() {
        let storage = build_storage().await;
        for id in 0..MAX_POOLED_DEPLOYS {
            storage.add_deploy(deploy_with_id(id)).await.unwrap();
        }
        let err = storage.add_deploy(deploy_with_id(MAX_POOLED_DEPLOYS)).await;
        assert!(err.is_err(), "deploy pool must reject once full");
        assert!(err.unwrap_err().contains("full"));
    }
}
