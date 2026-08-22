//! The concrete `HistoryReader` over a target history + cold store.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/history/instances/RSpaceHistoryReaderImpl.scala`.

use std::sync::Arc;

use async_trait::async_trait;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::serialize::Serialize;

use crate::errors::RSpaceError;
use crate::hashing::stable_hash_provider::{hash_channel, hash_channels};
use crate::history::cold_store::{ColdKeyValueStore, PersistedData};
use crate::history::history::History;
use crate::history::history_reader::{HistoryReader, HistoryReaderBase, HistoryReaderBinary};
use crate::history::key_segment::KeySegment;
use crate::internal::{Datum, WaitingContinuation};
use crate::native_store::NativeHistoryReader;
use crate::serializers::scodec_serialize::{
    decode_continuations, decode_continuations_binary, decode_datums, decode_datums_binary,
    decode_joins, decode_joins_binary,
};

const PREFIX_DATUM: u8 = 0x00;
const PREFIX_KONT: u8 = 0x01;
const PREFIX_JOINS: u8 = 0x02;

/// The history reader implementation (port of `RSpaceHistoryReaderImpl`).
pub struct RSpaceHistoryReaderImpl<C, P, A, K> {
    target_history: Arc<dyn History>,
    leaf_store: ColdKeyValueStore,
    marker: std::marker::PhantomData<(C, P, A, K)>,
}

impl<C, P, A, K> RSpaceHistoryReaderImpl<C, P, A, K>
where
    C: Serialize<C> + Send + Sync + 'static,
    P: Serialize<P> + Send + Sync + 'static,
    A: Serialize<A> + Send + Sync + 'static,
    K: Serialize<K> + Send + Sync + 'static,
{
    pub fn new(target_history: Arc<dyn History>, leaf_store: ColdKeyValueStore) -> Self {
        RSpaceHistoryReaderImpl {
            target_history,
            leaf_store,
            marker: std::marker::PhantomData,
        }
    }

    async fn fetch_data(&self, prefix: u8, key: Blake2b256Hash) -> Result<Option<PersistedData>, String> {
        let mut seg = vec![prefix];
        seg.extend_from_slice(key.as_bytes());
        match self.target_history.read(&KeySegment::new(seg)).await {
            Some(leaf_hash) => {
                let data = self.leaf_store.get(&[leaf_hash]).await?;
                Ok(data.into_iter().next().flatten())
            }
            None => Ok(None),
        }
    }
}

#[async_trait]
impl<C, P, A, K> HistoryReader<C, P, A, K> for RSpaceHistoryReaderImpl<C, P, A, K>
where
    C: Serialize<C> + Send + Sync + 'static,
    P: Serialize<P> + Send + Sync + 'static,
    A: Serialize<A> + Send + Sync + 'static,
    K: Serialize<K> + Send + Sync + 'static,
{
    fn root(&self) -> Blake2b256Hash {
        self.target_history.root()
    }

    async fn get_data(&self, key: Blake2b256Hash) -> Result<Vec<Datum<A>>, RSpaceError> {
        match self.fetch_data(PREFIX_DATUM, key).await.map_err(|_| RSpaceError::Codec("persisted data"))? {
            Some(PersistedData::DataLeaf(bytes)) => decode_datums(&bytes),
            Some(_) => Err(RSpaceError::UnexpectedLeaf("data")),
            None => Ok(Vec::new()),
        }
    }

    async fn get_continuations(
        &self,
        key: Blake2b256Hash,
    ) -> Result<Vec<WaitingContinuation<P, K>>, RSpaceError> {
        match self.fetch_data(PREFIX_KONT, key).await.map_err(|_| RSpaceError::Codec("persisted data"))? {
            Some(PersistedData::ContinuationsLeaf(bytes)) => decode_continuations(&bytes),
            Some(_) => Err(RSpaceError::UnexpectedLeaf("continuations")),
            None => Ok(Vec::new()),
        }
    }

    async fn get_joins(&self, key: Blake2b256Hash) -> Result<Vec<Vec<C>>, RSpaceError> {
        match self.fetch_data(PREFIX_JOINS, key).await.map_err(|_| RSpaceError::Codec("persisted data"))? {
            Some(PersistedData::JoinsLeaf(bytes)) => decode_joins(&bytes),
            Some(_) => Err(RSpaceError::UnexpectedLeaf("joins")),
            None => Ok(Vec::new()),
        }
    }

    async fn get_native(
        &self,
        prefix: u8,
        key: Blake2b256Hash,
    ) -> Result<Option<Vec<u8>>, RSpaceError> {
        match self.fetch_data(prefix, key).await.map_err(|_| RSpaceError::Codec("native"))? {
            Some(PersistedData::NativeLeaf(bytes)) => Ok(Some(bytes)),
            Some(_) => Err(RSpaceError::UnexpectedLeaf("native")),
            None => Ok(None),
        }
    }

    fn base(&self) -> Arc<dyn HistoryReaderBase<C, P, A, K>> {
        Arc::new(BaseReader {
            reader: Arc::new(RSpaceHistoryReaderImpl {
                target_history: self.target_history.clone(),
                leaf_store: self.leaf_store.clone(),
                marker: std::marker::PhantomData,
            }),
        })
    }

    fn reader_binary(&self) -> Arc<dyn HistoryReaderBinary<C, P, A, K>> {
        Arc::new(RSpaceHistoryReaderImpl {
            target_history: self.target_history.clone(),
            leaf_store: self.leaf_store.clone(),
            marker: std::marker::PhantomData,
        })
    }
}

#[async_trait]
impl<C, P, A, K> NativeHistoryReader for RSpaceHistoryReaderImpl<C, P, A, K>
where
    C: Serialize<C> + Send + Sync + 'static,
    P: Serialize<P> + Send + Sync + 'static,
    A: Serialize<A> + Send + Sync + 'static,
    K: Serialize<K> + Send + Sync + 'static,
{
    async fn get_native(&self, prefix: u8, key: Blake2b256Hash) -> Result<Option<Vec<u8>>, String> {
        match self.fetch_data(prefix, key).await? {
            Some(PersistedData::NativeLeaf(bytes)) => Ok(Some(bytes)),
            Some(_) => Err("unexpected leaf while looking for native".to_string()),
            None => Ok(None),
        }
    }
}

#[async_trait]
impl<C, P, A, K> HistoryReaderBinary<C, P, A, K> for RSpaceHistoryReaderImpl<C, P, A, K>
where
    C: Serialize<C> + Send + Sync + 'static,
    P: Serialize<P> + Send + Sync + 'static,
    A: Serialize<A> + Send + Sync + 'static,
    K: Serialize<K> + Send + Sync + 'static,
{
    async fn get_data(
        &self,
        key: Blake2b256Hash,
    ) -> Result<Vec<crate::serializers::scodec_serialize::DatumB<A>>, RSpaceError> {
        match self
            .fetch_data(PREFIX_DATUM, key)
            .await
            .map_err(|_| RSpaceError::Codec("persisted data"))?
        {
            Some(PersistedData::DataLeaf(bytes)) => decode_datums_binary(&bytes),
            Some(_) => Err(RSpaceError::UnexpectedLeaf("data")),
            None => Ok(Vec::new()),
        }
    }

    async fn get_continuations(
        &self,
        key: Blake2b256Hash,
    ) -> Result<Vec<crate::serializers::scodec_serialize::WaitingContinuationB<P, K>>, RSpaceError>
    {
        match self
            .fetch_data(PREFIX_KONT, key)
            .await
            .map_err(|_| RSpaceError::Codec("persisted data"))?
        {
            Some(PersistedData::ContinuationsLeaf(bytes)) => decode_continuations_binary(&bytes),
            Some(_) => Err(RSpaceError::UnexpectedLeaf("continuations")),
            None => Ok(Vec::new()),
        }
    }

    async fn get_joins(
        &self,
        key: Blake2b256Hash,
    ) -> Result<Vec<crate::serializers::scodec_serialize::JoinsB<C>>, RSpaceError> {
        match self
            .fetch_data(PREFIX_JOINS, key)
            .await
            .map_err(|_| RSpaceError::Codec("persisted data"))?
        {
            Some(PersistedData::JoinsLeaf(bytes)) => decode_joins_binary(&bytes),
            Some(_) => Err(RSpaceError::UnexpectedLeaf("joins")),
            None => Ok(Vec::new()),
        }
    }
}

struct BaseReader<C, P, A, K> {
    reader: Arc<RSpaceHistoryReaderImpl<C, P, A, K>>,
}

#[async_trait]
impl<C, P, A, K> HistoryReaderBase<C, P, A, K> for BaseReader<C, P, A, K>
where
    C: Serialize<C> + Send + Sync + 'static,
    P: Serialize<P> + Send + Sync + 'static,
    A: Serialize<A> + Send + Sync + 'static,
    K: Serialize<K> + Send + Sync + 'static,
{
    async fn get_data(&self, key: &C) -> Result<Vec<Datum<A>>, RSpaceError> {
        HistoryReader::get_data(self.reader.as_ref(), hash_channel(key)).await
    }

    async fn get_continuations(&self, key: &[C]) -> Result<Vec<WaitingContinuation<P, K>>, RSpaceError> {
        HistoryReader::get_continuations(self.reader.as_ref(), hash_channels(key)).await
    }

    async fn get_joins(&self, key: &C) -> Result<Vec<Vec<C>>, RSpaceError> {
        HistoryReader::get_joins(self.reader.as_ref(), hash_channel(key)).await
    }
}
