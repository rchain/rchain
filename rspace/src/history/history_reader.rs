//! Read APIs over a history root.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/history/HistoryReader.scala`.

use std::sync::Arc;

use async_trait::async_trait;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::errors::RSpaceError;
use crate::internal::{Datum, WaitingContinuation};
use crate::serializers::scodec_serialize::{DatumB, JoinsB, WaitingContinuationB};

/// A reader for a particular history root (port of `HistoryReader`).
#[async_trait]
pub trait HistoryReader<C, P, A, K>: Send + Sync {
    fn root(&self) -> Blake2b256Hash;

    async fn get_data(&self, key: Blake2b256Hash) -> Result<Vec<Datum<A>>, RSpaceError>;

    async fn get_continuations(
        &self,
        key: Blake2b256Hash,
    ) -> Result<Vec<WaitingContinuation<P, K>>, RSpaceError>;

    async fn get_joins(&self, key: Blake2b256Hash) -> Result<Vec<Vec<C>>, RSpaceError>;

    /// Read a native system-contract leaf under `prefix` (registry / PoS / vault).
    async fn get_native(
        &self,
        prefix: u8,
        key: Blake2b256Hash,
    ) -> Result<Option<Vec<u8>>, RSpaceError>;

    /// A reader that hashes channels internally (port of `base`).
    fn base(&self) -> Arc<dyn HistoryReaderBase<C, P, A, K>>;

    /// A reader returning raw bytes alongside decoded values (port of `readerBinary`).
    fn reader_binary(&self) -> Arc<dyn HistoryReaderBinary<C, P, A, K>>;
}

/// A reader keyed by raw (unhashed) channels (port of `HistoryReaderBase`).
#[async_trait]
pub trait HistoryReaderBase<C, P, A, K>: Send + Sync {
    async fn get_data(&self, key: &C) -> Result<Vec<Datum<A>>, RSpaceError>;

    async fn get_continuations(
        &self,
        key: &[C],
    ) -> Result<Vec<WaitingContinuation<P, K>>, RSpaceError>;

    async fn get_joins(&self, key: &C) -> Result<Vec<Vec<C>>, RSpaceError>;
}

/// A reader returning raw bytes alongside decoded values (port of `HistoryReaderBinary`).
#[async_trait]
pub trait HistoryReaderBinary<C, P, A, K>: Send + Sync {
    async fn get_data(&self, key: Blake2b256Hash) -> Result<Vec<DatumB<A>>, RSpaceError>;

    async fn get_continuations(
        &self,
        key: Blake2b256Hash,
    ) -> Result<Vec<WaitingContinuationB<P, K>>, RSpaceError>;

    async fn get_joins(&self, key: Blake2b256Hash) -> Result<Vec<JoinsB<C>>, RSpaceError>;
}
