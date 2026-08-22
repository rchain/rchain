//! The RSpace interface.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/ISpace.scala`.

use std::collections::BTreeMap;

use async_trait::async_trait;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::checkpoint::{Checkpoint, SoftCheckpoint};
use crate::errors::RSpaceError;
use crate::internal::{Datum, Row, WaitingContinuation};
use crate::tuple_space::Tuplespace;

/// The RSpace interface (port of `ISpace[F]`).
#[async_trait]
pub trait ISpace<C, P, A, K>: Tuplespace<C, P, A, K> {
    async fn create_checkpoint(&self) -> Result<Checkpoint, String>;

    async fn reset(&self, root: Blake2b256Hash) -> Result<(), String>;

    async fn get_data(&self, channel: &C) -> Result<Vec<Datum<A>>, RSpaceError>;

    async fn get_waiting_continuations(
        &self,
        channels: &[C],
    ) -> Result<Vec<WaitingContinuation<P, K>>, RSpaceError>;

    async fn get_joins(&self, channel: &C) -> Result<Vec<Vec<C>>, RSpaceError>;

    async fn clear(&self) -> Result<(), String>;

    async fn to_map(&self) -> BTreeMap<Vec<C>, Row<P, A, K>>;

    async fn create_soft_checkpoint(&self) -> SoftCheckpoint<C, P, A, K>;

    async fn revert_to_soft_checkpoint(&self, checkpoint: SoftCheckpoint<C, P, A, K>);
}
