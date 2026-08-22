//! The tuple-space API + result types.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/Tuplespace.scala` and the `Result`/`ContResult`
//! types from `ISpace.scala`.

use std::collections::BTreeSet;

use async_trait::async_trait;

use crate::errors::RSpaceError;

/// A matched datum result (port of `Result`).
#[derive(Clone, Debug, PartialEq)]
pub struct Result<C, A> {
    pub channel: C,
    pub matched_datum: A,
    pub removed_datum: A,
    pub persistent: bool,
}

/// A matched continuation result (port of `ContResult`).
#[derive(Clone, Debug, PartialEq)]
pub struct ContResult<C, P, K> {
    pub continuation: K,
    pub persistent: bool,
    pub channels: Vec<C>,
    pub patterns: Vec<P>,
    pub peek: bool,
}

/// The tuple-space interface (port of `Tuplespace[F]`).
#[async_trait]
pub trait Tuplespace<C, P, A, K>: Send + Sync {
    async fn consume(
        &self,
        channels: &[C],
        patterns: &[P],
        continuation: K,
        persist: bool,
        peeks: BTreeSet<usize>,
    ) -> std::result::Result<Option<(ContResult<C, P, K>, Vec<Result<C, A>>)>, RSpaceError>;

    async fn produce(
        &self,
        channel: C,
        data: A,
        persist: bool,
    ) -> std::result::Result<Option<(ContResult<C, P, K>, Vec<Result<C, A>>)>, RSpaceError>;

    async fn install(
        &self,
        channels: &[C],
        patterns: &[P],
        continuation: K,
    ) -> std::result::Result<Option<(K, Vec<A>)>, RSpaceError>;
}
