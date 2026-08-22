//! Hot-store diff actions (fed into checkpoint).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/HotStoreAction.scala`.

use crate::internal::{Datum, WaitingContinuation};

/// A hot-store mutation (port of `HotStoreAction`).
#[derive(Clone, Debug, PartialEq)]
pub enum HotStoreAction<C, P, A, K> {
    InsertData(C, Vec<Datum<A>>),
    InsertJoins(C, Vec<Vec<C>>),
    InsertContinuations(Vec<C>, Vec<WaitingContinuation<P, K>>),
    DeleteData(C),
    DeleteJoins(C),
    DeleteContinuations(Vec<C>),
}
