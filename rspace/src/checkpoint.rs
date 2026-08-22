//! Checkpoint data carriers.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/Checkpoint.scala`.

use std::collections::BTreeMap;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::hot_store::HotStoreState;
use crate::native_store::NativeStoreState;
use crate::trace::event::Produce;
use crate::trace::Log;

/// A soft (in-memory) checkpoint (port of `SoftCheckpoint`).
#[derive(Clone, Debug)]
pub struct SoftCheckpoint<C, P, A, K> {
    pub cache_snapshot: HotStoreState<C, P, A, K>,
    pub log: Log,
    pub produce_counter: BTreeMap<Produce, usize>,
    pub native_snapshot: NativeStoreState,
}

/// A checkpoint (port of `Checkpoint`).
#[derive(Clone, Debug)]
pub struct Checkpoint {
    pub root: Blake2b256Hash,
    pub log: Log,
}
