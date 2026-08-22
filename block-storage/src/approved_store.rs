//! Approved (finalized-fringe) store.
//!
//! Mirrors `block-storage/src/main/scala/coop/rchain/blockstorage/ApprovedStore.scala`. The store is
//! keyed by a single byte (the finalized-fringe key) and holds a `FinalizedFringe` protobuf.

use std::sync::Arc;

use rchain_models::casper::protocol::casper_message::FinalizedFringe;
use rchain_shared::store_manager::KeyValueStoreManager;
use rchain_shared::typed_store::{KeyValueTypedStore, KeyValueTypedStoreCodec};

use crate::dag::codecs::{ByteCodec, FringeCodec};

/// A typed store from a single-byte key to a finalized fringe (port of `ApprovedStore[F]`).
pub type ApprovedStore = Arc<dyn KeyValueTypedStore<u8, FinalizedFringe>>;

/// The finalized-fringe store key (the single key written by the approved store).
pub const FINALIZED_FRINGE_KEY: u8 = 42;

/// Open the approved store from a store manager (port of `approvedStore.create[F](kvm)`).
pub async fn create(kvm: &dyn KeyValueStoreManager) -> Result<ApprovedStore, String> {
    let store = kvm.store("finalized-store").await?;
    Ok(Arc::new(KeyValueTypedStoreCodec::new(
        store,
        Arc::new(ByteCodec),
        Arc::new(FringeCodec),
    )))
}
