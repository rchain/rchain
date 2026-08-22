//! Faithful Rust port of the RChain `block-storage` module (the CBC-Casper DAG finalizer /
//! estimator and the block metadata store).
//!
//! Mirrors `block-storage/src/main/scala/coop/rchain/blockstorage/`. Implements Laws 14 (finality
//! requires > 2/3 bonded stake), 15 (fringe/seen monotone), and 18 (height map contiguous; fringe
//! identity order-independent). The finalizer core is pure and generic (`M: Ord`, `S: Ord`), so it
//! has no async or serialization dependencies.
//!
//! The store glue (`BlockStore`/`ApprovedStore`) lives in [`block_store`]/[`approved_store`]; the
//! concrete `BlockDagStorage` implementation lives in the `casper` crate (`BlockDagKeyValueStorage`).

pub mod approved_store;
pub mod block_store;
pub mod dag;
pub mod errors;
pub mod syntax;
