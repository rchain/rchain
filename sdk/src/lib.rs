//! Faithful Rust port of the RChain `sdk` module.
//!
//! Mirrors `sdk/src/main/scala/coop/rchain/sdk/`. This crate is a *leaf* in the dependency graph
//! (no internal dependencies) and carries two formal laws from [`spec/INVENTORY.md`]:
//! Law 14 (the >2/3 supermajority threshold) and Law 17 (deterministic merge/conflict resolution).

pub mod block;
pub mod casper_syntax;
pub mod consensus;
pub mod dag;
pub mod error;
pub mod primitive;
