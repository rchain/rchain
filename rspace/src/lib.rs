//! Faithful Rust port of the RChain `rspace` module (the concurrent tuple space and its
//! content-addressed radix history).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/`. Encodes Laws 7–11 (join commutativity,
//! deterministic COMM, merge monoid, Merkle determinism, replay determinism). The crate is generic
//! over channel/pattern/datum/continuation types `C/P/A/K` (via `Serialize`) plus a `Match<P, A>`
//! typeclass.

pub mod checkpoint;
pub mod concurrent;
pub mod errors;
pub mod factory;
pub mod hashing;
pub mod history;
pub mod hot_store;
pub mod hot_store_action;
pub mod hot_store_trie_action;
pub mod i_replay_space;
pub mod i_space;
pub mod internal;
pub mod lock;
pub mod match_;
pub mod merger;
pub mod native_store;
#[cfg(test)]
mod property_tests;
pub mod replay_rspace;
pub mod reporting_rspace;
pub mod reporting_transformer;
pub mod rspace;
pub mod serializers;
pub mod space_matcher;
pub mod state;
pub mod trace;
pub mod tuple_space;
pub mod util;
