//! Faithful Rust port of the RChain `shared` module.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/`. This crate is *near-leaf* (the Scala version
//! depends only on `sdk`) and is imported by everything else. Ported here are the
//! framework-agnostic, load-bearing pieces plus the LMDB store (`lmdb` feature); scodec codecs,
//! cats-contrib monad shims, `StreamT`, metrics, and fs2/grpc/monix interop are deferred.

pub mod base16;
pub mod dag;
pub mod debug;
pub mod key_value_cache;
pub mod language;
pub mod long_ops;
pub mod matcher;
pub mod maybe_cell;
pub mod path_ops;
pub mod printer;
pub mod rate_limiter;
pub mod refined;
pub mod seq_ops;
pub mod serialize;
pub mod state;
pub mod stopwatch;
pub mod store;
pub mod string_ops;
pub mod sync_var;
pub mod terminal_mode;
pub mod throwable_ops;
pub mod time;

#[cfg(feature = "tokio")]
pub mod compression;
#[cfg(feature = "lmdb")]
pub mod lmdb;
#[cfg(feature = "tokio")]
pub mod log;
#[cfg(feature = "tokio")]
pub mod metrics;
#[cfg(feature = "tokio")]
pub mod monixable;
#[cfg(feature = "tokio")]
pub mod store_manager;
#[cfg(feature = "tokio")]
pub mod typed_store;
