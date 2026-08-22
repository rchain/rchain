//! Faithful Rust port of the RChain `casper` module (CBC-Casper consensus + DAG).
//!
//! Mirrors `casper/src/main/scala/coop/rchain/casper/`. Encodes Laws 14–18 (finality, fringe
//! monotonicity, block numbering/content-addressing, merge determinism, height-map contiguity).

pub mod api;
pub mod block_metadata_store;
pub mod block_random_seed;
pub mod block_status;
pub mod blocks;
pub mod bonds_parser;
pub mod conf;
pub mod construct_deploy;
pub mod dag;
pub mod engine;
pub mod event_converter;
pub mod genesis;
pub mod interpreter_util;
pub mod merging;
pub mod multi_parent_casper;
pub mod protocol;
pub mod proto_util;
pub mod reporting;
pub mod rholang;
pub mod runtime_manager;
pub mod runtime_replay;
pub mod state;
pub mod storage;
pub mod system_deploy;
pub mod tools;
pub mod validate;
pub mod validator_identity;
pub mod vault_parser;

pub use conf::{CasperConf, GenesisBlockData};
