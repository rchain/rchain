//! Faithful Rust port of the RChain rholang interpreter core.
//!
//! Mirrors `rholang/src/main/scala/coop/rchain/rholang/interpreter/`. This crate ports the
//! interpreter core: de Bruijn `Env`, capture-avoiding `Substitute`, the spatial matcher, gas
//! accounting, the parser + normalizer, the reducer, and the `RhoRuntime`/`ReplayRhoRuntime`
//! (`ReportingRuntime`) over the in-memory + LMDB tuple spaces.

pub mod accounting;
pub mod compiler;
pub mod contract_call;
pub mod dispatch;
pub mod env;
pub mod errors;
pub mod evaluate_result;
pub mod matcher;
pub mod merging;
pub mod native_state;
pub mod normalizer;
pub mod parser;
pub mod pretty_printer;
pub mod proc_ast;
pub mod reduce;
pub mod registry;
pub mod reporting_runtime;
pub mod runtime;
pub mod storage;
pub mod storage_printer;
pub mod substitute;
pub mod system_processes;
pub mod util;
