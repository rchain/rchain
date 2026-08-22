//! The event log / trace types.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/trace/`.

pub mod event;
pub mod tuplespace_event;

pub use event::{Comm, Consume, Event, Produce};

/// The event log (port of `trace.Log`).
pub type Log = Vec<Event>;
