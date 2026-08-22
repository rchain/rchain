//! Tracing context (port of the `Trace`/`TraceId` half of `diagnostics/effects/package.scala`).
//!
//! The pure data model behind the node's request tracing: a monotonically-increasing span id
//! (`TraceId`), the span itself (`SourceTrace`), and the constructors (`Trace::source` /
//! `Trace::next`). The kamon span backend (`ks`/`mark`/`end`) and the effectful `span[F]` wrapper
//! are out of scope (the data model is ported; kamon span instrumentation is not).

use std::sync::atomic::{AtomicI64, Ordering};

use rchain_shared::metrics::Source;

/// A tracing span id (port of `Trace.TraceId(id: Long) extends AnyVal`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraceId(pub i64);

/// A concrete tracing span (port of `Trace.SourceTrace`).
///
/// kamon's `KSpan` (`ks`), `mark`, and `end` are the out-of-scope backend — only the data fields
/// are ported.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceTrace {
    pub s: Source,
    pub network_id: String,
    pub host: String,
    pub parent: Option<Box<SourceTrace>>,
}

/// A tracing span (port of the sealed `trait Trace`; `SourceTrace` is its only instance).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Trace {
    Source(SourceTrace),
}

/// Monotonically-increasing span-id counter (port of `Trace.counter = AtomicLong(0L)`).
static COUNTER: AtomicI64 = AtomicI64::new(0);

impl Trace {
    /// Build a root span with no parent (port of `Trace.source(s, networkId, host)`).
    pub fn source(source: Source, network_id: String, host: String) -> Trace {
        Trace::Source(SourceTrace {
            s: source,
            network_id,
            host,
            parent: None,
        })
    }

    /// Allocate the next span id (port of `Trace.next = TraceId(counter.incrementAndGet())`).
    pub fn next() -> TraceId {
        TraceId(COUNTER.fetch_add(1, Ordering::SeqCst) + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_is_monotonic_and_unique() {
        let a = Trace::next();
        let b = Trace::next();
        assert!(a.0 > 0);
        assert!(b.0 > a.0);
        assert_ne!(a, b);
    }

    #[test]
    fn source_builds_a_root_source_trace() {
        let src = Source::base().sub("store");
        let trace = Trace::source(src.clone(), "testnet".to_string(), "localhost".to_string());
        match trace {
            Trace::Source(st) => {
                assert_eq!(st.s, src);
                assert_eq!(st.network_id, "testnet");
                assert_eq!(st.host, "localhost");
                assert!(st.parent.is_none());
            }
        }
    }
}
