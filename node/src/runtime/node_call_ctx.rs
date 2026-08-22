//! Node call context (port of the pure `NodeCallCtx` in `runtime/NodeCallCtx.scala`).
//!
//! The `NodeCallCtxReader` (cats-effect `ConcurrentEffect[ReaderT]`) is effect-machinery with no
//! Rust analog and is deferred.

use crate::diagnostics::trace::{Trace, TraceId};

/// Request-local tracing context (port of `final case class NodeCallCtx(trace: TraceId)`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodeCallCtx {
    pub trace: TraceId,
}

impl NodeCallCtx {
    /// Fresh context seeded from the global counter (port of `NodeCallCtx.init`).
    pub fn init() -> Self {
        NodeCallCtx { trace: Trace::next() }
    }

    /// Advance to the next span id (port of `NodeCallCtx.next`).
    pub fn next(&self) -> Self {
        NodeCallCtx { trace: Trace::next() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn init_uses_the_global_counter() {
        let ctx = NodeCallCtx::init();
        assert!(ctx.trace.0 > 0);
    }

    #[test]
    fn next_advances_to_a_fresh_id() {
        let ctx = NodeCallCtx::init();
        let next = ctx.next();
        assert_ne!(ctx.trace, next.trace);
        assert!(next.trace.0 > ctx.trace.0);
    }
}
