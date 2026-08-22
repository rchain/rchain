//! Metrics facade.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/metrics/Metrics.scala`. The Scala `F[_]` effect is
//! simplified to synchronous calls. The no-op instances (`MetricsNOP`, `NoopSpan`) are the
//! load-bearing pieces here.

/// A metrics source prefix (port of `Metrics.Source`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Source(pub String);

impl Source {
    pub const BASE: &'static str = "rchain";
    pub fn base() -> Self {
        Source(Self::BASE.to_string())
    }
    pub fn sub(&self, name: &str) -> Self {
        Source(format!("{}.{}", self.0, name))
    }
}

/// A metrics sink (port of `Metrics[F]`).
pub trait Metrics {
    fn increment_counter(&self, source: &Source, name: &str, delta: i64);
    fn increment_sampler(&self, source: &Source, name: &str, delta: i64);
    fn sample(&self, source: &Source, name: &str);
    fn set_gauge(&self, source: &Source, name: &str, value: i64);
    fn increment_gauge(&self, source: &Source, name: &str, delta: i64);
    fn decrement_gauge(&self, source: &Source, name: &str, delta: i64);
    fn record(&self, source: &Source, name: &str, value: i64, count: i64);
}

/// No-op metrics sink (port of `Metrics.MetricsNOP`).
#[derive(Default)]
pub struct MetricsNop;

impl Metrics for MetricsNop {
    fn increment_counter(&self, _source: &Source, _name: &str, _delta: i64) {}
    fn increment_sampler(&self, _source: &Source, _name: &str, _delta: i64) {}
    fn sample(&self, _source: &Source, _name: &str) {}
    fn set_gauge(&self, _source: &Source, _name: &str, _value: i64) {}
    fn increment_gauge(&self, _source: &Source, _name: &str, _delta: i64) {}
    fn decrement_gauge(&self, _source: &Source, _name: &str, _delta: i64) {}
    fn record(&self, _source: &Source, _name: &str, _value: i64, _count: i64) {}
}

/// A span (port of `Span[F]`).
pub trait Span {
    fn mark(&self, name: &str);
}

/// No-op span (port of `Span.NoopSpan`).
#[derive(Default)]
pub struct NoopSpan;

impl Span for NoopSpan {
    fn mark(&self, _name: &str) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_composes() {
        let base = Source::base();
        assert_eq!(base.sub("store").0, "rchain.store");
    }

    #[test]
    fn metrics_nop_accepts_any_call() {
        let m = MetricsNop;
        let src = Source::base();
        m.increment_counter(&src, "counter", 1);
        m.record(&src, "hist", 42, 1);
    }
}
