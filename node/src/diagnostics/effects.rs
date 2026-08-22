//! In-memory metric registry (port of the `metrics[F]` half of `effects/package.scala`).
//!
//! Implements `rchain_shared::metrics::Metrics` and turns instrument calls into a
//! `PeriodSnapshot` the reporters can consume. Replaces kamon's `TrieMap[String, Metric[_]]`
//! backend with a `Mutex`-guarded `BTreeMap` accumulator.

use std::collections::BTreeMap;
use std::sync::{Mutex, MutexGuard};
use std::time::{SystemTime, UNIX_EPOCH};

use rchain_shared::metrics::{Metrics, Source};

use super::model::{
    Bucket, Distribution, MeasurementUnit, MetricDistribution, MetricSnapshot, MetricValue,
    PeriodSnapshot, Tags,
};

#[derive(Clone, Copy, Debug, Default)]
struct HistogramAcc {
    count: i64,
    sum: i64,
    min: i64,
    max: i64,
}

#[derive(Debug, Default)]
struct Inner {
    counters: BTreeMap<String, i64>,
    gauges: BTreeMap<String, i64>,
    range_samplers: BTreeMap<String, i64>,
    histograms: BTreeMap<String, HistogramAcc>,
}

/// A thread-safe in-memory metric registry (port of `effects.metrics`).
#[derive(Debug, Default)]
pub struct MetricsRegistry {
    inner: Mutex<Inner>,
}

impl MetricsRegistry {
    pub fn new() -> Self {
        MetricsRegistry::default()
    }

    fn lock(&self) -> MutexGuard<'_, Inner> {
        self.inner.lock().unwrap_or_else(|p| p.into_inner())
    }

    fn key(source: &Source, name: &str) -> String {
        source.sub(name).0
    }

    /// Produce a snapshot of the currently-accumulated metrics (port of the reporters' input).
    pub fn snapshot(&self) -> PeriodSnapshot {
        let inner = self.lock();

        let mut counters = Vec::new();
        for (name, value) in &inner.counters {
            counters.push(MetricValue {
                name: name.clone(),
                tags: Tags::new(),
                value: *value,
                unit: MeasurementUnit::NONE,
            });
        }

        let mut gauges = Vec::new();
        for (name, value) in &inner.gauges {
            gauges.push(MetricValue {
                name: name.clone(),
                tags: Tags::new(),
                value: *value,
                unit: MeasurementUnit::NONE,
            });
        }

        let mut histograms = Vec::new();
        for (name, h) in &inner.histograms {
            histograms.push(MetricDistribution {
                name: name.clone(),
                tags: Tags::new(),
                unit: MeasurementUnit::NONE,
                distribution: Distribution {
                    count: h.count,
                    sum: h.sum,
                    min: h.min,
                    max: h.max,
                    buckets: if h.count > 0 {
                        vec![Bucket {
                            value: h.max,
                            frequency: h.count,
                        }]
                    } else {
                        Vec::new()
                    },
                },
            });
        }

        let mut range_samplers = Vec::new();
        for (name, value) in &inner.range_samplers {
            range_samplers.push(MetricDistribution {
                name: name.clone(),
                tags: Tags::new(),
                unit: MeasurementUnit::NONE,
                distribution: Distribution {
                    count: 1,
                    sum: *value,
                    min: *value,
                    max: *value,
                    buckets: vec![Bucket {
                        value: *value,
                        frequency: 1,
                    }],
                },
            });
        }

        PeriodSnapshot {
            from: 0,
            to: now_millis(),
            metrics: MetricSnapshot {
                counters,
                gauges,
                histograms,
                range_samplers,
            },
        }
    }
}

impl Metrics for MetricsRegistry {
    fn increment_counter(&self, source: &Source, name: &str, delta: i64) {
        let mut inner = self.lock();
        *inner.counters.entry(Self::key(source, name)).or_default() += delta;
    }

    fn increment_sampler(&self, source: &Source, name: &str, delta: i64) {
        let mut inner = self.lock();
        *inner.range_samplers.entry(Self::key(source, name)).or_default() += delta;
    }

    fn sample(&self, source: &Source, name: &str) {
        let mut inner = self.lock();
        inner.range_samplers.entry(Self::key(source, name)).or_default();
    }

    fn set_gauge(&self, source: &Source, name: &str, value: i64) {
        let mut inner = self.lock();
        inner.gauges.insert(Self::key(source, name), value);
    }

    fn increment_gauge(&self, source: &Source, name: &str, delta: i64) {
        let mut inner = self.lock();
        *inner.gauges.entry(Self::key(source, name)).or_default() += delta;
    }

    fn decrement_gauge(&self, source: &Source, name: &str, delta: i64) {
        let mut inner = self.lock();
        *inner.gauges.entry(Self::key(source, name)).or_default() -= delta;
    }

    fn record(&self, source: &Source, name: &str, value: i64, count: i64) {
        let mut inner = self.lock();
        let h = inner.histograms.entry(Self::key(source, name)).or_default();
        if h.count == 0 {
            h.min = value;
            h.max = value;
        } else {
            h.min = h.min.min(value);
            h.max = h.max.max(value);
        }
        h.count += count;
        h.sum += value * count;
    }
}

fn now_millis() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counters_gauges_and_histograms_accumulate() {
        let registry = MetricsRegistry::new();
        let src = Source::base();

        registry.increment_counter(&src, "req", 3);
        registry.increment_counter(&src, "req", 2);
        registry.set_gauge(&src, "conn", 10);
        registry.increment_gauge(&src, "conn", 5);
        registry.record(&src, "latency", 100, 2);
        registry.record(&src, "latency", 300, 1);

        let snap = registry.snapshot();

        let counter = snap
            .metrics
            .counters
            .iter()
            .find(|m| m.name == "rchain.req")
            .unwrap();
        assert_eq!(counter.value, 5);

        let gauge = snap
            .metrics
            .gauges
            .iter()
            .find(|m| m.name == "rchain.conn")
            .unwrap();
        assert_eq!(gauge.value, 15);

        let hist = snap
            .metrics
            .histograms
            .iter()
            .find(|m| m.name == "rchain.latency")
            .unwrap();
        assert_eq!(hist.distribution.count, 3);
        assert_eq!(hist.distribution.sum, 500);
        assert_eq!(hist.distribution.min, 100);
        assert_eq!(hist.distribution.max, 300);
    }

    #[test]
    fn empty_registry_produces_empty_snapshot() {
        let registry = MetricsRegistry::new();
        let snap = registry.snapshot();
        assert!(snap.metrics.counters.is_empty());
        assert!(snap.metrics.gauges.is_empty());
        assert!(snap.metrics.histograms.is_empty());
        assert!(snap.metrics.range_samplers.is_empty());
    }
}
