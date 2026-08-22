//! Prometheus reporter (port of `NewPrometheusReporter.scala`).
//!
//! Accumulates `PeriodSnapshot`s and, on each report, re-renders the merged snapshot into
//! Prometheus text-exposition format via `ScrapeDataBuilder`. The reporter's configuration is read
//! from kamon in the original; here the ported `Configuration` (with its `Default`) stands in, since
//! the kamon config backend is deferred.

use std::collections::BTreeMap;
use std::sync::{Mutex, MutexGuard};
use std::time::Duration;

use super::model::{
    Distribution, MetricDistribution, MetricSnapshot, MetricValue, PeriodSnapshot, Tags,
};
use super::scrape_data_builder::{Configuration, ScrapeDataBuilder};

/// Accumulates `PeriodSnapshot`s by merging (port of kamon's `PeriodSnapshotAccumulator`).
///
/// The retention/refresh window mechanics are inert for the reporter's fixed configuration
/// (`retention = 5 years`, `refreshInterval = 0`) — snapshots are never evicted — so the port
/// merges every added snapshot and returns the merge from `peek`. Merge semantics match kamon's
/// `MetricSnapshotBuffer`: counters sum, gauges take the latest value, and distributions merge
/// (count/sum added, min/min, max/max, buckets combined by value).
pub struct PeriodSnapshotAccumulator {
    #[allow(dead_code)]
    retention: Duration,
    #[allow(dead_code)]
    refresh_interval: Duration,
    from: i64,
    to: i64,
    has_data: bool,
    counters: BTreeMap<(String, Tags), MetricValue>,
    gauges: BTreeMap<(String, Tags), MetricValue>,
    histograms: BTreeMap<(String, Tags), MetricDistribution>,
    range_samplers: BTreeMap<(String, Tags), MetricDistribution>,
}

impl PeriodSnapshotAccumulator {
    pub fn new(retention: Duration, refresh_interval: Duration) -> Self {
        PeriodSnapshotAccumulator {
            retention,
            refresh_interval,
            from: 0,
            to: 0,
            has_data: false,
            counters: BTreeMap::new(),
            gauges: BTreeMap::new(),
            histograms: BTreeMap::new(),
            range_samplers: BTreeMap::new(),
        }
    }

    /// Merge a snapshot into the accumulator (port of `PeriodSnapshotAccumulator.add`).
    pub fn add(&mut self, snapshot: &PeriodSnapshot) {
        if self.has_data {
            self.from = self.from.min(snapshot.from);
            self.to = self.to.max(snapshot.to);
        } else {
            self.from = snapshot.from;
            self.to = snapshot.to;
            self.has_data = true;
        }

        for c in &snapshot.metrics.counters {
            self.counters
                .entry((c.name.clone(), c.tags.clone()))
                .and_modify(|acc| acc.value += c.value)
                .or_insert_with(|| c.clone());
        }

        for g in &snapshot.metrics.gauges {
            self.gauges
                .insert((g.name.clone(), g.tags.clone()), g.clone());
        }

        for h in &snapshot.metrics.histograms {
            merge_distribution_into(&mut self.histograms, h);
        }

        for rs in &snapshot.metrics.range_samplers {
            merge_distribution_into(&mut self.range_samplers, rs);
        }
    }

    /// Return the merged snapshot (port of `PeriodSnapshotAccumulator.peek`).
    pub fn peek(&self) -> PeriodSnapshot {
        PeriodSnapshot {
            from: self.from,
            to: self.to,
            metrics: MetricSnapshot {
                counters: self.counters.values().cloned().collect(),
                gauges: self.gauges.values().cloned().collect(),
                histograms: self.histograms.values().cloned().collect(),
                range_samplers: self.range_samplers.values().cloned().collect(),
            },
        }
    }
}

/// Merge two distributions (port of kamon `Distribution.merge`): counts/sums add, min/min,
/// max/max, and buckets with equal values combine their frequencies.
fn merge_distribution(a: &Distribution, b: &Distribution) -> Distribution {
    let mut buckets: BTreeMap<i64, i64> = BTreeMap::new();
    for bucket in a.buckets.iter().chain(b.buckets.iter()) {
        *buckets.entry(bucket.value).or_insert(0) += bucket.frequency;
    }
    Distribution {
        count: a.count + b.count,
        sum: a.sum + b.sum,
        min: a.min.min(b.min),
        max: a.max.max(b.max),
        buckets: buckets
            .into_iter()
            .map(|(value, frequency)| super::model::Bucket { value, frequency })
            .collect(),
    }
}

fn merge_distribution_into(
    map: &mut BTreeMap<(String, Tags), MetricDistribution>,
    incoming: &MetricDistribution,
) {
    let key = (incoming.name.clone(), incoming.tags.clone());
    map.entry(key)
        .and_modify(|acc| acc.distribution = merge_distribution(&acc.distribution, &incoming.distribution))
        .or_insert_with(|| incoming.clone());
}

/// Initial scrape payload before any snapshot has been reported (port of the reporter's initial
/// `preparedScrapeData`).
const EMPTY_SCRAPE_DATA: &str =
    "# The kamon-prometheus module didn't receive any data just yet.\n";

/// Prometheus reporter (port of `NewPrometheusReporter`).
pub struct NewPrometheusReporter {
    accumulator: Mutex<PeriodSnapshotAccumulator>,
    config: Configuration,
    prepared: Mutex<String>,
}

impl NewPrometheusReporter {
    pub fn new(config: Configuration) -> Self {
        NewPrometheusReporter {
            accumulator: Mutex::new(PeriodSnapshotAccumulator::new(
                Duration::from_secs(365 * 5 * 24 * 60 * 60),
                Duration::ZERO,
            )),
            config,
            prepared: Mutex::new(EMPTY_SCRAPE_DATA.to_string()),
        }
    }

    /// Accumulate `snapshot` and re-render the merged result (port of `reportPeriodSnapshot`).
    pub fn report_period_snapshot(&self, snapshot: &PeriodSnapshot) {
        let mut accumulator = lock(&self.accumulator);
        accumulator.add(snapshot);
        let current = accumulator.peek();
        drop(accumulator);

        let mut builder = ScrapeDataBuilder::new(self.config.clone(), Tags::new());
        builder
            .append_counters(&current.metrics.counters)
            .append_gauges(&current.metrics.gauges)
            .append_histograms(&current.metrics.histograms)
            .append_histograms(&current.metrics.range_samplers);

        *lock(&self.prepared) = builder.build();
    }

    /// Return the last-rendered scrape payload (port of `scrapeData`).
    pub fn scrape_data(&self) -> String {
        lock(&self.prepared).clone()
    }
}

fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|p| p.into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::model::{Bucket, Dimension, MeasurementUnit};

    fn mv(name: &str, value: i64) -> MetricValue {
        MetricValue {
            name: name.to_string(),
            tags: Tags::new(),
            value,
            unit: MeasurementUnit::NONE,
        }
    }

    fn snapshot(from: i64, to: i64, counters: Vec<i64>, gauges: Vec<i64>) -> PeriodSnapshot {
        PeriodSnapshot {
            from,
            to,
            metrics: MetricSnapshot {
                counters: counters.into_iter().map(|v| mv("c", v)).collect(),
                gauges: gauges.into_iter().map(|v| mv("g", v)).collect(),
                histograms: vec![],
                range_samplers: vec![],
            },
        }
    }

    #[test]
    fn counters_accumulate_and_gauges_take_latest() {
        let mut acc =
            PeriodSnapshotAccumulator::new(Duration::from_secs(1), Duration::ZERO);
        acc.add(&snapshot(0, 1000, vec![1], vec![10]));
        acc.add(&snapshot(1000, 2000, vec![2], vec![99]));

        let p = acc.peek();
        assert_eq!(p.from, 0);
        assert_eq!(p.to, 2000);
        assert_eq!(p.metrics.counters.len(), 1);
        assert_eq!(p.metrics.counters[0].value, 3);
        assert_eq!(p.metrics.gauges.len(), 1);
        assert_eq!(p.metrics.gauges[0].value, 99);
    }

    #[test]
    fn distributions_merge_buckets_and_stats() {
        let dist = |count, sum, min, max, buckets: Vec<(i64, i64)>| Distribution {
            count,
            sum,
            min,
            max,
            buckets: buckets
                .into_iter()
                .map(|(value, frequency)| Bucket { value, frequency })
                .collect(),
        };
        let merged = merge_distribution(
            &dist(1, 5, 5, 5, vec![(5, 1)]),
            &dist(2, 20, 3, 15, vec![(5, 1), (15, 1)]),
        );
        assert_eq!(merged.count, 3);
        assert_eq!(merged.sum, 25);
        assert_eq!(merged.min, 3);
        assert_eq!(merged.max, 15);
        assert_eq!(
            merged.buckets,
            vec![
                Bucket { value: 5, frequency: 2 },
                Bucket { value: 15, frequency: 1 }
            ]
        );
    }

    #[test]
    fn reporter_renders_scrape_data_from_snapshots() {
        let reporter = NewPrometheusReporter::new(Configuration::default());
        assert_eq!(reporter.scrape_data(), EMPTY_SCRAPE_DATA);

        reporter.report_period_snapshot(&PeriodSnapshot {
            from: 0,
            to: 1000,
            metrics: MetricSnapshot {
                counters: vec![mv("my.counter", 5)],
                gauges: vec![],
                histograms: vec![],
                range_samplers: vec![],
            },
        });
        assert_eq!(
            reporter.scrape_data(),
            "# TYPE my_counter_total counter\nmy_counter_total 5.0\n"
        );

        // A second snapshot accumulates: the counter sums across reports.
        reporter.report_period_snapshot(&PeriodSnapshot {
            from: 1000,
            to: 2000,
            metrics: MetricSnapshot {
                counters: vec![mv("my.counter", 2)],
                gauges: vec![],
                histograms: vec![],
                range_samplers: vec![],
            },
        });
        assert_eq!(
            reporter.scrape_data(),
            "# TYPE my_counter_total counter\nmy_counter_total 7.0\n"
        );
    }

    #[test]
    fn range_samplers_are_rendered_as_histograms() {
        let reporter = NewPrometheusReporter::new(Configuration::default());
        let mut tags = Tags::new();
        tags.insert("le".to_string(), "ignored".to_string());
        reporter.report_period_snapshot(&PeriodSnapshot {
            from: 0,
            to: 1000,
            metrics: MetricSnapshot {
                counters: vec![],
                gauges: vec![],
                histograms: vec![],
                range_samplers: vec![MetricDistribution {
                    name: "sampler".to_string(),
                    tags,
                    unit: MeasurementUnit {
                        dimension: Dimension::None,
                        magnitude: 1.0,
                    },
                    distribution: Distribution {
                        count: 1,
                        sum: 42,
                        min: 42,
                        max: 42,
                        buckets: vec![Bucket { value: 42, frequency: 1 }],
                    },
                }],
            },
        });
        let out = reporter.scrape_data();
        assert!(out.starts_with("# TYPE sampler histogram\n"), "got: {out}");
    }
}
