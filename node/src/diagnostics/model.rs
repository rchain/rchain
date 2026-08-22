//! Metric snapshot data model (port of the kamon snapshot types the reporters read).

use std::collections::BTreeMap;

/// Metric tags (labels). Deterministic iteration order (sorted by key).
pub type Tags = BTreeMap<String, String>;

/// A measurement-unit dimension (port of `kamon.metric.MeasurementUnit.Dimension`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Dimension {
    Time,
    Information,
    None,
}

/// A measurement unit (port of `kamon.metric.MeasurementUnit`).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MeasurementUnit {
    pub dimension: Dimension,
    pub magnitude: f64,
}

impl MeasurementUnit {
    pub const NONE: MeasurementUnit = MeasurementUnit {
        dimension: Dimension::None,
        magnitude: 1.0,
    };
    pub const SECONDS: MeasurementUnit = MeasurementUnit {
        dimension: Dimension::Time,
        magnitude: 1.0,
    };
    pub const BYTES: MeasurementUnit = MeasurementUnit {
        dimension: Dimension::Information,
        magnitude: 1.0,
    };
}

/// A histogram bucket (port of `kamon.metric.Bucket`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bucket {
    pub value: i64,
    pub frequency: i64,
}

/// A value distribution (port of `kamon.metric.Distribution`).
#[derive(Clone, Debug, PartialEq)]
pub struct Distribution {
    pub count: i64,
    pub sum: i64,
    pub min: i64,
    pub max: i64,
    pub buckets: Vec<Bucket>,
}

impl Distribution {
    /// Value at the given percentile (0.0–1.0), by cumulative bucket frequency.
    pub fn percentile(&self, p: f64) -> i64 {
        if self.count <= 0 {
            return 0;
        }
        let target = (p * self.count as f64) as i64;
        let mut cumulative = 0i64;
        for bucket in &self.buckets {
            cumulative += bucket.frequency;
            if cumulative >= target {
                return bucket.value;
            }
        }
        self.max
    }
}

/// A counter/gauge snapshot (port of `kamon.metric.MetricValue`).
#[derive(Clone, Debug, PartialEq)]
pub struct MetricValue {
    pub name: String,
    pub tags: Tags,
    pub value: i64,
    pub unit: MeasurementUnit,
}

/// A histogram/range-sampler snapshot (port of `kamon.metric.MetricDistribution`).
#[derive(Clone, Debug, PartialEq)]
pub struct MetricDistribution {
    pub name: String,
    pub tags: Tags,
    pub unit: MeasurementUnit,
    pub distribution: Distribution,
}

/// A collection of metric snapshots (port of `kamon.metric.MetricSnapshot`).
#[derive(Clone, Debug, Default, PartialEq)]
pub struct MetricSnapshot {
    pub counters: Vec<MetricValue>,
    pub gauges: Vec<MetricValue>,
    pub histograms: Vec<MetricDistribution>,
    pub range_samplers: Vec<MetricDistribution>,
}

/// A period snapshot (port of `kamon.metric.PeriodSnapshot`). Timestamps are epoch milliseconds.
#[derive(Clone, Debug, PartialEq)]
pub struct PeriodSnapshot {
    pub from: i64,
    pub to: i64,
    pub metrics: MetricSnapshot,
}
