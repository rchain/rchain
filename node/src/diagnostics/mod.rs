//! Node diagnostics (port of `coop.rchain.node.diagnostics`).
//!
//! The metrics *export* side: a snapshot data model and the Prometheus/InfluxDB text-format
//! encoders. The kamon metrics backend is replaced by the in-memory `effects::MetricsRegistry`; the
//! kamon tracing backend (`span[F]`/`mark`/`end`) is out of scope (no kamon instrumentation in
//! Rust).

pub mod effects;
pub mod influxdb;
pub mod model;
pub mod prometheus_reporter;
pub mod scrape_data_builder;
pub mod trace;

pub use prometheus_reporter::{NewPrometheusReporter, PeriodSnapshotAccumulator};
pub use trace::{Trace, TraceId};
