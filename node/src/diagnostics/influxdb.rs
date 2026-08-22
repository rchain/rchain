//! InfluxDB line-protocol reporters (port of `UdpInfluxDBReporter.scala` +
//! `BatchInfluxDBReporter.scala`). The line-protocol encoding, the UDP reporter, and the batch
//! reporter's HTTP POST are ported; the periodic flush/batching loop is left to the caller.

use std::net::{SocketAddr, UdpSocket};
use std::time::Duration;

use super::model::{MetricDistribution, MetricValue, PeriodSnapshot, Tags};

/// InfluxDB string escaping (port of `escapeString`): ` `, `=` and `,` are backslash-escaped.
pub fn escape_string(input: &str) -> String {
    input
        .replace(' ', "\\ ")
        .replace('=', "\\=")
        .replace(',', "\\,")
}

/// Java `Double.toString` (append `.0` for integral doubles).
fn double_to_string(value: f64) -> String {
    let s = value.to_string();
    if s.contains('.') || s.contains('e') || s.contains('E') {
        s
    } else {
        format!("{s}.0")
    }
}

fn write_name_and_tags(builder: &mut String, name: &str, metric_tags: &Tags, additional_tags: &Tags) {
    builder.push_str(name);
    let mut tags = metric_tags.clone();
    for (k, v) in additional_tags {
        tags.insert(k.clone(), v.clone());
    }
    for (key, value) in &tags {
        builder.push(',');
        builder.push_str(&escape_string(key));
        builder.push('=');
        builder.push_str(&escape_string(value));
    }
    builder.push(' ');
}

fn write_int_field(builder: &mut String, field_name: &str, value: i64, append_separator: bool) {
    builder.push_str(field_name);
    builder.push('=');
    builder.push_str(&value.to_string());
    builder.push('i');
    if append_separator {
        builder.push(',');
    }
}

fn write_double_field(builder: &mut String, field_name: &str, value: f64, append_separator: bool) {
    builder.push_str(field_name);
    builder.push('=');
    builder.push_str(&double_to_string(value));
    if append_separator {
        builder.push(',');
    }
}

fn write_timestamp(builder: &mut String, timestamp: i64, newline: bool) {
    builder.push(' ');
    builder.push_str(&timestamp.to_string());
    if newline {
        builder.push('\n');
    }
}

/// Encode a counter/gauge into a line-protocol record (port of `writeMetricValue`).
pub fn write_metric_value(
    builder: &mut String,
    metric: &MetricValue,
    field_name: &str,
    timestamp: i64,
    additional_tags: &Tags,
    newline: bool,
) {
    write_name_and_tags(builder, &metric.name, &metric.tags, additional_tags);
    write_int_field(builder, field_name, metric.value, false);
    write_timestamp(builder, timestamp, newline);
}

/// Encode a histogram/range-sampler into a line-protocol record (port of `writeMetricDistribution`).
pub fn write_metric_distribution(
    builder: &mut String,
    metric: &MetricDistribution,
    percentiles: &[f64],
    timestamp: i64,
    additional_tags: &Tags,
    newline: bool,
) {
    write_name_and_tags(builder, &metric.name, &metric.tags, additional_tags);
    write_int_field(builder, "count", metric.distribution.count, true);
    write_int_field(builder, "sum", metric.distribution.sum, true);
    write_int_field(builder, "min", metric.distribution.min, true);
    for &p in percentiles {
        write_double_field(
            builder,
            &format!("p{}", double_to_string(p)),
            metric.distribution.percentile(p) as f64,
            true,
        );
    }
    write_int_field(builder, "max", metric.distribution.max, false);
    write_timestamp(builder, timestamp, newline);
}

/// UDP reporter settings (port of `UdpInfluxDBReporter.Settings`).
#[derive(Clone, Debug, PartialEq)]
pub struct UdpSettings {
    pub address: SocketAddr,
    pub max_packet_size: i64,
    pub percentiles: Vec<f64>,
    pub additional_tags: Tags,
}

/// Batches line-protocol measurements and flushes over UDP (port of `MetricDataPacketBuffer`).
pub struct MetricDataPacketBuffer<'a> {
    max_packet_size: i64,
    socket: &'a UdpSocket,
    remote: SocketAddr,
    buffer: String,
}

impl<'a> MetricDataPacketBuffer<'a> {
    pub fn new(max_packet_size: i64, socket: &'a UdpSocket, remote: SocketAddr) -> Self {
        MetricDataPacketBuffer {
            max_packet_size,
            socket,
            remote,
            buffer: String::new(),
        }
    }

    pub fn append_measurement(&mut self, measurement: &str) -> std::io::Result<()> {
        let separator = "\n";
        if self.fits_on_buffer(&format!("{separator}{measurement}")) {
            let m_separator = if self.buffer.is_empty() { "" } else { separator };
            self.buffer.push_str(m_separator);
            self.buffer.push_str(measurement);
        } else {
            self.flush()?;
            self.buffer.push_str(measurement);
        }
        Ok(())
    }

    fn fits_on_buffer(&self, data: &str) -> bool {
        (self.buffer.len() + data.len()) as i64 <= self.max_packet_size
    }

    pub fn flush(&mut self) -> std::io::Result<()> {
        self.socket.send_to(self.buffer.as_bytes(), self.remote)?;
        self.buffer.clear();
        Ok(())
    }
}

/// UDP InfluxDB reporter (port of `UdpInfluxDBReporter`).
pub struct UdpInfluxDbReporter {
    settings: UdpSettings,
    socket: UdpSocket,
}

impl UdpInfluxDbReporter {
    pub fn new(settings: UdpSettings) -> std::io::Result<Self> {
        let socket = UdpSocket::bind("0.0.0.0:0")?;
        Ok(UdpInfluxDbReporter { settings, socket })
    }

    pub fn report_period_snapshot(&self, snapshot: &PeriodSnapshot) -> std::io::Result<()> {
        let mut buffer = MetricDataPacketBuffer::new(
            self.settings.max_packet_size,
            &self.socket,
            self.settings.address,
        );
        let timestamp = snapshot.to;

        for c in &snapshot.metrics.counters {
            let mut b = String::new();
            write_metric_value(
                &mut b,
                c,
                "count",
                timestamp,
                &self.settings.additional_tags,
                false,
            );
            buffer.append_measurement(&b)?;
        }
        for g in &snapshot.metrics.gauges {
            let mut b = String::new();
            write_metric_value(
                &mut b,
                g,
                "value",
                timestamp,
                &self.settings.additional_tags,
                false,
            );
            buffer.append_measurement(&b)?;
        }
        for h in &snapshot.metrics.histograms {
            let mut b = String::new();
            write_metric_distribution(
                &mut b,
                h,
                &self.settings.percentiles,
                timestamp,
                &self.settings.additional_tags,
                false,
            );
            buffer.append_measurement(&b)?;
        }
        for rs in &snapshot.metrics.range_samplers {
            let mut b = String::new();
            write_metric_distribution(
                &mut b,
                rs,
                &self.settings.percentiles,
                timestamp,
                &self.settings.additional_tags,
                false,
            );
            buffer.append_measurement(&b)?;
        }

        Ok(())
    }
}

/// Batch (HTTP) reporter settings (port of `BatchInfluxDBReporter.Settings`).
#[derive(Clone, Debug, PartialEq)]
pub struct BatchSettings {
    pub url: String,
    pub batch_interval: Duration,
    pub percentiles: Vec<f64>,
    pub credentials: Option<String>,
    pub additional_tags: Tags,
}

/// Batch InfluxDB reporter (port of `BatchInfluxDBReporter`); the HTTP POST transport is ported.
pub struct BatchInfluxDbReporter {
    settings: BatchSettings,
}

impl BatchInfluxDbReporter {
    pub fn new(settings: BatchSettings) -> Self {
        BatchInfluxDbReporter { settings }
    }

    /// Translate a whole snapshot into a `\n`-terminated line-protocol string (port of
    /// `translateToLineProtocol`).
    pub fn translate_to_line_protocol(&self, snapshot: &PeriodSnapshot) -> String {
        let mut builder = String::new();
        let timestamp = snapshot.to;

        for c in &snapshot.metrics.counters {
            write_metric_value(
                &mut builder,
                c,
                "count",
                timestamp,
                &self.settings.additional_tags,
                true,
            );
        }
        for g in &snapshot.metrics.gauges {
            write_metric_value(
                &mut builder,
                g,
                "value",
                timestamp,
                &self.settings.additional_tags,
                true,
            );
        }
        for h in &snapshot.metrics.histograms {
            write_metric_distribution(
                &mut builder,
                h,
                &self.settings.percentiles,
                timestamp,
                &self.settings.additional_tags,
                true,
            );
        }
        for rs in &snapshot.metrics.range_samplers {
            write_metric_distribution(
                &mut builder,
                rs,
                &self.settings.percentiles,
                timestamp,
                &self.settings.additional_tags,
                true,
            );
        }

        builder
    }

    /// Translate the snapshot to line protocol and POST it to the configured InfluxDB `/write`
    /// endpoint (port of `BatchInfluxDBReporter.reportPeriodSnapshot`). The periodic flush loop that
    /// drives this on `batch_interval` is left to the caller.
    pub async fn report_period_snapshot(&self, snapshot: &PeriodSnapshot) -> Result<(), String> {
        let body = self.translate_to_line_protocol(snapshot);
        let client = reqwest::Client::new();
        let mut req = client.post(&self.settings.url).body(body);
        if let Some(creds) = &self.settings.credentials {
            req = req.header(reqwest::header::AUTHORIZATION, format!("Basic {creds}"));
        }
        let resp = req.send().await.map_err(|e| e.to_string())?;
        if resp.status().is_success() {
            Ok(())
        } else {
            Err(format!("InfluxDB POST returned HTTP {}", resp.status()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::model::{Bucket, Distribution, Tags};

    #[test]
    fn escape_string_escapes_space_eq_comma() {
        assert_eq!(escape_string("a b=c,d"), "a\\ b\\=c\\,d");
    }

    #[test]
    fn counter_line_protocol() {
        let mut tags = Tags::new();
        tags.insert("tag".to_string(), "value".to_string());
        let metric = MetricValue {
            name: "my.counter".to_string(),
            tags,
            value: 5,
            unit: crate::diagnostics::model::MeasurementUnit::NONE,
        };
        let mut b = String::new();
        write_metric_value(&mut b, &metric, "count", 1234567890, &Tags::new(), false);
        assert_eq!(b, "my.counter,tag=value count=5i 1234567890");
    }

    #[test]
    fn distribution_line_protocol() {
        let metric = MetricDistribution {
            name: "hist".to_string(),
            tags: Tags::new(),
            unit: crate::diagnostics::model::MeasurementUnit::NONE,
            distribution: Distribution {
                count: 3,
                sum: 16,
                min: 1,
                max: 10,
                buckets: vec![
                    Bucket { value: 1, frequency: 1 },
                    Bucket { value: 5, frequency: 1 },
                    Bucket { value: 10, frequency: 1 },
                ],
            },
        };
        let mut b = String::new();
        write_metric_distribution(&mut b, &metric, &[0.5, 0.99], 1234567890, &Tags::new(), false);
        assert_eq!(b, "hist count=3i,sum=16i,min=1i,p0.5=1.0,p0.99=5.0,max=10i 1234567890");
    }

    #[test]
    fn batch_reporter_terminates_records_with_newline() {
        let snapshot = crate::diagnostics::model::PeriodSnapshot {
            from: 0,
            to: 1234567890,
            metrics: crate::diagnostics::model::MetricSnapshot {
                counters: vec![MetricValue {
                    name: "c".to_string(),
                    tags: Tags::new(),
                    value: 1,
                    unit: crate::diagnostics::model::MeasurementUnit::NONE,
                }],
                gauges: vec![],
                histograms: vec![],
                range_samplers: vec![],
            },
        };
        let reporter = BatchInfluxDbReporter::new(BatchSettings {
            url: "http://localhost:8086/write?precision=ms&db=metrics".to_string(),
            batch_interval: Duration::from_secs(10),
            percentiles: vec![0.5],
            credentials: None,
            additional_tags: Tags::new(),
        });
        assert_eq!(reporter.translate_to_line_protocol(&snapshot), "c count=1i 1234567890\n");
    }
}
