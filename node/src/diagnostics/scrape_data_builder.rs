//! Prometheus text-exposition writer (port of `ScrapeDataBuilder.scala`).

use std::collections::BTreeMap;

use super::model::{Dimension, MeasurementUnit, MetricDistribution, MetricValue, Tags};

/// Reporter configuration (port of `NewPrometheusReporter.Configuration`).
#[derive(Clone, Debug, PartialEq)]
pub struct Configuration {
    pub start_embedded_server: bool,
    pub embedded_server_hostname: String,
    pub embedded_server_port: i32,
    pub default_buckets: Vec<f64>,
    pub time_buckets: Vec<f64>,
    pub information_buckets: Vec<f64>,
    pub custom_buckets: BTreeMap<String, Vec<f64>>,
    pub include_environment_tags: bool,
}

impl Default for Configuration {
    fn default() -> Self {
        Configuration {
            start_embedded_server: false,
            embedded_server_hostname: "0.0.0.0".to_string(),
            embedded_server_port: 9095,
            default_buckets: vec![
                0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1.0, 2.5, 5.0, 7.5, 10.0,
            ],
            time_buckets: vec![
                0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1.0, 2.5, 5.0, 7.5, 10.0,
            ],
            information_buckets: vec![
                512.0, 1024.0, 2048.0, 4096.0, 8192.0, 16384.0, 32768.0, 65536.0, 131072.0,
                262144.0, 524288.0, 1048576.0,
            ],
            custom_buckets: BTreeMap::new(),
            include_environment_tags: false,
        }
    }
}

/// Builds a Prometheus text-exposition-format document from a snapshot (port of
/// `ScrapeDataBuilder`).
#[derive(Clone, Debug, Default)]
pub struct ScrapeDataBuilder {
    config: Configuration,
    environment_tags: Tags,
    builder: String,
}

impl ScrapeDataBuilder {
    pub fn new(config: Configuration, environment_tags: Tags) -> Self {
        ScrapeDataBuilder {
            config,
            environment_tags,
            builder: String::new(),
        }
    }

    pub fn build(&self) -> String {
        self.builder.clone()
    }

    pub fn append_counters(&mut self, counters: &[MetricValue]) -> &mut Self {
        for (name, snapshots) in group_by_name(counters, |m| m.name.as_str()) {
            self.append_value_metric("counter", true, &name, &snapshots);
        }
        self
    }

    pub fn append_gauges(&mut self, gauges: &[MetricValue]) -> &mut Self {
        for (name, snapshots) in group_by_name(gauges, |m| m.name.as_str()) {
            self.append_value_metric("gauge", false, &name, &snapshots);
        }
        self
    }

    pub fn append_histograms(&mut self, histograms: &[MetricDistribution]) -> &mut Self {
        for (name, snapshots) in group_by_name(histograms, |m| m.name.as_str()) {
            self.append_distribution_metric(&name, &snapshots);
        }
        self
    }

    fn push(&mut self, s: &str) {
        self.builder.push_str(s);
    }

    fn append_value_metric(
        &mut self,
        metric_type: &str,
        always_increasing: bool,
        metric_name: &str,
        snapshots: &[&MetricValue],
    ) {
        let unit = snapshots
            .first()
            .map(|m| m.unit)
            .unwrap_or(MeasurementUnit::NONE);
        let mut normalized = normalize_metric_name(metric_name, &unit);
        if always_increasing {
            normalized.push_str("_total");
        }

        self.push("# TYPE ");
        self.push(&normalized);
        self.push(" ");
        self.push(metric_type);
        self.push("\n");

        for metric in snapshots {
            self.push(&normalized);
            self.append_tags(&metric.tags);
            self.push(" ");
            self.push(&format_decimal(scale(metric.value, &metric.unit)));
            self.push("\n");
        }
    }

    fn append_distribution_metric(&mut self, metric_name: &str, snapshots: &[&MetricDistribution]) {
        let unit = snapshots
            .first()
            .map(|m| m.unit)
            .unwrap_or(MeasurementUnit::NONE);
        let normalized = normalize_metric_name(metric_name, &unit);

        self.push("# TYPE ");
        self.push(&normalized);
        self.push(" histogram\n");

        for metric in snapshots {
            if metric.distribution.count > 0 {
                let buckets = self.resolve_bucket_configuration(metric);
                self.append_histogram_buckets(&normalized, &metric.tags, metric, &buckets);

                let count = format_decimal(metric.distribution.count as f64);
                let sum = format_decimal(scale(metric.distribution.sum, &metric.unit));
                self.append_time_serie_value(&normalized, &metric.tags, &count, "_count");
                self.append_time_serie_value(&normalized, &metric.tags, &sum, "_sum");
            }
        }
    }

    fn append_time_serie_value(
        &mut self,
        name: &str,
        tags: &Tags,
        value: &str,
        suffix: &str,
    ) {
        self.push(name);
        self.push(suffix);
        self.append_tags(tags);
        self.push(" ");
        self.push(value);
        self.push("\n");
    }

    fn resolve_bucket_configuration(&self, metric: &MetricDistribution) -> Vec<f64> {
        if let Some(buckets) = self.config.custom_buckets.get(&metric.name) {
            buckets.clone()
        } else {
            match metric.unit.dimension {
                Dimension::Time => self.config.time_buckets.clone(),
                Dimension::Information => self.config.information_buckets.clone(),
                Dimension::None => self.config.default_buckets.clone(),
            }
        }
    }

    fn append_histogram_buckets(
        &mut self,
        name: &str,
        tags: &Tags,
        metric: &MetricDistribution,
        buckets: &[f64],
    ) {
        let mut buckets_iter = metric.distribution.buckets.iter();
        let Some(first) = buckets_iter.next() else {
            return;
        };
        let mut current = *first;
        let mut current_value = scale(current.value, &metric.unit);
        let mut in_bucket_count: i64 = 0;
        let mut left_over = current.frequency;

        for &configured in buckets {
            let mut bucket_tags = tags.clone();
            bucket_tags.insert("le".to_string(), double_to_string(configured));

            if current_value <= configured {
                in_bucket_count += left_over;
                left_over = 0;
                while current_value <= configured {
                    match buckets_iter.next() {
                        Some(b) => {
                            current = *b;
                            current_value = scale(current.value, &metric.unit);
                            if current_value <= configured {
                                in_bucket_count += current.frequency;
                            } else {
                                left_over = current.frequency;
                            }
                        }
                        None => break,
                    }
                }
            }

            self.append_time_serie_value(
                name,
                &bucket_tags,
                &format_decimal(in_bucket_count as f64),
                "_bucket",
            );
        }

        for b in buckets_iter {
            left_over += b.frequency;
        }

        let mut inf_tags = tags.clone();
        inf_tags.insert("le".to_string(), "+Inf".to_string());
        self.append_time_serie_value(
            name,
            &inf_tags,
            &format_decimal((left_over + in_bucket_count) as f64),
            "_bucket",
        );
    }

    fn append_tags(&mut self, tags: &Tags) {
        let mut all = tags.clone();
        for (k, v) in &self.environment_tags {
            all.insert(k.clone(), v.clone());
        }
        if all.is_empty() {
            return;
        }
        self.push("{");
        let mut first = true;
        for (key, value) in &all {
            if !first {
                self.push(",");
            }
            first = false;
            self.push(&normalize_label_name(key));
            self.push("=\"");
            self.push(value);
            self.push("\"");
        }
        self.push("}");
    }
}

/// Group a slice by name, preserving first-seen order (Scala `groupBy` on small maps).
fn group_by_name<'a, T>(items: &'a [T], name: impl Fn(&T) -> &str) -> Vec<(String, Vec<&'a T>)> {
    let mut order: Vec<String> = Vec::new();
    let mut index: BTreeMap<String, usize> = BTreeMap::new();
    let mut groups: Vec<Vec<&'a T>> = Vec::new();
    for item in items {
        let k = name(item).to_string();
        match index.get(&k) {
            Some(&i) => groups[i].push(item),
            None => {
                index.insert(k.clone(), groups.len());
                order.push(k);
                groups.push(vec![item]);
            }
        }
    }
    order.into_iter().zip(groups).collect()
}

fn normalize_metric_name(metric_name: &str, unit: &MeasurementUnit) -> String {
    let normalized: String = metric_name.chars().map(char_or_underscore).collect();
    match unit.dimension {
        Dimension::Time => format!("{normalized}_seconds"),
        Dimension::Information => format!("{normalized}_bytes"),
        Dimension::None => normalized,
    }
}

fn normalize_label_name(label: &str) -> String {
    label.chars().map(char_or_underscore).collect()
}

fn char_or_underscore(c: char) -> char {
    if c.is_alphanumeric() || c == '_' {
        c
    } else {
        '_'
    }
}

/// `DecimalFormat("#0.0########")` with HALF_EVEN rounding (dot decimal, 1–9 fraction digits).
fn format_decimal(value: f64) -> String {
    let s = format!("{value:.9}");
    let trimmed = s.trim_end_matches('0');
    if trimmed.ends_with('.') {
        format!("{trimmed}0")
    } else {
        trimmed.to_string()
    }
}

/// Java `Double.toString` (append `.0` for integral doubles) — used for `le` bucket labels.
fn double_to_string(value: f64) -> String {
    let s = value.to_string();
    if s.contains('.') || s.contains('e') || s.contains('E') {
        s
    } else {
        format!("{s}.0")
    }
}

/// Scale a metric value into its base unit (seconds/bytes), port of `MeasurementUnit.scale`.
fn scale(value: i64, unit: &MeasurementUnit) -> f64 {
    match unit.dimension {
        Dimension::Time | Dimension::Information => value as f64 * unit.magnitude,
        Dimension::None => value as f64,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::model::{Bucket, Distribution, MetricDistribution, MetricValue, Tags};

    fn mv(name: &str, value: i64, unit: MeasurementUnit) -> MetricValue {
        MetricValue {
            name: name.to_string(),
            tags: Tags::new(),
            value,
            unit,
        }
    }

    #[test]
    fn counters_and_gauges_render() {
        let mut b = ScrapeDataBuilder::new(Configuration::default(), Tags::new());
        b.append_counters(&[mv("my.counter", 5, MeasurementUnit::NONE)])
            .append_gauges(&[mv("gauge.metric", 42, MeasurementUnit::NONE)]);

        let out = b.build();
        assert_eq!(
            out,
            "# TYPE my_counter_total counter\nmy_counter_total 5.0\n\
             # TYPE gauge_metric gauge\ngauge_metric 42.0\n"
        );
    }

    #[test]
    fn time_metric_gets_seconds_suffix_and_scaling() {
        // A metric in milliseconds (magnitude 0.001) is scaled to seconds.
        let ms = MeasurementUnit {
            dimension: Dimension::Time,
            magnitude: 0.001,
        };
        let mut b = ScrapeDataBuilder::new(Configuration::default(), Tags::new());
        b.append_counters(&[mv("latency", 5000, ms)]);
        assert_eq!(
            b.build(),
            "# TYPE latency_seconds_total counter\nlatency_seconds_total 5.0\n"
        );
    }

    #[test]
    fn histograms_render_buckets_count_sum() {
        let hist = MetricDistribution {
            name: "hist".to_string(),
            tags: Tags::new(),
            unit: MeasurementUnit::SECONDS,
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
        let mut config = Configuration::default();
        config.custom_buckets.insert("hist".to_string(), vec![1.0, 5.0]);

        let mut b = ScrapeDataBuilder::new(config, Tags::new());
        b.append_histograms(&[hist]);
        assert_eq!(
            b.build(),
            "# TYPE hist_seconds histogram\n\
             hist_seconds_bucket{le=\"1.0\"} 1.0\n\
             hist_seconds_bucket{le=\"5.0\"} 2.0\n\
             hist_seconds_bucket{le=\"+Inf\"} 3.0\n\
             hist_seconds_count 3.0\n\
             hist_seconds_sum 16.0\n"
        );
    }

    #[test]
    fn tags_are_sanitized_and_environment_tags_merged() {
        let mut tags = Tags::new();
        tags.insert("my.tag-name".to_string(), "value".to_string());
        let mut env = Tags::new();
        env.insert("env".to_string(), "prod".to_string());

        let mut b = ScrapeDataBuilder::new(Configuration::default(), env);
        b.append_counters(&[MetricValue {
            name: "c".to_string(),
            tags,
            value: 1,
            unit: MeasurementUnit::NONE,
        }]);
        assert_eq!(
            b.build(),
            "# TYPE c_total counter\nc_total{env=\"prod\",my_tag_name=\"value\"} 1.0\n"
        );
    }
}
