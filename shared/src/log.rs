//! Logging facade.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/shared/Log.scala`. The Scala `F[_]` effect (a
//! `Sync[F].delay` around slf4j) is simplified to synchronous calls, matching the crate's sync
//! `store` convention. The no-op instance (port of `Log.NOPLog`) is the load-bearing piece here.

/// Identifies the source class of a log message (port of `LogSource`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LogSource {
    pub class_name: &'static str,
}

impl LogSource {
    pub const fn new(class_name: &'static str) -> Self {
        Self { class_name }
    }
}

/// A logger (port of `Log[F]`).
pub trait Log: Send + Sync {
    fn is_trace_enabled(&self, source: LogSource) -> bool;
    fn trace(&self, source: LogSource, msg: &str);
    fn debug(&self, source: LogSource, msg: &str);
    fn info(&self, source: LogSource, msg: &str);
    fn warn(&self, source: LogSource, msg: &str);
    fn error(&self, source: LogSource, msg: &str);
}

/// No-op logger (port of `Log.NOPLog`).
#[derive(Default)]
pub struct NopLog;

impl Log for NopLog {
    fn is_trace_enabled(&self, _source: LogSource) -> bool {
        false
    }
    fn trace(&self, _source: LogSource, _msg: &str) {}
    fn debug(&self, _source: LogSource, _msg: &str) {}
    fn info(&self, _source: LogSource, _msg: &str) {}
    fn warn(&self, _source: LogSource, _msg: &str) {}
    fn error(&self, _source: LogSource, _msg: &str) {}
}

/// A logger writing to stderr (a concrete `Log` for tests and CLI use).
#[derive(Default)]
pub struct StderrLog;

impl Log for StderrLog {
    fn is_trace_enabled(&self, _source: LogSource) -> bool {
        false
    }
    fn trace(&self, _source: LogSource, _msg: &str) {}
    fn debug(&self, _source: LogSource, _msg: &str) {}
    fn info(&self, source: LogSource, msg: &str) {
        eprintln!("INFO  [{}] {}", source.class_name, msg);
    }
    fn warn(&self, source: LogSource, msg: &str) {
        eprintln!("WARN  [{}] {}", source.class_name, msg);
    }
    fn error(&self, source: LogSource, msg: &str) {
        eprintln!("ERROR [{}] {}", source.class_name, msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nop_log_never_traces() {
        let log = NopLog;
        let source = LogSource::new("test");
        assert!(!log.is_trace_enabled(source));
        log.error(source, "ignored");
    }
}
