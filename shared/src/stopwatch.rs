//! Timing utilities (port of `shared/Stopwatch.scala`).
//!
//! The cats-effect `Sync[F]` is simplified to synchronous calls; the logging callback is a plain
//! `FnMut(String)`.

use std::time::{Duration, Instant};

/// Format a duration (in nanoseconds) as a human-readable string (port of `Stopwatch.showTime`).
pub fn show_time(nanos: i64) -> String {
    let m = nanos as f64;
    let hour = 3_600_000_000_000.0_f64;
    let min = 60_000_000_000.0_f64;
    let sec = 1_000_000_000.0_f64;
    let ms = 1_000_000.0_f64;
    if m >= hour {
        format!("{} hour", double_to_string(m / hour))
    } else if m >= min {
        format!("{} min", double_to_string(m / min))
    } else if m >= sec {
        format!("{} sec", double_to_string(m / sec))
    } else {
        format!("{} ms", double_to_string(m / ms))
    }
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

/// Time a block, logging the elapsed duration (port of `Stopwatch.time`).
pub fn time<A>(mut log: impl FnMut(String), tag: &str, block: impl FnOnce() -> A) -> A {
    let t0 = Instant::now();
    let a = block();
    log(format!("{} [{}]", tag, show_time(t0.elapsed().as_nanos() as i64)));
    a
}

/// Time a block, returning its value and the raw elapsed duration (port of
/// `Stopwatch.durationRaw`).
pub fn duration_raw<A>(block: impl FnOnce() -> A) -> (A, Duration) {
    let t0 = Instant::now();
    let a = block();
    (a, t0.elapsed())
}

/// Time a block, returning its value and the formatted elapsed duration (port of
/// `Stopwatch.duration`).
pub fn duration<A>(block: impl FnOnce() -> A) -> (A, String) {
    let (a, d) = duration_raw(block);
    (a, show_time(d.as_nanos() as i64))
}

/// Time a by-name block, returning its value and the formatted elapsed duration (port of
/// `Stopwatch.profile`).
pub fn profile<A>(block: impl FnOnce() -> A) -> (A, String) {
    let t0 = Instant::now();
    let a = block();
    (a, show_time(t0.elapsed().as_nanos() as i64))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn show_time_formats_units() {
        assert_eq!(show_time(3_600_000_000_000), "1.0 hour");
        assert_eq!(show_time(60_000_000_000), "1.0 min");
        assert_eq!(show_time(1_000_000_000), "1.0 sec");
        assert_eq!(show_time(5_000_000), "5.0 ms");
        assert_eq!(show_time(500_000), "0.5 ms");
    }

    #[test]
    fn show_time_uses_fractional_boundaries() {
        assert_eq!(show_time(5_400_000_000_000), "1.5 hour");
        assert_eq!(show_time(90_000_000_000), "1.5 min");
        assert_eq!(show_time(1_500_000_000), "1.5 sec");
    }

    #[test]
    fn profile_returns_value_and_duration() {
        let (v, s) = profile(|| 42);
        assert_eq!(v, 42);
        assert!(s.ends_with(" ms"));
    }

    #[test]
    fn duration_raw_returns_value_and_duration() {
        let (v, _d) = duration_raw(|| "x".to_string());
        assert_eq!(v, "x");
    }

    #[test]
    fn time_logs_and_returns_value() {
        let mut logs = Vec::new();
        let v = time(|msg| logs.push(msg), "work", || 7);
        assert_eq!(v, 7);
        assert_eq!(logs.len(), 1);
        assert!(logs[0].starts_with("work ["));
        assert!(logs[0].ends_with(']'));
    }
}
