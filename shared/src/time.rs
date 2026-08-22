//! Time utilities (port of the sync half of `shared/Time.scala`).
//!
//! The cats-effect `Time[F]`/`Timer[F]` abstraction is simplified to plain functions.

use std::sync::OnceLock;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

/// Current epoch time in milliseconds (port of `Time.currentMillis`).
pub fn current_millis() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64
}

/// Monotonic time in nanoseconds since process start (port of `Time.nanoTime`).
pub fn nano_time() -> i64 {
    static START: OnceLock<Instant> = OnceLock::new();
    START.get_or_init(Instant::now).elapsed().as_nanos() as i64
}

/// Sleep for the given duration (port of `Time.sleep`).
pub fn sleep(duration: Duration) {
    std::thread::sleep(duration);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn current_millis_is_epoch_based() {
        // Sanity: any post-2017 epoch-millis value.
        assert!(current_millis() > 1_500_000_000_000);
    }

    #[test]
    fn nano_time_is_monotonic() {
        let a = nano_time();
        let b = nano_time();
        assert!(b >= a);
    }

    #[test]
    fn sleep_blocks_for_at_least_the_duration() {
        let start = Instant::now();
        sleep(Duration::from_millis(5));
        assert!(start.elapsed() >= Duration::from_millis(5));
    }
}
