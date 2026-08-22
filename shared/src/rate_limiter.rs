//! A minimal fixed-window rate limiter.
//!
//! Shared by the unauthenticated deploy gRPC/HTTP servers and the Kademlia discovery RPC to bound
//! request rate (documented Scala deviations: those surfaces are unlimited in Scala).

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::{Duration, Instant};

/// A minimal fixed-window rate limiter (bounded requests per second).
pub struct RateLimiter {
    max_per_sec: u64,
    window_start: Mutex<Instant>,
    count: AtomicU64,
}

impl RateLimiter {
    pub fn new(max_per_sec: u64) -> Self {
        RateLimiter {
            max_per_sec,
            window_start: Mutex::new(Instant::now()),
            count: AtomicU64::new(0),
        }
    }

    /// Admit a request if the current one-second window has capacity.
    pub fn allow(&self) -> bool {
        let now = Instant::now();
        let mut start = self.window_start.lock().unwrap_or_else(|p| p.into_inner());
        if now.duration_since(*start) >= Duration::from_secs(1) {
            *start = now;
            self.count.store(0, Ordering::SeqCst);
        }
        self.count.fetch_add(1, Ordering::SeqCst) < self.max_per_sec
    }
}
