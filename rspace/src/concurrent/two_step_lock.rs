//! Two-phase lock: acquire phase-A keys, compute phase-B keys while holding A, then acquire B.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/concurrent/TwoStepLock.scala`.

use std::future::Future;
use std::hash::Hash;

use crate::concurrent::multi_lock::MultiLock;
use crate::concurrent::BoxFuture;
use crate::errors::RSpaceError;

/// A two-phase lock (port of `TwoStepLock` / `ConcurrentTwoStepLockF`).
pub struct TwoStepLock<K> {
    phase_a: MultiLock<K>,
    phase_b: MultiLock<K>,
}

impl<K> TwoStepLock<K>
where
    K: Eq + Hash + Ord + Clone + Send + Sync + 'static,
{
    pub fn new() -> Self {
        TwoStepLock {
            phase_a: MultiLock::new(),
            phase_b: MultiLock::new(),
        }
    }

    /// Acquire `keys_a`, then run `phase_two` to compute `keys_b`, acquire those, then run `thunk`
    /// (port of `acquire`).
    pub async fn acquire<'a, F>(
        &'a self,
        keys_a: &[K],
        phase_two: BoxFuture<'a, std::result::Result<Vec<K>, RSpaceError>>,
        thunk: F,
    ) -> std::result::Result<F::Output, RSpaceError>
    where
        F: Future + Send + 'a,
    {
        self.phase_a
            .acquire(keys_a, async move {
                let keys_b = phase_two.await?;
                Ok(self.phase_b.acquire(&keys_b, thunk).await)
            })
            .await
    }
}

impl<K> Default for TwoStepLock<K>
where
    K: Eq + Hash + Ord + Clone + Send + Sync + 'static,
{
    fn default() -> Self {
        Self::new()
    }
}
