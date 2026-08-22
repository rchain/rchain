//! Lock a set of keys with ordered acquisition to avoid deadlock.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/concurrent/MultiLock.scala`. The per-key
//! `Semaphore[F]` becomes `Arc<tokio::sync::Mutex<()>>` in a `DashMap`.

use std::future::Future;
use std::hash::Hash;
use std::sync::Arc;

use dashmap::DashMap;

/// A set of per-key mutexes, acquired in sorted order (port of `MultiLock`).
pub struct MultiLock<K> {
    locks: DashMap<K, Arc<tokio::sync::Mutex<()>>>,
}

impl<K> MultiLock<K>
where
    K: Eq + Hash + Ord + Clone + Send + Sync + 'static,
{
    pub fn new() -> Self {
        MultiLock {
            locks: DashMap::new(),
        }
    }

    /// Acquire the locks for `keys` (sorted, deduped), run `thunk`, then release (port of `acquire`).
    pub async fn acquire<F>(&self, keys: &[K], thunk: F) -> F::Output
    where
        F: Future + Send,
    {
        let mut sorted: Vec<K> = keys.to_vec();
        sorted.sort();
        sorted.dedup();

        let mut arcs: Vec<Arc<tokio::sync::Mutex<()>>> = Vec::new();
        for key in sorted {
            let lock = match self.locks.get(&key) {
                Some(existing) => existing.clone(),
                None => {
                    let new = Arc::new(tokio::sync::Mutex::new(()));
                    self.locks.insert(key.clone(), new.clone());
                    new
                }
            };
            arcs.push(lock);
        }

        let mut guards = Vec::new();
        for lock in &arcs {
            guards.push(lock.lock().await);
        }

        let result = thunk.await;
        drop(guards);
        drop(arcs);
        result
    }
}

impl<K> Default for MultiLock<K>
where
    K: Eq + Hash + Ord + Clone + Send + Sync + 'static,
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[tokio::test]
    async fn acquire_runs_thunk() {
        let lock = MultiLock::<u8>::new();
        let value = lock.acquire(&[3, 1, 2], async { 42 }).await;
        assert_eq!(value, 42);
    }

    #[tokio::test]
    async fn acquire_serializes_conflicting_keys() {
        let lock = Arc::new(MultiLock::<u8>::new());
        let counter = Arc::new(AtomicUsize::new(0));

        let mut handles = Vec::new();
        for _ in 0..10 {
            let lock = lock.clone();
            let counter = counter.clone();
            handles.push(tokio::spawn(async move {
                lock.acquire(&[1u8], async {
                    let v = counter.load(Ordering::SeqCst);
                    counter.store(v + 1, Ordering::SeqCst);
                })
                .await;
            }));
        }
        for h in handles {
            h.await.unwrap();
        }
        assert_eq!(counter.load(Ordering::SeqCst), 10);
    }
}
