//! Lazy key-value caches (port of `store/LazyKeyValueCache.scala` +
//! `store/LazyAdHocKeyValueCache.scala`).
//!
//! The cats-effect `Deferred`/`Ref` are simplified to a `Mutex`-guarded map; the "populate at most
//! once per key" semantics are preserved.

use std::collections::BTreeMap;
use std::sync::{Mutex, MutexGuard};

/// A key-value cache (port of `KeyValueCache[F, K, V]`).
pub trait KeyValueCache<K, V> {
    fn get(&self, key: K, fallback: impl FnOnce() -> V) -> V;

    fn put(&self, key: K, value: V);

    fn to_map(&self) -> BTreeMap<K, V>;
}

/// A no-op cache that always evaluates the fallback (port of `NoOpKeyValueCache`).
#[derive(Default)]
pub struct NoOpKeyValueCache;

impl<K, V> KeyValueCache<K, V> for NoOpKeyValueCache {
    fn get(&self, _key: K, fallback: impl FnOnce() -> V) -> V {
        fallback()
    }

    fn put(&self, _key: K, _value: V) {}

    fn to_map(&self) -> BTreeMap<K, V> {
        BTreeMap::new()
    }
}

/// A cache that populates a value at most once per key (port of `LazyAdHocKeyValueCache`).
pub struct LazyAdHocKeyValueCache<K: Ord, V> {
    cache: Mutex<BTreeMap<K, V>>,
}

impl<K: Ord, V> LazyAdHocKeyValueCache<K, V> {
    pub fn new() -> Self {
        LazyAdHocKeyValueCache {
            cache: Mutex::new(BTreeMap::new()),
        }
    }
}

impl<K: Ord + Clone, V: Clone> KeyValueCache<K, V> for LazyAdHocKeyValueCache<K, V> {
    fn get(&self, key: K, fallback: impl FnOnce() -> V) -> V {
        let mut cache = lock(&self.cache);
        if let Some(v) = cache.get(&key) {
            return v.clone();
        }
        let v = fallback();
        cache.insert(key, v.clone());
        v
    }

    fn put(&self, key: K, value: V) {
        let _ = self.get(key, || value.clone());
    }

    fn to_map(&self) -> BTreeMap<K, V> {
        lock(&self.cache).clone()
    }
}

/// A cache with a fixed populate function (port of `LazyKeyValueCache`).
pub struct LazyKeyValueCache<K: Ord, V, F: Fn(&K) -> V> {
    cache: Mutex<BTreeMap<K, V>>,
    populate: F,
}

impl<K: Ord + Clone, V: Clone, F: Fn(&K) -> V> LazyKeyValueCache<K, V, F> {
    pub fn new(populate: F) -> Self {
        LazyKeyValueCache {
            cache: Mutex::new(BTreeMap::new()),
            populate,
        }
    }

    pub fn get(&self, key: K) -> V {
        let mut cache = lock(&self.cache);
        if let Some(v) = cache.get(&key) {
            return v.clone();
        }
        let v = (self.populate)(&key);
        cache.insert(key, v.clone());
        v
    }

    pub fn to_map(&self) -> BTreeMap<K, V> {
        lock(&self.cache).clone()
    }
}

fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|p| p.into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    #[test]
    fn lazy_adhoc_populates_once_per_key() {
        let cache = LazyAdHocKeyValueCache::<i32, i32>::new();
        let mut calls = 0;
        let a = cache.get(1, || {
            calls += 1;
            10
        });
        let b = cache.get(1, || {
            calls += 1;
            20
        });
        assert_eq!(a, 10);
        assert_eq!(b, 10);
        assert_eq!(calls, 1);
    }

    #[test]
    fn lazy_adhoc_put_and_to_map() {
        let cache = LazyAdHocKeyValueCache::<i32, i32>::new();
        cache.put(1, 10);
        cache.put(2, 20);
        let map = cache.to_map();
        assert_eq!(map.len(), 2);
        assert_eq!(map[&1], 10);
        assert_eq!(map[&2], 20);
    }

    #[test]
    fn noop_always_evaluates_fallback() {
        let cache = NoOpKeyValueCache;
        let v = cache.get(1, || 42);
        assert_eq!(v, 42);
        let map: BTreeMap<i32, i32> = cache.to_map();
        assert!(map.is_empty());
    }

    #[test]
    fn lazy_key_value_populates_once() {
        let calls = Arc::new(AtomicUsize::new(0));
        let cache = LazyKeyValueCache::new({
            let calls = Arc::clone(&calls);
            move |k: &i32| {
                calls.fetch_add(1, Ordering::SeqCst);
                k * 10
            }
        });
        assert_eq!(cache.get(3), 30);
        assert_eq!(cache.get(3), 30);
        assert_eq!(calls.load(Ordering::SeqCst), 1);
        assert_eq!(cache.to_map()[&3], 30);
    }
}
