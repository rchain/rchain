//! Key-value store abstractions.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/store/{KeyValueStore,InMemoryKeyValueStore,
//! NoOpKeyValueStore}.scala`. The Scala `F[_]` effect and `ByteBuffer` zero-copy handling are
//! simplified to synchronous `Vec<u8>` operations; the async/effect model is reintroduced when
//! tokio lands. `iterate` becomes `entries` (eager).

use std::collections::BTreeMap;

/// A byte-oriented key-value store.
pub trait KeyValueStore {
    fn get(&self, keys: &[Vec<u8>]) -> Result<Vec<Option<Vec<u8>>>, String>;
    fn put(&mut self, pairs: Vec<(Vec<u8>, Vec<u8>)>) -> Result<(), String>;
    /// Delete the given keys, returning the number of keys that were actually present.
    fn delete(&mut self, keys: &[Vec<u8>]) -> Result<usize, String>;
    fn entries(&self) -> Result<Vec<(Vec<u8>, Vec<u8>)>, String>;

    /// Number of records currently stored. Defaults to an O(n) `entries` scan; implementations
    /// may override with a cheaper count.
    fn num_records(&self) -> usize {
        self.entries().map(|e| e.len()).unwrap_or(0)
    }
}

/// In-memory implementation (port of `InMemoryKeyValueStore`, using a `BTreeMap` for determinism).
#[derive(Default)]
pub struct InMemoryKeyValueStore {
    map: BTreeMap<Vec<u8>, Vec<u8>>,
}

impl InMemoryKeyValueStore {
    pub fn clear(&mut self) {
        self.map.clear();
    }
}

impl KeyValueStore for InMemoryKeyValueStore {
    fn get(&self, keys: &[Vec<u8>]) -> Result<Vec<Option<Vec<u8>>>, String> {
        Ok(keys.iter().map(|k| self.map.get(k).cloned()).collect())
    }

    fn put(&mut self, pairs: Vec<(Vec<u8>, Vec<u8>)>) -> Result<(), String> {
        for (k, v) in pairs {
            self.map.insert(k, v);
        }
        Ok(())
    }

    fn delete(&mut self, keys: &[Vec<u8>]) -> Result<usize, String> {
        Ok(keys.iter().filter(|k| self.map.remove(*k).is_some()).count())
    }

    fn entries(&self) -> Result<Vec<(Vec<u8>, Vec<u8>)>, String> {
        Ok(self.map.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
    }

    fn num_records(&self) -> usize {
        self.map.len()
    }
}

/// No-op implementation (port of `NoOpKeyValueStore`).
#[derive(Default)]
pub struct NoOpKeyValueStore;

impl KeyValueStore for NoOpKeyValueStore {
    fn get(&self, _keys: &[Vec<u8>]) -> Result<Vec<Option<Vec<u8>>>, String> {
        Ok(Vec::new())
    }

    fn put(&mut self, _pairs: Vec<(Vec<u8>, Vec<u8>)>) -> Result<(), String> {
        Ok(())
    }

    fn delete(&mut self, _keys: &[Vec<u8>]) -> Result<usize, String> {
        Ok(0)
    }

    fn entries(&self) -> Result<Vec<(Vec<u8>, Vec<u8>)>, String> {
        Ok(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn k(s: &str) -> Vec<u8> {
        s.as_bytes().to_vec()
    }

    #[test]
    fn get_put_delete_round_trip() {
        let mut store = InMemoryKeyValueStore::default();
        store.put(vec![(k("a"), vec![1]), (k("b"), vec![2])]).unwrap();
        assert_eq!(store.get(&[k("a"), k("b"), k("c")]).unwrap(), vec![Some(vec![1]), Some(vec![2]), None]);
        assert_eq!(store.delete(&[k("a"), k("c")]).unwrap(), 1);
        assert_eq!(store.get(&[k("a"), k("b")]).unwrap(), vec![None, Some(vec![2])]);
    }

    #[test]
    fn entries_returns_all_pairs() {
        let mut store = InMemoryKeyValueStore::default();
        store.put(vec![(k("b"), vec![2]), (k("a"), vec![1])]).unwrap();
        assert_eq!(store.entries().unwrap(), vec![(k("a"), vec![1]), (k("b"), vec![2])]);
    }

    #[test]
    fn no_op_store_is_empty() {
        let mut store = NoOpKeyValueStore;
        store.put(vec![(k("a"), vec![1])]).unwrap();
        assert_eq!(store.get(&[k("a")]).unwrap(), Vec::<Option<Vec<u8>>>::new());
        assert_eq!(store.delete(&[k("a")]).unwrap(), 0);
        assert!(store.entries().unwrap().is_empty());
    }
}
