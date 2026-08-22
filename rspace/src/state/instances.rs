//! Store-backed exporter/importer/state-manager instances (port of
//! `RSpaceExporterStore`/`RSpaceImporterStore`/`RSpaceStateManagerImpl`).

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::state::{StateManager, TrieExporter, TrieImporter, TrieNode};
use rchain_shared::store::KeyValueStore;

use crate::state::{traverse_history, RSpaceExporter, RSpaceImporter, RSpaceStateManager};

const CURRENT_ROOT: &[u8] = b"current-root";
const ROOT_TAG: &[u8] = b"root";

fn hash_bytes(h: &Blake2b256Hash) -> Vec<u8> {
    h.to_byte_array().to_vec()
}

fn get_items<Value>(
    store: &dyn KeyValueStore,
    keys: &[Blake2b256Hash],
    from_buffer: &dyn Fn(&[u8]) -> Value,
) -> Vec<(Blake2b256Hash, Value)> {
    let key_bytes: Vec<Vec<u8>> = keys.iter().map(hash_bytes).collect();
    let loaded = store.get(&key_bytes).unwrap_or_default();
    keys.iter()
        .zip(loaded)
        .filter_map(|(h, v)| v.map(|b| (*h, from_buffer(&b))))
        .collect()
}

/// An exporter over three byte stores: history (branch nodes), value (leaf nodes), roots (port of
/// `RSpaceExporterStore`).
pub struct RSpaceExporterStore {
    history_store: Box<dyn KeyValueStore + Send + Sync>,
    value_store: Box<dyn KeyValueStore + Send + Sync>,
    roots_store: Box<dyn KeyValueStore + Send + Sync>,
}

impl RSpaceExporterStore {
    pub fn new(
        history_store: Box<dyn KeyValueStore + Send + Sync>,
        value_store: Box<dyn KeyValueStore + Send + Sync>,
        roots_store: Box<dyn KeyValueStore + Send + Sync>,
    ) -> Self {
        RSpaceExporterStore {
            history_store,
            value_store,
            roots_store,
        }
    }
}

impl TrieExporter<Blake2b256Hash> for RSpaceExporterStore {
    fn get_nodes(
        &self,
        start_path: &[(Blake2b256Hash, Option<u8>)],
        skip: usize,
        take: usize,
    ) -> Vec<TrieNode<Blake2b256Hash>> {
        let history_store: &dyn KeyValueStore = self.history_store.as_ref();
        let get_node = |h: &Blake2b256Hash| -> Option<Vec<u8>> {
            history_store
                .get(&[hash_bytes(h)])
                .unwrap_or_default()
                .into_iter()
                .next()
                .flatten()
        };
        traverse_history(start_path, skip as i32, take as i32, &get_node).unwrap_or_default()
    }

    fn get_history_items<Value>(
        &self,
        keys: &[Blake2b256Hash],
        from_buffer: impl Fn(&[u8]) -> Value,
    ) -> Vec<(Blake2b256Hash, Value)> {
        get_items(self.history_store.as_ref(), keys, &from_buffer)
    }

    fn get_data_items<Value>(
        &self,
        keys: &[Blake2b256Hash],
        from_buffer: impl Fn(&[u8]) -> Value,
    ) -> Vec<(Blake2b256Hash, Value)> {
        get_items(self.value_store.as_ref(), keys, &from_buffer)
    }
}

impl RSpaceExporter for RSpaceExporterStore {
    fn get_root(&self) -> Option<Blake2b256Hash> {
        self.roots_store
            .get(&[CURRENT_ROOT.to_vec()])
            .unwrap_or_default()
            .into_iter()
            .next()
            .flatten()
            .map(|b| Blake2b256Hash::from_byte_array(&b))
    }
}

/// An importer over three byte stores (port of `RSpaceImporterStore`).
pub struct RSpaceImporterStore {
    history_store: Box<dyn KeyValueStore + Send + Sync>,
    value_store: Box<dyn KeyValueStore + Send + Sync>,
    roots_store: Box<dyn KeyValueStore + Send + Sync>,
}

impl RSpaceImporterStore {
    pub fn new(
        history_store: Box<dyn KeyValueStore + Send + Sync>,
        value_store: Box<dyn KeyValueStore + Send + Sync>,
        roots_store: Box<dyn KeyValueStore + Send + Sync>,
    ) -> Self {
        RSpaceImporterStore {
            history_store,
            value_store,
            roots_store,
        }
    }
}

impl TrieImporter<Blake2b256Hash> for RSpaceImporterStore {
    fn set_history_items<Value>(
        &mut self,
        data: &[(Blake2b256Hash, Value)],
        to_buffer: impl Fn(&Value) -> Vec<u8>,
    ) {
        let pairs: Vec<(Vec<u8>, Vec<u8>)> = data
            .iter()
            .map(|(h, v)| (hash_bytes(h), to_buffer(v)))
            .collect();
        let _ = self.history_store.put(pairs);
    }

    fn set_data_items<Value>(
        &mut self,
        data: &[(Blake2b256Hash, Value)],
        to_buffer: impl Fn(&Value) -> Vec<u8>,
    ) {
        let pairs: Vec<(Vec<u8>, Vec<u8>)> = data
            .iter()
            .map(|(h, v)| (hash_bytes(h), to_buffer(v)))
            .collect();
        let _ = self.value_store.put(pairs);
    }

    fn set_root(&mut self, key: Blake2b256Hash) {
        let bytes = hash_bytes(&key);
        let _ = self.roots_store.put(vec![(bytes.clone(), ROOT_TAG.to_vec())]);
        let _ = self.roots_store.put(vec![(CURRENT_ROOT.to_vec(), bytes)]);
    }
}

impl RSpaceImporter for RSpaceImporterStore {
    fn get_history_item(&self, hash: Blake2b256Hash) -> Option<Vec<u8>> {
        self.history_store
            .get(&[hash_bytes(&hash)])
            .unwrap_or_default()
            .into_iter()
            .next()
            .flatten()
    }
}

/// The state manager pairing an exporter and importer (port of `RSpaceStateManagerImpl`).
pub struct RSpaceStateManagerImpl<E: RSpaceExporter, I: RSpaceImporter> {
    exporter: E,
    importer: I,
}

impl<E: RSpaceExporter, I: RSpaceImporter> RSpaceStateManagerImpl<E, I> {
    pub fn new(exporter: E, importer: I) -> Self {
        RSpaceStateManagerImpl { exporter, importer }
    }
}

impl<E: RSpaceExporter, I: RSpaceImporter> StateManager for RSpaceStateManagerImpl<E, I> {
    fn is_empty(&self) -> bool {
        self.exporter.get_root().is_none()
    }
}

impl<E: RSpaceExporter, I: RSpaceImporter> RSpaceStateManager for RSpaceStateManagerImpl<E, I> {
    type Exporter = E;
    type Importer = I;

    fn exporter(&self) -> &E {
        &self.exporter
    }

    fn importer(&self) -> &I {
        &self.importer
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::store::InMemoryKeyValueStore;

    fn store() -> Box<dyn KeyValueStore + Send + Sync> {
        Box::new(InMemoryKeyValueStore::default())
    }

    #[test]
    fn exporter_reads_root_and_items() {
        let mut roots = InMemoryKeyValueStore::default();
        let root = Blake2b256Hash::from_bytes([0x11; 32]);
        roots
            .put(vec![(CURRENT_ROOT.to_vec(), root.to_byte_array().to_vec())])
            .unwrap();

        let exporter = RSpaceExporterStore::new(store(), store(), Box::new(roots));
        assert_eq!(exporter.get_root(), Some(root));
    }

    #[test]
    fn state_manager_empty_when_no_root() {
        let exporter = RSpaceExporterStore::new(store(), store(), store());
        let importer = RSpaceImporterStore::new(store(), store(), store());
        let manager = RSpaceStateManagerImpl::new(exporter, importer);
        assert!(manager.is_empty());
    }

    #[test]
    fn importer_sets_root() {
        let mut importer = RSpaceImporterStore::new(store(), store(), store());
        let root = Blake2b256Hash::from_bytes([0x22; 32]);
        importer.set_root(root);
        assert_eq!(importer.get_history_item(root), None);
        // root is recorded under "current-root", not as a history item
        let roots = &*importer.roots_store;
        let current = roots
            .get(&[CURRENT_ROOT.to_vec()])
            .unwrap()
            .into_iter()
            .next()
            .flatten()
            .unwrap();
        assert_eq!(current, root.to_byte_array().to_vec());
    }
}
