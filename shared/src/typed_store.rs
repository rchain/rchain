//! Typed key-value store + codec bridge.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/store/{KeyValueTypedStore,KeyValueTypedStoreCodec,
//! KeyValueStoreSyntax}.scala`. The Scala `scodec.Codec[K]`/`Codec[V]` become a `Codec<T>` trait
//! (encode to / decode from `Vec<u8>`), and the `F[_]` effect becomes an `async_trait` boxed future
//! over the synchronous byte `KeyValueStore` (see [`crate::store`]).

use std::collections::BTreeMap;
use std::sync::Arc;

use async_trait::async_trait;

use crate::store::KeyValueStore;

/// A key/value codec (port of the `scodec.Codec` restriction used by the typed store).
pub trait Codec<T>: Send + Sync {
    fn encode(&self, value: &T) -> Vec<u8>;
    fn decode(&self, bytes: &[u8]) -> Result<T, String>;
}

/// A shared, mutable byte store (the Scala `TrieMap`/`Ref` analogue).
pub type SharedStore = Arc<tokio::sync::Mutex<Box<dyn KeyValueStore + Send + Sync>>>;

/// A typed key-value store (port of `KeyValueTypedStore[F, K, V]`).
#[async_trait]
pub trait KeyValueTypedStore<K, V>: Send + Sync {
    async fn get(&self, keys: &[K]) -> Result<Vec<Option<V>>, String>;
    async fn put(&self, pairs: &[(K, V)]) -> Result<(), String>;
    async fn delete(&self, keys: &[K]) -> Result<usize, String>;
    async fn contains(&self, keys: &[K]) -> Result<Vec<bool>, String>;
    async fn to_map(&self) -> Result<BTreeMap<K, V>, String>;

    /// Number of records stored. Defaults to `to_map().len()`; the codec-backed implementation
    /// overrides this with a cheaper `num_records` on the underlying byte store.
    async fn count(&self) -> Result<usize, String> {
        Ok(self.to_map().await?.len())
    }
}

/// A typed store over a byte store with key/value codecs (port of `KeyValueTypedStoreCodec`).
pub struct KeyValueTypedStoreCodec<K, V> {
    store: SharedStore,
    k_codec: Arc<dyn Codec<K>>,
    v_codec: Arc<dyn Codec<V>>,
}

impl<K, V> KeyValueTypedStoreCodec<K, V> {
    pub fn new(store: SharedStore, k_codec: Arc<dyn Codec<K>>, v_codec: Arc<dyn Codec<V>>) -> Self {
        Self {
            store,
            k_codec,
            v_codec,
        }
    }
}

#[async_trait]
impl<K, V> KeyValueTypedStore<K, V> for KeyValueTypedStoreCodec<K, V>
where
    K: Clone + Ord + Send + Sync,
    V: Clone + Send + Sync,
{
    async fn get(&self, keys: &[K]) -> Result<Vec<Option<V>>, String> {
        let encoded: Vec<Vec<u8>> = keys.iter().map(|k| self.k_codec.encode(k)).collect();
        let store = self.store.clone();
        let raw = tokio::task::spawn_blocking(move || {
            let store = store.blocking_lock();
            store.get(&encoded)
        })
        .await
        .map_err(|e| e.to_string())??;
        raw.into_iter()
            .map(|opt| opt.map(|bytes| self.v_codec.decode(&bytes)).transpose())
            .collect()
    }

    async fn put(&self, pairs: &[(K, V)]) -> Result<(), String> {
        let encoded: Vec<(Vec<u8>, Vec<u8>)> = pairs
            .iter()
            .map(|(k, v)| (self.k_codec.encode(k), self.v_codec.encode(v)))
            .collect();
        let store = self.store.clone();
        tokio::task::spawn_blocking(move || {
            let mut store = store.blocking_lock();
            store.put(encoded)
        })
        .await
        .map_err(|e| e.to_string())?
    }

    async fn delete(&self, keys: &[K]) -> Result<usize, String> {
        let encoded: Vec<Vec<u8>> = keys.iter().map(|k| self.k_codec.encode(k)).collect();
        let store = self.store.clone();
        tokio::task::spawn_blocking(move || {
            let mut store = store.blocking_lock();
            store.delete(&encoded)
        })
        .await
        .map_err(|e| e.to_string())?
    }

    async fn contains(&self, keys: &[K]) -> Result<Vec<bool>, String> {
        let encoded: Vec<Vec<u8>> = keys.iter().map(|k| self.k_codec.encode(k)).collect();
        let store = self.store.clone();
        let raw = tokio::task::spawn_blocking(move || {
            let store = store.blocking_lock();
            store.get(&encoded)
        })
        .await
        .map_err(|e| e.to_string())??;
        Ok(raw.into_iter().map(|opt| opt.is_some()).collect())
    }

    async fn to_map(&self) -> Result<BTreeMap<K, V>, String> {
        let store = self.store.clone();
        let raw = tokio::task::spawn_blocking(move || {
            let store = store.blocking_lock();
            store.entries()
        })
        .await
        .map_err(|e| e.to_string())??;
        raw.into_iter()
            .map(|(k, v)| {
                let k = self.k_codec.decode(&k)?;
                let v = self.v_codec.decode(&v)?;
                Ok((k, v))
            })
            .collect()
    }

    async fn count(&self) -> Result<usize, String> {
        let store = self.store.clone();
        tokio::task::spawn_blocking(move || {
            let store = store.blocking_lock();
            store.num_records()
        })
        .await
        .map_err(|e| e.to_string())
    }
}

/// A `Codec` for `Vec<u8>` (identity).
#[derive(Default)]
pub struct BytesCodec;

impl Codec<Vec<u8>> for BytesCodec {
    fn encode(&self, value: &Vec<u8>) -> Vec<u8> {
        value.clone()
    }

    fn decode(&self, bytes: &[u8]) -> Result<Vec<u8>, String> {
        Ok(bytes.to_vec())
    }
}

/// A `Codec` for UTF-8 strings.
#[derive(Default)]
pub struct StringCodec;

impl Codec<String> for StringCodec {
    fn encode(&self, value: &String) -> Vec<u8> {
        value.as_bytes().to_vec()
    }

    fn decode(&self, bytes: &[u8]) -> Result<String, String> {
        String::from_utf8(bytes.to_vec()).map_err(|e| e.to_string())
    }
}

/// A big-endian `Codec` for `i64`.
#[derive(Default)]
pub struct I64Codec;

impl Codec<i64> for I64Codec {
    fn encode(&self, value: &i64) -> Vec<u8> {
        value.to_be_bytes().to_vec()
    }

    fn decode(&self, bytes: &[u8]) -> Result<i64, String> {
        let arr: [u8; 8] = bytes.try_into().map_err(|_| "expected 8 bytes".to_string())?;
        Ok(i64::from_be_bytes(arr))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::store::InMemoryKeyValueStore;

    fn in_memory() -> SharedStore {
        Arc::new(tokio::sync::Mutex::new(Box::new(
            InMemoryKeyValueStore::default(),
        ) as Box<dyn KeyValueStore + Send + Sync>))
    }

    #[tokio::test]
    async fn in_memory_round_trip() {
        let codec = KeyValueTypedStoreCodec::new(
            in_memory(),
            Arc::new(StringCodec),
            Arc::new(I64Codec),
        );
        codec.put(&[("a".to_string(), 1), ("b".to_string(), 2)]).await.unwrap();
        let vals = codec.get(&["a".to_string(), "b".to_string(), "c".to_string()]).await.unwrap();
        assert_eq!(vals, vec![Some(1), Some(2), None]);
        assert_eq!(codec.contains(&["a".to_string(), "c".to_string()]).await.unwrap(), vec![true, false]);
        assert_eq!(codec.delete(&["a".to_string(), "c".to_string()]).await.unwrap(), 1);
        assert_eq!(codec.to_map().await.unwrap(), BTreeMap::from([("b".to_string(), 2)]));
    }
}
