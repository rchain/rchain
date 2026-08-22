//! Keyed in-memory packet cache (store/restore).
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/PacketOps.scala`. The Scala `TrieMap`
//! cache becomes a `HashMap<String, Vec<u8>>` (the same cache shape used by `StreamHandler`).

use std::collections::HashMap;

use prost::Message;
use rchain_models::comm::protocol::Packet;

use crate::errors::{CommErr, CommError};

/// The shared streamed-packet cache (port of `TrieMap[String, Array[Byte]]`).
pub type PacketCache = HashMap<String, Vec<u8>>;

/// Decode a stored packet by key (port of `PacketOps.restore`).
pub fn restore(key: &str, cache: &PacketCache) -> CommErr<Packet> {
    cache
        .get(key)
        .ok_or_else(|| CommError::UnableToRestorePacket(key.to_string()))
        .and_then(|bytes| {
            Packet::decode(bytes.as_slice())
                .map_err(|_| CommError::UnableToRestorePacket(key.to_string()))
        })
}

/// Store a packet under a fresh key and return the key (port of `RichPacket.store`).
pub fn store(packet: &Packet, cache: &mut PacketCache) -> CommErr<String> {
    let key = create_cache_entry("packet_receive/", cache);
    cache.insert(key.clone(), packet.encode_to_vec());
    Ok(key)
}

/// Reserve a cache key with empty data and return it (port of `PacketOps.createCacheEntry`).
pub fn create_cache_entry(prefix: &str, cache: &mut PacketCache) -> String {
    let key = format!("{prefix}/{}", timestamp());
    cache.insert(key.clone(), Vec::new());
    key
}

/// `yyyyMMddHHmmss_<8-hex>` timestamp (port of `PacketOps.timestamp`).
fn timestamp() -> String {
    let date = chrono::Local::now().format("%Y%m%d%H%M%S").to_string();
    let bytes: [u8; 4] = rand::random();
    let hex = rchain_shared::base16::encode(&bytes);
    format!("{date}_{hex}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_models::comm::protocol::Packet;

    #[test]
    fn store_then_restore_round_trips() {
        let packet = Packet {
            type_id: "BlockMessage".to_string(),
            content: vec![1, 2, 3, 4, 5],
        };
        let mut cache = PacketCache::new();
        let key = store(&packet, &mut cache).unwrap();
        assert_eq!(restore(&key, &cache).unwrap(), packet);
    }

    #[test]
    fn restore_missing_key_errors() {
        let cache = PacketCache::new();
        assert!(matches!(
            restore("missing", &cache),
            Err(CommError::UnableToRestorePacket(_))
        ));
    }
}
