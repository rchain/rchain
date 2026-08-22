//! Kademlia routing table.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/discovery/PeerTable.scala`. The `F[_]` effect and
//! the `KademliaRPC` ping are dropped here — the bucket mutation is synchronous and the
//! oldest-peer ping eviction is deferred to the Kademlia RPC layer. The XOR distance (`dlut`) is
//! ported literally.

use std::cmp::Ordering;
use std::sync::Mutex;

use crate::peer_node::PeerNode;

/// Number of entries per bucket (the Scala `PeerTable.Redundancy`).
pub const REDUNDANCY: usize = 20;

/// Concurrency factor (the Scala `PeerTable.Alpha`).
pub const ALPHA: usize = 3;

/// A value exposing a byte key (port of the `Keyed` trait).
pub trait Keyed {
    fn key(&self) -> &[u8];
}

impl Keyed for PeerNode {
    fn key(&self) -> &[u8] {
        self.key()
    }
}

/// `dlut(n) = 7 - floor(log2 n)` for `n != 0`, i.e. the number of leading (most-significant) common
/// bits within a differing byte. This equals `n.leading_zeros()` and reproduces the literal Scala
/// `PeerTable.dlut` table (`Seq(0,7,6,6)` ++ `Seq.fill(4)(5)` ++ …).
#[inline]
fn dlut(n: u8) -> usize {
    n.leading_zeros() as usize
}

#[derive(Clone)]
struct PeerTableEntry<A> {
    entry: A,
    key: Vec<u8>,
    pinging: bool,
}

impl<A: Keyed> PeerTableEntry<A> {
    fn new(entry: A) -> Self {
        let key = entry.key().to_vec();
        PeerTableEntry {
            entry,
            key,
            pinging: false,
        }
    }
}

/// A Kademlia routing table (port of `PeerTable[A, F]`).
pub struct PeerTable<A: Keyed + Clone> {
    local_key: Vec<u8>,
    k: usize,
    width: usize,
    buckets: Vec<Mutex<Vec<PeerTableEntry<A>>>>,
}

impl<A: Keyed + Clone> PeerTable<A> {
    pub fn new(local_key: Vec<u8>, k: usize) -> Self {
        let width = local_key.len();
        let buckets = (0..8 * width).map(|_| Mutex::new(Vec::new())).collect();
        PeerTable {
            local_key,
            k,
            width,
            buckets,
        }
    }

    /// Kademlia XOR distance: the bit-length of the longest common prefix of `a` and `b` (higher is
    /// closer). Returns `None` if the keys are not both `width` bytes.
    pub fn distance(&self, a: &[u8], b: &[u8]) -> Option<usize> {
        if a.len() != self.width || b.len() != self.width {
            return None;
        }
        for idx in 0..self.width {
            let n = a[idx] ^ b[idx];
            if n != 0 {
                return Some(8 * idx + dlut(n));
            }
        }
        Some(8 * self.width)
    }

    /// Insert or refresh a peer in its bucket (port of `updateLastSeen`; the ping eviction is
    /// deferred to the Kademlia RPC layer).
    pub fn update_last_seen(&self, peer: A) {
        let Some(index) = self
            .distance(&self.local_key, peer.key())
            .filter(|&d| d < 8 * self.width)
        else {
            return;
        };
        let mut bucket = self.buckets[index].lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some(pos) = bucket.iter().position(|e| e.key == peer.key()) {
            bucket.remove(pos);
            bucket.push(PeerTableEntry::new(peer));
            return;
        }
        if bucket.len() < self.k {
            bucket.push(PeerTableEntry::new(peer));
        } else if let Some(candidate) = bucket.iter_mut().find(|e| !e.pinging) {
            // Oldest non-pinging entry is a ping candidate; the actual ping is deferred to the
            // Kademlia RPC layer.
            candidate.pinging = true;
        } else {
            // All entries are already pending a ping with no RPC to resolve them — evict the
            // least-recently-seen entry so a full bucket can never saturate permanently (M5).
            if !bucket.is_empty() {
                bucket.remove(0);
            }
            bucket.push(PeerTableEntry::new(peer));
        }
    }

    /// Remove the peer with the given key.
    pub fn remove(&self, key: &[u8]) {
        if let Some(index) = self.distance(&self.local_key, key) {
            if index < 8 * self.width {
                let mut bucket = self.buckets[index].lock().unwrap_or_else(|poisoned| poisoned.into_inner());
                if let Some(pos) = bucket.iter().position(|e| e.key == key) {
                    bucket.remove(pos);
                }
            }
        }
    }

    /// Return the `k` peers closest to `key`, sorted nearest-first (port of `lookup`).
    pub fn lookup(&self, key: &[u8]) -> Vec<A> {
        let Some(index) = self.distance(&self.local_key, key) else {
            return Vec::new();
        };
        let mut entries: Vec<PeerTableEntry<A>> = Vec::new();

        for i in index..(8 * self.width) {
            if entries.len() >= self.k {
                break;
            }
            let bucket = self.buckets[i].lock().unwrap_or_else(|poisoned| poisoned.into_inner());
            entries.extend(bucket.iter().filter(|e| e.key != key).cloned());
        }
        for i in (0..index).rev() {
            if entries.len() >= self.k {
                break;
            }
            let bucket = self.buckets[i].lock().unwrap_or_else(|poisoned| poisoned.into_inner());
            entries.extend(bucket.iter().cloned());
        }

        entries.sort_by(|a, b| match (self.distance(key, &a.key), self.distance(key, &b.key)) {
            (Some(d0), Some(d1)) => d1.cmp(&d0),
            _ => Ordering::Equal,
        });
        entries.into_iter().take(self.k).map(|e| e.entry).collect()
    }

    /// Return the peer named by `key`, if present (port of `find`).
    pub fn find(&self, key: &[u8]) -> Option<A> {
        let d = self.distance(&self.local_key, key)?;
        let bucket = self.buckets.get(d)?;
        bucket
            .lock().unwrap_or_else(|poisoned| poisoned.into_inner())
            .iter()
            .find(|e| e.key == key)
            .map(|e| e.entry.clone())
    }

    /// Return every peer in the table (port of `peers`).
    pub fn peers(&self) -> Vec<A> {
        self.buckets
            .iter()
            .flat_map(|b| {
                b.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
                    .iter()
                    .map(|e| e.entry.clone())
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    /// Return bucket indices ordered from least to most filled (port of `sparseness`).
    pub fn sparseness(&self) -> Vec<usize> {
        let mut indexed: Vec<(usize, usize)> = self
            .buckets
            .iter()
            .take(256)
            .enumerate()
            .map(|(i, b)| (b.lock().unwrap_or_else(|poisoned| poisoned.into_inner()).len(), i))
            .collect();
        indexed.sort_by(|a, b| a.0.cmp(&b.0));
        indexed.into_iter().map(|(_, i)| i).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(byte: u8) -> PeerNode {
        PeerNode::from(
            crate::peer_node::NodeIdentifier::new(vec![byte; 32]),
            "host".to_string(),
            rchain_shared::refined::Port::new(40400),
            rchain_shared::refined::Port::new(40404),
        )
    }

    #[test]
    fn distance_is_full_common_prefix_length() {
        let table: PeerTable<PeerNode> = PeerTable::new(vec![0u8; 32], REDUNDANCY);
        // Identical keys share all 256 bits.
        assert_eq!(table.distance(&[0u8; 32], &[0u8; 32]), Some(256));
        // Differ in the LSB of the first byte -> 7 leading common bits.
        assert_eq!(table.distance(&[0u8; 32], &[1u8; 32]), Some(7));
        // Differ in the MSB of the first byte -> 0 common bits.
        assert_eq!(table.distance(&[0u8; 32], &[0x80u8; 32]), Some(0));
        // Differ only in the second byte -> 8 common bits from byte 0, then 7 within byte 1.
        let mut b = [0u8; 32];
        b[1] = 1;
        assert_eq!(table.distance(&[0u8; 32], &b), Some(8 + 7));
    }

    #[test]
    fn distance_requires_equal_length() {
        let table: PeerTable<PeerNode> = PeerTable::new(vec![0u8; 32], REDUNDANCY);
        assert_eq!(table.distance(&[0u8; 31], &[0u8; 32]), None);
    }

    #[test]
    fn update_and_find() {
        let table: PeerTable<PeerNode> = PeerTable::new(vec![0u8; 32], REDUNDANCY);
        let peer = node(1);
        table.update_last_seen(peer.clone());
        assert_eq!(table.find(peer.key()).as_ref(), Some(&peer));
        assert_eq!(table.peers(), vec![peer]);
    }

    #[test]
    fn lookup_returns_closest_first() {
        let table: PeerTable<PeerNode> = PeerTable::new(vec![0u8; 32], REDUNDANCY);
        // Nodes that differ far away from the local key; lookup from the local key.
        let near = node(1); // distance 7
        let far = node(0x80); // distance 0
        table.update_last_seen(far.clone());
        table.update_last_seen(near.clone());
        let result = table.lookup(&[0u8; 32]);
        assert_eq!(result, vec![near, far]);
    }

    #[test]
    fn full_bucket_evicts_oldest_when_all_pinging() {
        // A small k=3 table; every peer uses a 0b01xxxxxx first byte so all share distance 1 and
        // land in the same bucket.
        let table: PeerTable<PeerNode> = PeerTable::new(vec![0u8; 32], 3);
        let a = node(0x40);
        let b = node(0x41);
        let c = node(0x42);
        let d = node(0x43);
        table.update_last_seen(a.clone());
        table.update_last_seen(b.clone());
        table.update_last_seen(c.clone());
        // Bucket full; each new peer marks the oldest non-pinging entry as pinging.
        table.update_last_seen(node(0x48));
        table.update_last_seen(node(0x49));
        table.update_last_seen(node(0x4A));
        // All entries now pinging; the next peer evicts the oldest (a) to make room.
        table.update_last_seen(d.clone());
        assert_eq!(table.find(a.key()), None);
        assert_eq!(table.find(d.key()).as_ref(), Some(&d));
    }
}
