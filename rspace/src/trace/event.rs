//! Trace events: `Produce`, `Consume`, and the deterministic `COMM` (Law 8).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/trace/Event.scala`. `Produce`/`Consume` are
//! content-addressed: equality is by `hash` only (matching the Scala `equals`/`hashCode`).

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::serialize::Serialize;

use crate::hashing::stable_hash_provider::{hash_channel, hash_consume, hash_produce, hash_seq};
use crate::internal::ConsumeCandidate;

/// A produce event (port of `Produce`).
#[derive(Clone, Debug)]
pub struct Produce {
    pub channels_hash: Blake2b256Hash,
    pub hash: Blake2b256Hash,
    pub persistent: bool,
}

impl Produce {
    pub fn apply<C, A>(channel: &C, datum: &A, persistent: bool) -> Self
    where
        C: Serialize<C>,
        A: Serialize<A>,
    {
        let channel_hash = hash_channel(channel);
        let hash = hash_produce(channel_hash.as_bytes(), datum, persistent);
        Produce {
            channels_hash: channel_hash,
            hash,
            persistent,
        }
    }

    pub fn from_hash(channels_hash: Blake2b256Hash, hash: Blake2b256Hash, persistent: bool) -> Self {
        Produce {
            channels_hash,
            hash,
            persistent,
        }
    }
}

impl PartialEq for Produce {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}
impl Eq for Produce {}
impl std::hash::Hash for Produce {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}
impl PartialOrd for Produce {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Produce {
    fn cmp(&self, other: &Self) -> Ordering {
        self.hash.cmp(&other.hash)
    }
}

/// A consume event (port of `Consume`).
#[derive(Clone, Debug)]
pub struct Consume {
    pub channels_hashes: Vec<Blake2b256Hash>,
    pub hash: Blake2b256Hash,
    pub persistent: bool,
}

impl Consume {
    pub fn apply<C, P, K>(
        channels: &[C],
        patterns: &[P],
        continuation: &K,
        persistent: bool,
    ) -> Self
    where
        C: Serialize<C>,
        P: Serialize<P>,
        K: Serialize<K>,
    {
        let channels_hashes = hash_seq(channels);
        let encoded_channels: Vec<Vec<u8>> = channels_hashes
            .iter()
            .map(|h| h.to_byte_array().to_vec())
            .collect();
        let hash = hash_consume(&encoded_channels, patterns, continuation, persistent);
        Consume {
            channels_hashes,
            hash,
            persistent,
        }
    }

    pub fn from_hash(
        channels_hashes: Vec<Blake2b256Hash>,
        hash: Blake2b256Hash,
        persistent: bool,
    ) -> Self {
        Consume {
            channels_hashes,
            hash,
            persistent,
        }
    }
}

impl PartialEq for Consume {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}
impl Eq for Consume {}
impl std::hash::Hash for Consume {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}
impl PartialOrd for Consume {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Consume {
    fn cmp(&self, other: &Self) -> Ordering {
        self.hash.cmp(&other.hash)
    }
}

/// A COMM event: one consume matched with one or more produces (port of `COMM`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comm {
    pub consume: Consume,
    pub produces: Vec<Produce>,
    pub peeks: BTreeSet<usize>,
    pub times_repeated: BTreeMap<Produce, usize>,
}

impl Comm {
    /// Build a `COMM`, sorting produces by `(channelsHash, hash, persistent)` (Law 8).
    pub fn apply<C, A, F>(
        data_candidates: &[ConsumeCandidate<C, A>],
        consume_ref: Consume,
        peeks: BTreeSet<usize>,
        produce_counters: F,
    ) -> Self
    where
        F: FnOnce(&[Produce]) -> BTreeMap<Produce, usize>,
    {
        let mut produce_refs: Vec<Produce> = data_candidates
            .iter()
            .map(|candidate| candidate.datum.source.clone())
            .collect();
        produce_refs.sort_by_key(|p| (p.channels_hash, p.hash, p.persistent));
        let times_repeated = produce_counters(&produce_refs);
        Comm {
            consume: consume_ref,
            produces: produce_refs,
            peeks,
            times_repeated,
        }
    }
}

/// An event log entry (port of the sealed `Event` hierarchy).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Event {
    Comm(Comm),
    Produce(Produce),
    Consume(Consume),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::internal::Datum;

    #[derive(Clone)]
    struct Byte(Vec<u8>);
    impl Serialize<Byte> for Byte {
        fn encode(a: &Byte) -> Vec<u8> {
            a.0.clone()
        }
        fn decode(bytes: &[u8]) -> Result<Byte, String> {
            Ok(Byte(bytes.to_vec()))
        }
    }

    #[test]
    fn produce_equality_is_by_hash() {
        let datum = Byte(vec![1]);
        let p1 = Produce::apply(&Byte(vec![9]), &datum, false);
        // same hash, different persistent flag
        let p2 = Produce::from_hash(p1.channels_hash, p1.hash, true);
        assert_eq!(p1, p2);
        assert_ne!(p1, Produce::apply(&Byte(vec![9]), &Byte(vec![2]), false));
    }

    #[test]
    fn comm_sorts_produces() {
        let produce_a = Produce::apply(&Byte(vec![0x0a]), &Byte(vec![1]), false);
        let produce_b = Produce::apply(&Byte(vec![0x0b]), &Byte(vec![2]), true);

        let candidate = |p: Produce| ConsumeCandidate {
            channel: Byte(vec![0]),
            datum: Datum {
                a: Byte(vec![0]),
                persist: false,
                source: p,
            },
            removed_datum: Byte(vec![0]),
            datum_index: 0,
        };

        // Insert in reverse order; `apply` must sort by (channelsHash, hash, persistent).
        let candidates = vec![candidate(produce_b), candidate(produce_a)];
        let consume = Consume::from_hash(vec![], Blake2b256Hash::from_bytes([0u8; 32]), false);
        let comm = Comm::apply(&candidates, consume, BTreeSet::new(), |_| BTreeMap::new());

        assert!(comm.produces.windows(2).all(|w| {
            (w[0].channels_hash, w[0].hash, w[0].persistent)
                <= (w[1].channels_hash, w[1].hash, w[1].persistent)
        }));
    }
}
