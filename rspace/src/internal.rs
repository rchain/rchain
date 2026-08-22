//! Core tuple-space data types.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/internal.scala`.

use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;

use rchain_shared::serialize::Serialize;

use crate::trace::event::{Consume, Produce};

/// A value paired with its serialized bytes (port of `Encoded`).
#[derive(Clone, Debug, PartialEq)]
pub struct Encoded<D> {
    pub item: D,
    pub byte_vector: Vec<u8>,
}

/// A piece of data plus its produce source (port of `Datum`).
#[derive(Clone, Debug, PartialEq)]
pub struct Datum<A> {
    pub a: A,
    pub persist: bool,
    pub source: Produce,
}

impl<A> Datum<A> {
    pub fn create<C>(channel: &C, a: A, persist: bool) -> Self
    where
        C: Serialize<C>,
        A: Serialize<A>,
    {
        let source = Produce::apply(channel, &a, persist);
        Datum { a, persist, source }
    }
}

/// A waiting continuation plus its consume source (port of `WaitingContinuation`).
#[derive(Clone, Debug, PartialEq)]
pub struct WaitingContinuation<P, K> {
    pub patterns: Vec<P>,
    pub continuation: K,
    pub persist: bool,
    pub peeks: BTreeSet<usize>,
    pub source: Consume,
}

impl<P, K> WaitingContinuation<P, K> {
    pub fn create<C>(
        channels: &[C],
        patterns: Vec<P>,
        continuation: K,
        persist: bool,
        peeks: BTreeSet<usize>,
    ) -> Self
    where
        C: Serialize<C>,
        P: Serialize<P>,
        K: Serialize<K>,
    {
        let source = Consume::apply(channels, &patterns, &continuation, persist);
        WaitingContinuation {
            patterns,
            continuation,
            persist,
            peeks,
            source,
        }
    }
}

/// A matched datum candidate during consume (port of `ConsumeCandidate`).
#[derive(Clone, Debug, PartialEq)]
pub struct ConsumeCandidate<C, A> {
    pub channel: C,
    pub datum: Datum<A>,
    pub removed_datum: A,
    pub datum_index: i64,
}

/// A matched continuation candidate during produce (port of `ProduceCandidate`).
#[derive(Clone, Debug, PartialEq)]
pub struct ProduceCandidate<C, P, A, K> {
    pub channels: Vec<C>,
    pub continuation: WaitingContinuation<P, K>,
    pub continuation_index: usize,
    pub data_candidates: Vec<ConsumeCandidate<C, A>>,
}

/// A row of data and waiting continuations at a channel (port of `Row`).
#[derive(Clone, Debug, PartialEq)]
pub struct Row<P, A, K> {
    pub data: Vec<Datum<A>>,
    pub wks: Vec<WaitingContinuation<P, K>>,
}

impl<P, A, K> Default for Row<P, A, K> {
    fn default() -> Self {
        Row {
            data: Vec::new(),
            wks: Vec::new(),
        }
    }
}

/// A multi-map whose values form a multiset (port of `MultisetMultiMap`).
#[derive(Clone, Debug, Default)]
pub struct MultisetMultiMap<K, V> {
    map: HashMap<K, Vec<V>>,
}

impl<K, V> MultisetMultiMap<K, V>
where
    K: Eq + Hash,
    V: PartialEq,
{
    pub fn empty() -> Self {
        MultisetMultiMap {
            map: HashMap::new(),
        }
    }

    pub fn add_binding(&mut self, key: K, value: V) {
        self.map.entry(key).or_default().push(value);
    }

    pub fn remove_binding(&mut self, key: K, value: V) {
        let mut remove_key = false;
        if let Some(values) = self.map.get_mut(&key) {
            if let Some(pos) = values.iter().position(|v| v == &value) {
                values.remove(pos);
            }
            remove_key = values.is_empty();
        }
        if remove_key {
            self.map.remove(&key);
        }
    }
}

/// An installed continuation (port of `Install`; the unused `F`/`A` parameters are dropped).
#[derive(Clone, Debug, PartialEq)]
pub struct Install<P, K> {
    pub patterns: Vec<P>,
    pub continuation: K,
}

/// The installed-continuation map (port of `Installs`).
pub type Installs<C, P, K> = HashMap<Vec<C>, Install<P, K>>;
