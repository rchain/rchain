//! An index over an event log, classifying produces/consumes (a monoid).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/merger/EventLogIndex.scala` (the data type and
//! its `empty`/`combine`; the effectful `apply` constructor is in the engine phase).

use std::collections::{BTreeMap, BTreeSet};

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::merger::event_log_merging_logic::combine_produces_copied_by_peek;
use crate::trace::event::{Consume, Event, Produce};

/// Numeric-channel difference map (port of `NumberChannelsDiff`).
pub type NumberChannelsDiff = BTreeMap<Blake2b256Hash, i64>;

fn union<T: Ord + Clone>(a: &BTreeSet<T>, b: &BTreeSet<T>) -> BTreeSet<T> {
    a.union(b).cloned().collect()
}

/// An event-log index (port of `EventLogIndex`).
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct EventLogIndex {
    pub produces_linear: BTreeSet<Produce>,
    pub produces_persistent: BTreeSet<Produce>,
    pub produces_consumed: BTreeSet<Produce>,
    pub produces_peeked: BTreeSet<Produce>,
    pub produces_copied_by_peek: BTreeSet<Produce>,
    pub produces_touching_base_joins: BTreeSet<Produce>,
    pub consumes_linear_and_peeks: BTreeSet<Consume>,
    pub consumes_persistent: BTreeSet<Consume>,
    pub consumes_produced: BTreeSet<Consume>,
    pub produces_mergeable: BTreeSet<Produce>,
    pub consumes_mergeable: BTreeSet<Consume>,
    pub number_channels_data: NumberChannelsDiff,
}

impl EventLogIndex {
    /// Classify an event log into an index (port of `EventLogIndex.apply`).
    ///
    /// The two predicates are effectful in the Scala (`Produce => F[Boolean]`); here they are
    /// plain `bool` closures — the classification itself is pure, and callers resolve any
    /// pre-state reads before invoking.
    pub fn apply<F, G>(
        event_log: &[Event],
        produce_exists_in_pre_state: F,
        produce_touch_pre_state_join: G,
        mergeable_chs: NumberChannelsDiff,
    ) -> EventLogIndex
    where
        F: Fn(&Produce) -> bool,
        G: Fn(&Produce) -> bool,
    {
        let mut produces_linear = BTreeSet::new();
        let mut produces_persistent = BTreeSet::new();
        let mut consumes_linear_and_peeks = BTreeSet::new();
        let mut consumes_persistent = BTreeSet::new();
        let mut produces_consumed = BTreeSet::new();
        let mut produces_peeked = BTreeSet::new();
        let mut produces_touching_base_joins = BTreeSet::new();
        let mut produces_copied_by_peek = BTreeSet::new();
        let mut consumes_produced = BTreeSet::new();

        for event in event_log {
            match event {
                Event::Produce(p) => {
                    if produce_exists_in_pre_state(p) {
                        produces_copied_by_peek.insert(p.clone());
                    }
                    if produce_touch_pre_state_join(p) {
                        produces_touching_base_joins.insert(p.clone());
                    }
                    if p.persistent {
                        produces_persistent.insert(p.clone());
                    } else {
                        produces_linear.insert(p.clone());
                    }
                }
                Event::Consume(c) => {
                    if c.persistent {
                        consumes_persistent.insert(c.clone());
                    } else {
                        consumes_linear_and_peeks.insert(c.clone());
                    }
                }
                Event::Comm(c) => {
                    if c.peeks.is_empty() {
                        for p in &c.produces {
                            produces_consumed.insert(p.clone());
                        }
                    } else {
                        for p in &c.produces {
                            produces_peeked.insert(p.clone());
                        }
                    }
                    consumes_produced.insert(c.consume.clone());
                }
            }
        }

        let all_produces: BTreeSet<Produce> = produces_linear
            .iter()
            .chain(produces_persistent.iter())
            .chain(produces_consumed.iter())
            .chain(produces_peeked.iter())
            .cloned()
            .collect();
        let mergeable_produces: BTreeSet<Produce> = all_produces
            .into_iter()
            .filter(|p| mergeable_chs.contains_key(&p.channels_hash))
            .collect();

        let all_consumes: BTreeSet<Consume> = consumes_linear_and_peeks
            .iter()
            .chain(consumes_persistent.iter())
            .chain(consumes_produced.iter())
            .cloned()
            .collect();
        let mergeable_consumes: BTreeSet<Consume> = all_consumes
            .into_iter()
            .filter(|c| c.channels_hashes.iter().any(|h| mergeable_chs.contains_key(h)))
            .collect();

        EventLogIndex {
            produces_linear,
            produces_persistent,
            produces_consumed,
            produces_peeked,
            produces_copied_by_peek,
            produces_touching_base_joins,
            consumes_linear_and_peeks,
            consumes_persistent,
            consumes_produced,
            produces_mergeable: mergeable_produces,
            consumes_mergeable: mergeable_consumes,
            number_channels_data: mergeable_chs,
        }
    }

    pub fn empty() -> Self {
        EventLogIndex::default()
    }

    /// Combine two indices (port of `EventLogIndex.combine`).
    pub fn combine(x: &EventLogIndex, y: &EventLogIndex) -> EventLogIndex {
        let mut number_channels = x.number_channels_data.clone();
        for (k, v) in &y.number_channels_data {
            *number_channels.entry(*k).or_insert(0) += *v;
        }
        EventLogIndex {
            produces_linear: union(&x.produces_linear, &y.produces_linear),
            produces_persistent: union(&x.produces_persistent, &y.produces_persistent),
            produces_consumed: union(&x.produces_consumed, &y.produces_consumed),
            produces_peeked: union(&x.produces_peeked, &y.produces_peeked),
            produces_copied_by_peek: combine_produces_copied_by_peek(x, y),
            produces_touching_base_joins: union(
                &x.produces_touching_base_joins,
                &y.produces_touching_base_joins,
            ),
            consumes_linear_and_peeks: union(
                &x.consumes_linear_and_peeks,
                &y.consumes_linear_and_peeks,
            ),
            consumes_persistent: union(&x.consumes_persistent, &y.consumes_persistent),
            consumes_produced: union(&x.consumes_produced, &y.consumes_produced),
            produces_mergeable: union(&x.produces_mergeable, &y.produces_mergeable),
            consumes_mergeable: union(&x.consumes_mergeable, &y.consumes_mergeable),
            number_channels_data: number_channels,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::event::Comm;

    fn h(b: u8) -> Blake2b256Hash {
        Blake2b256Hash::from_bytes([b; 32])
    }

    #[test]
    fn apply_classifies_produce_consume_and_comm() {
        let produce = Produce::from_hash(h(1), h(2), false);
        let consume = Consume::from_hash(vec![h(1)], h(3), false);
        let comm = Comm {
            consume: consume.clone(),
            produces: vec![produce.clone()],
            peeks: BTreeSet::new(),
            times_repeated: BTreeMap::new(),
        };
        let log = vec![
            Event::Produce(produce.clone()),
            Event::Consume(consume.clone()),
            Event::Comm(comm),
        ];

        let mergeable: NumberChannelsDiff = BTreeMap::from([(h(1), 1i64)]);
        let idx = EventLogIndex::apply(&log, |_| false, |_| false, mergeable.clone());

        // Non-persistent events land in the linear sets.
        assert!(idx.produces_linear.contains(&produce));
        assert!(idx.consumes_linear_and_peeks.contains(&consume));
        // The COMM (no peeks) consumes the produce and produces the consume.
        assert!(idx.produces_consumed.contains(&produce));
        assert!(idx.consumes_produced.contains(&consume));
        // Channel h(1) is mergeable, so both events are classified as mergeable.
        assert!(idx.produces_mergeable.contains(&produce));
        assert!(idx.consumes_mergeable.contains(&consume));
        assert_eq!(idx.number_channels_data, mergeable);
    }
}
