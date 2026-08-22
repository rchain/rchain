//! Tuplespace-level reporting events (port of `trace/TuplespaceEvent.scala`).
//!
//! A [`TuplespaceEvent`] pairs an incoming operation with the operation it matched (if any), and
//! classifies each operation by polarity (send/receive) and cardinality (linear/non-linear/peek).

use std::collections::BTreeSet;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::trace::event::{Comm, Consume, Produce};

/// The polarity of a tuplespace operation (port of `Polarity`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Polarity {
    Send,
    Receive,
}

/// The cardinality of a tuplespace operation (port of `Cardinality`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cardinality {
    Linear,
    NonLinear,
    Peek,
}

/// A single tuplespace operation (port of `TuplespaceOperation`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TuplespaceOperation {
    pub polarity: Polarity,
    pub cardinality: Cardinality,
    pub event_hash: Blake2b256Hash,
}

/// An incoming operation paired with the operation it matched (port of `TuplespaceEvent`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TuplespaceEvent {
    pub incoming: TuplespaceOperation,
    pub matched: Option<TuplespaceOperation>,
}

fn to_operation_produce(produce: &Produce) -> TuplespaceOperation {
    TuplespaceOperation {
        polarity: Polarity::Send,
        cardinality: if produce.persistent {
            Cardinality::NonLinear
        } else {
            Cardinality::Linear
        },
        event_hash: produce.hash,
    }
}

fn to_operation_consume(consume: &Consume, peeks: bool) -> TuplespaceOperation {
    TuplespaceOperation {
        polarity: Polarity::Receive,
        cardinality: if consume.persistent {
            Cardinality::NonLinear
        } else if peeks {
            Cardinality::Peek
        } else {
            Cardinality::Linear
        },
        event_hash: consume.hash,
    }
}

impl TuplespaceEvent {
    /// Build an event from a produce (port of `TuplespaceEvent.from(produce)`).
    pub fn from_produce(produce: &Produce) -> (Blake2b256Hash, TuplespaceEvent) {
        (
            produce.channels_hash,
            TuplespaceEvent {
                incoming: to_operation_produce(produce),
                matched: None,
            },
        )
    }

    /// Build an event from a single-channel consume (port of `TuplespaceEvent.from(consume)`).
    pub fn from_consume(consume: &Consume) -> Option<(Blake2b256Hash, TuplespaceEvent)> {
        if let [single_channel_hash] = consume.channels_hashes.as_slice() {
            Some((
                *single_channel_hash,
                TuplespaceEvent {
                    incoming: to_operation_consume(consume, false),
                    matched: None,
                },
            ))
        } else {
            None
        }
    }

    /// Build an event from a single-produce COMM (port of `TuplespaceEvent.from(comm, incoming)`).
    pub fn from_comm(
        comm: &Comm,
        incoming_consumes: &BTreeSet<Consume>,
    ) -> Option<(Blake2b256Hash, TuplespaceEvent)> {
        if comm.produces.len() != 1 {
            return None;
        }
        let produce = &comm.produces[0];
        let produce_op = to_operation_produce(produce);
        let consume_op = to_operation_consume(&comm.consume, !comm.peeks.is_empty());

        let peek_initiated = comm.times_repeated.get(produce).copied().unwrap_or(0) != 0;
        let incoming = if incoming_consumes.contains(&comm.consume) && !peek_initiated {
            consume_op.clone()
        } else {
            produce_op.clone()
        };
        let matched = if incoming == produce_op {
            consume_op
        } else {
            produce_op
        };
        Some((
            produce.channels_hash,
            TuplespaceEvent {
                incoming,
                matched: Some(matched),
            },
        ))
    }

    /// Whether this event was left unsatisfied (port of `TuplespaceEventOps.unsatisfied`).
    pub fn unsatisfied(&self) -> bool {
        match self.incoming.cardinality {
            Cardinality::Peek => self.matched.is_none(),
            Cardinality::Linear => self
                .matched
                .as_ref()
                .map_or(true, |m| m.cardinality == Cardinality::Peek),
            Cardinality::NonLinear => self
                .matched
                .as_ref()
                .map_or(true, |m| m.cardinality != Cardinality::NonLinear),
        }
    }

    /// Whether this event conflicts with another (port of `TuplespaceEventOps.conflicts`).
    pub fn conflicts(&self, other: &TuplespaceEvent) -> bool {
        if self.incoming.polarity == other.incoming.polarity {
            let both_peeks = self.incoming.cardinality == Cardinality::Peek
                && other.incoming.cardinality == Cardinality::Peek;
            let both_matched_same_non_persistent = match (&self.matched, &other.matched) {
                (Some(this_matched), Some(other_matched)) => {
                    Some(this_matched == other_matched && other_matched.cardinality != Cardinality::NonLinear)
                }
                _ => None,
            };
            if both_peeks {
                false
            } else {
                both_matched_same_non_persistent.unwrap_or(false)
            }
        } else {
            self.unsatisfied() && other.unsatisfied()
        }
    }
}
