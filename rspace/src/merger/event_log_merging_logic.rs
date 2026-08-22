//! Pure logic for merging event logs (conflict/dependency analysis).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/merger/EventLogMergingLogic.scala`.

use std::collections::BTreeSet;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::merger::event_log_index::EventLogIndex;
use crate::trace::event::{Consume, Produce};

fn union<T: Ord + Clone>(a: &BTreeSet<T>, b: &BTreeSet<T>) -> BTreeSet<T> {
    a.union(b).cloned().collect()
}

fn diff<T: Ord + Clone>(a: &BTreeSet<T>, b: &BTreeSet<T>) -> BTreeSet<T> {
    a.difference(b).cloned().collect()
}

fn intersect<T: Ord + Clone>(a: &BTreeSet<T>, b: &BTreeSet<T>) -> BTreeSet<T> {
    a.intersection(b).cloned().collect()
}

/// Produces created inside the event log (port of `producesCreated`).
pub fn produces_created(e: &EventLogIndex) -> BTreeSet<Produce> {
    diff(
        &union(&e.produces_linear, &e.produces_persistent),
        &e.produces_copied_by_peek,
    )
}

/// Consumes created inside the event log (port of `consumesCreated`).
pub fn consumes_created(e: &EventLogIndex) -> BTreeSet<Consume> {
    union(&e.consumes_linear_and_peeks, &e.consumes_persistent)
}

/// Produces created and not destroyed inside the event log (port of
/// `producesCreatedAndNotDestroyed`).
pub fn produces_created_and_not_destroyed(e: &EventLogIndex) -> BTreeSet<Produce> {
    diff(
        &union(
            &diff(&e.produces_linear, &e.produces_consumed),
            &e.produces_persistent,
        ),
        &e.produces_copied_by_peek,
    )
}

/// Consumes created and not destroyed inside the event log (port of
/// `consumesCreatedAndNotDestroyed`).
pub fn consumes_created_and_not_destroyed(e: &EventLogIndex) -> BTreeSet<Consume> {
    union(
        &diff(&e.consumes_linear_and_peeks, &e.consumes_produced),
        &e.consumes_persistent,
    )
}

/// Produces affected by the event log (port of `producesAffected`).
pub fn produces_affected(e: &EventLogIndex) -> BTreeSet<Produce> {
    let created = produces_created(e);
    let external_destroyed: BTreeSet<Produce> = diff(&e.produces_consumed, &created)
        .into_iter()
        .filter(|p| !p.persistent)
        .collect();
    union(&produces_created_and_not_destroyed(e), &external_destroyed)
}

/// Consumes affected by the event log (port of `consumesAffected`).
pub fn consumes_affected(e: &EventLogIndex) -> BTreeSet<Consume> {
    let created = consumes_created(e);
    let external_destroyed: BTreeSet<Consume> = diff(&e.consumes_produced, &created)
        .into_iter()
        .filter(|c| !c.persistent)
        .collect();
    union(&consumes_created_and_not_destroyed(e), &external_destroyed)
}

/// Combine the copied-by-peek sets of two indices (port of `combineProducesCopiedByPeek`).
pub fn combine_produces_copied_by_peek(x: &EventLogIndex, y: &EventLogIndex) -> BTreeSet<Produce> {
    let copied = union(&x.produces_copied_by_peek, &y.produces_copied_by_peek);
    let created = union(&produces_created(x), &produces_created(y));
    diff(&copied, &created)
}

/// Whether `target` depends on `source` (port of `depends`).
pub fn depends(target: &EventLogIndex, source: &EventLogIndex) -> bool {
    let produces_source = diff(
        &produces_created_and_not_destroyed(source),
        &source.produces_mergeable,
    );
    let produces_target = diff(&target.produces_consumed, &source.produces_mergeable);
    let consumes_source = consumes_created_and_not_destroyed(source);
    let consumes_target = &target.consumes_produced;

    !intersect(&produces_source, &produces_target).is_empty()
        || !intersect(&consumes_source, consumes_target).is_empty()
}

/// Whether two event logs conflict (port of `areConflicting`).
pub fn are_conflicting(a: &EventLogIndex, b: &EventLogIndex) -> bool {
    !conflicts(a, b).is_empty()
}

/// The channels conflicting between two event logs (port of `conflicts`).
pub fn conflicts(a: &EventLogIndex, b: &EventLogIndex) -> Vec<Blake2b256Hash> {
    // Check #1: the same non-persistent produce/consume destroyed in both branches.
    let shared_consumes = intersect(&a.consumes_produced, &b.consumes_produced);
    let mergeable_consumes = intersect(&a.consumes_mergeable, &b.consumes_mergeable);
    let consume_races: Vec<Consume> = diff(&shared_consumes, &mergeable_consumes)
        .into_iter()
        .filter(|c| !c.persistent)
        .collect();

    let shared_produces = intersect(&a.produces_consumed, &b.produces_consumed);
    let mergeable_produces = intersect(&a.produces_mergeable, &b.produces_mergeable);
    let produce_races: Vec<Produce> = diff(&shared_produces, &mergeable_produces)
        .into_iter()
        .filter(|p| !p.persistent)
        .collect();

    let mut races: Vec<Blake2b256Hash> = Vec::new();
    for c in consume_races {
        races.extend(c.channels_hashes.iter().copied());
    }
    for p in produce_races {
        races.push(p.channels_hash);
    }

    // Check #2: potential COMMs between creates in one branch and consumes in the other.
    let match_found = |consume: &Consume, produce: &Produce| {
        consume.channels_hashes.contains(&produce.channels_hash)
    };
    let check = |left: &EventLogIndex, right: &EventLogIndex| -> Vec<Blake2b256Hash> {
        let produces = produces_created_and_not_destroyed(left);
        let consumes = consumes_created_and_not_destroyed(right);
        let mut out = Vec::new();
        for p in &produces {
            for c in &consumes {
                if match_found(c, p) {
                    out.push(p.channels_hash);
                }
            }
        }
        out
    };
    races.extend(check(a, b));
    races.extend(check(b, a));

    // Check #3: produces touching base joins.
    for p in a.produces_touching_base_joins.union(&b.produces_touching_base_joins) {
        races.push(p.channels_hash);
    }

    races
}
