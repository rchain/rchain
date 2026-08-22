//! The multi-parent DAG finalizer / fringe estimator.
//!
//! Mirrors `block-storage/src/main/scala/coop/rchain/blockstorage/dag/Finalizer.scala`. The
//! finality threshold is [`rchain_sdk::consensus::is_super_majority`] (Law 14: strictly > 2/3).

use std::collections::{BTreeMap, BTreeSet};
use std::hash::{Hash, Hasher};

use rchain_sdk::consensus::is_super_majority;
use rchain_shared::refined::{BlockHeight, NonNegI64, SeqNum};

use super::message_map;

/// A message (view) carrying all data the finalizer needs.
///
/// `hashCode` is overridden to `id.hashCode()` in the Scala (identity for set/map membership);
/// the Rust `Hash` impl mirrors that. `Eq`/`Ord` remain full structural comparison.
///
/// `height`/`sender_seq` carry their non-negativity structurally (`BlockHeight`/`SeqNum`), and the
/// `bonds_map` value is a *stake* amount (non-negative by the PoS invariant), carried as `NonNegI64`
/// (the `Bond` refinement).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Message<M, S> {
    pub id: M,
    pub height: BlockHeight,
    pub sender: S,
    pub sender_seq: SeqNum,
    pub bonds_map: BTreeMap<S, NonNegI64>,
    pub parents: BTreeSet<M>,
    pub fringe: BTreeSet<M>,
    /// Cache of seen message ids.
    pub seen: BTreeSet<M>,
}

impl<M: Hash, S> Hash for Message<M, S> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

/// Multi-parent finalization over a cache of all messages. The message map is borrowed (never
/// cloned) — the finalizer only reads it, so a per-call full-map clone is unnecessary (H6).
#[derive(Clone, Debug)]
pub struct Finalizer<'a, M, S> {
    pub msg_map: &'a BTreeMap<M, Message<M, S>>,
}

impl<'a, M, S> Finalizer<'a, M, S>
where
    M: Ord + Clone + Eq + Hash,
    S: Ord + Clone + Eq + Hash,
{
    pub fn new(msg_map: &'a BTreeMap<M, Message<M, S>>) -> Self {
        Self { msg_map }
    }

    fn msg(&self, id: &M) -> Option<Message<M, S>> {
        self.msg_map.get(id).cloned()
    }

    /// Iterate the sender's own parent chain (nearest to oldest), stopping at `finalized` messages.
    fn self_parents(&self, mv: &Message<M, S>, finalized: &BTreeSet<Message<M, S>>) -> Vec<Message<M, S>> {
        let mut chain = Vec::new();
        let mut next: Vec<Message<M, S>> = mv
            .parents
            .iter()
            .filter_map(|p| self.msg(p))
            .filter(|x| x.sender == mv.sender && !finalized.contains(x))
            .collect();
        while let Some(m) = next.pop() {
            chain.push(m.clone());
            next = m
                .parents
                .iter()
                .filter_map(|p| self.msg(p))
                .filter(|x| x.sender == mv.sender && !finalized.contains(x))
                .collect();
        }
        chain
    }

    /// Whether the minimum messages are enough for the next-fringe calculation.
    pub fn check_min_messages(&self, min_msgs: &[Message<M, S>], bonds_map: &BTreeMap<S, NonNegI64>) -> bool {
        // TODO: epoch changes need more than a sender-count comparison.
        min_msgs.len() == bonds_map.len()
    }

    /// Find the top (most recent) message referenced from the minimum messages, per sender.
    pub fn calculate_next_layer(&self, min_msgs: &[Message<M, S>]) -> BTreeMap<S, Message<M, S>> {
        let mut min_messages_map: BTreeMap<S, Message<M, S>> = min_msgs
            .iter()
            .map(|x| (x.sender.clone(), x.clone()))
            .collect();
        let mut candidates: Vec<Message<M, S>> = min_msgs
            .iter()
            .flat_map(|x| x.parents.iter().filter_map(|p| self.msg(p)))
            .collect();
        candidates.retain(|x| min_messages_map.contains_key(&x.sender));
        for m in candidates {
            if let Some(curr) = min_messages_map.get(&m.sender) {
                if m.sender_seq > curr.sender_seq {
                    min_messages_map.insert(m.sender.clone(), m);
                }
            }
        }
        min_messages_map
    }

    /// For each justification sender, which next-layer messages each parent sees.
    pub fn calculate_next_fringe_support_map(
        &self,
        parents: &BTreeSet<Message<M, S>>,
        next_layer: &BTreeMap<S, Message<M, S>>,
        finalized: &BTreeSet<Message<M, S>>,
    ) -> BTreeMap<S, BTreeMap<S, BTreeSet<S>>> {
        let next_layer_ids: BTreeSet<M> = next_layer.values().map(|m| m.id.clone()).collect();
        let mut result = BTreeMap::new();
        for mv in parents {
            let mut seen_by: BTreeMap<S, BTreeSet<S>> = BTreeMap::new();
            for (s, min_msg) in next_layer {
                let parents_of_parent: BTreeSet<M> =
                    mv.parents.difference(&next_layer_ids).cloned().collect();
                let mut see_min_msg: BTreeSet<S> = BTreeSet::new();
                for p_id in &parents_of_parent {
                    if let Some(p) = self.msg(p_id) {
                        let mut self_msgs = vec![p.clone()];
                        self_msgs.extend(self.self_parents(&p, finalized));
                        if self_msgs.iter().any(|m| m.seen.contains(&min_msg.id)) {
                            see_min_msg.insert(p.sender.clone());
                        }
                    }
                }
                if !see_min_msg.is_empty() {
                    seen_by.insert(s.clone(), see_min_msg);
                }
            }
            if !seen_by.is_empty() {
                result.insert(mv.sender.clone(), seen_by);
            }
        }
        result
    }

    /// Whether the supporting stake is a supermajority (Law 14).
    pub fn calculate_fringe(
        &self,
        next_fringe_support_map: &BTreeMap<S, BTreeMap<S, BTreeSet<S>>>,
        bonds_map: &BTreeMap<S, NonNegI64>,
    ) -> bool {
        let bonded_senders: BTreeSet<S> = bonds_map.keys().cloned().collect();
        let mut full_partition_stake: i128 = 0;
        for (sender, seen_by) in next_fringe_support_map {
            let all_bonded = !seen_by.is_empty() && seen_by.values().all(|v| v == &bonded_senders);
            // Only bonded senders contribute stake. A non-bonded justification sender must not
            // index `bonds_map` (it would panic) — skip it instead.
            if all_bonded {
                if let Some(stake) = bonds_map.get(sender) {
                    full_partition_stake += i128::from(i64::from(*stake));
                }
            }
        }
        let total_stake: i128 = bonds_map
            .values()
            .map(|v| i128::from(i64::from(*v)))
            .sum();
        is_super_majority(full_partition_stake, total_stake)
    }

    fn next_fringe(
        &self,
        justifications: &BTreeSet<Message<M, S>>,
        bonds_map: &BTreeMap<S, NonNegI64>,
        prev_fringe: &BTreeSet<Message<M, S>>,
    ) -> Option<BTreeSet<Message<M, S>>> {
        // Minimum (oldest non-finalized) message from each justification sender.
        let mut min_msgs = Vec::new();
        for p in justifications {
            let mut chain = vec![p.clone()];
            chain.extend(self.self_parents(p, prev_fringe));
            // `chain` is non-empty by construction (seeded with `p`).
            min_msgs.push(chain.into_iter().last().unwrap_or_else(|| p.clone()));
        }
        if !self.check_min_messages(&min_msgs, bonds_map) {
            return None;
        }
        let next_layer = self.calculate_next_layer(&min_msgs);
        let fringe_support_map =
            self.calculate_next_fringe_support_map(justifications, &next_layer, prev_fringe);
        if self.calculate_fringe(&fringe_support_map, bonds_map) {
            Some(next_layer.values().cloned().collect())
        } else {
            None
        }
    }

    /// Compute the fringe from joined justifications and any newly detected fringe.
    pub fn calculate_finalization(
        &self,
        justifications: &BTreeSet<Message<M, S>>,
        bonds_map: &BTreeMap<S, NonNegI64>,
    ) -> (BTreeSet<Message<M, S>>, Option<BTreeSet<Message<M, S>>>) {
        let parent_fringe = message_map::latest_fringe(self.msg_map, justifications);
        let mut current = parent_fringe.clone();
        let mut new_fringe_opt: Option<BTreeSet<Message<M, S>>> = None;
        while let Some(nf) = self.next_fringe(justifications, bonds_map, &current) {
            // Progress guard: a non-advancing fringe would loop forever. Only record a strictly
            // new fringe.
            if nf == current {
                break;
            }
            new_fringe_opt = Some(nf.clone());
            current = nf;
        }
        (parent_fringe, new_fringe_opt)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn msg(id: i32, sender: i32, sender_seq: i64, parents: &[i32], seen: &[i32]) -> Message<i32, i32> {
        Message {
            id,
            height: BlockHeight::zero(),
            sender,
            sender_seq: SeqNum::try_from(sender_seq).unwrap(),
            bonds_map: BTreeMap::new(),
            parents: parents.iter().copied().collect(),
            fringe: BTreeSet::new(),
            seen: seen.iter().copied().collect(),
        }
    }

    fn bonded() -> BTreeMap<i32, NonNegI64> {
        [(0, 1), (1, 1), (2, 1)]
            .into_iter()
            .map(|(k, v)| (k, NonNegI64::try_from(v).unwrap()))
            .collect()
    }

    /// A support map in which every sender in `see_full` sees the full bonded partition.
    fn support_with_full(
        see_full: &[i32],
        bonded: &BTreeSet<i32>,
    ) -> BTreeMap<i32, BTreeMap<i32, BTreeSet<i32>>> {
        see_full
            .iter()
            .map(|&s| {
                let seen_by = bonded.iter().map(|&b| (b, bonded.clone())).collect();
                (s, seen_by)
            })
            .collect()
    }

    #[test]
    fn law14_fringe_requires_supermajority() {
        let map: BTreeMap<i32, Message<i32, i32>> = BTreeMap::new();
        let finalizer: Finalizer<i32, i32> = Finalizer::new(&map);
        let bonds = bonded();
        let bonded_senders: BTreeSet<i32> = bonds.keys().copied().collect();

        // All 3 validators see the full partition -> 3/3 > 2/3 -> finalizes.
        assert!(finalizer.calculate_fringe(&support_with_full(&[0, 1, 2], &bonded_senders), &bonds));

        // Only 2 of 3 -> 2/3 is NOT > 2/3 -> does not finalize.
        assert!(!finalizer.calculate_fringe(&support_with_full(&[0, 1], &bonded_senders), &bonds));
    }

    #[test]
    fn calculate_fringe_ignores_non_bonded_sender() {
        let map: BTreeMap<i32, Message<i32, i32>> = BTreeMap::new();
        let finalizer: Finalizer<i32, i32> = Finalizer::new(&map);
        let bonds = bonded();
        let bonded_senders: BTreeSet<i32> = bonds.keys().copied().collect();

        // A support map that includes a non-bonded sender (99) seeing the full partition. This
        // must not panic on `bonds_map[sender]` and must still finalize on the bonded 3/3 stake.
        let mut support = support_with_full(&[0, 1, 2], &bonded_senders);
        support.insert(
            99,
            bonded_senders.iter().map(|&b| (b, bonded_senders.clone())).collect(),
        );
        assert!(finalizer.calculate_fringe(&support, &bonds));
    }

    #[test]
    fn check_min_messages_needs_all_bonded_senders() {
        let map: BTreeMap<i32, Message<i32, i32>> = BTreeMap::new();
        let finalizer: Finalizer<i32, i32> = Finalizer::new(&map);
        let bonds = bonded();
        assert!(finalizer.check_min_messages(&[msg(0, 0, 0, &[], &[]), msg(1, 1, 0, &[], &[]), msg(2, 2, 0, &[], &[])], &bonds));
        assert!(!finalizer.check_min_messages(&[msg(0, 0, 0, &[], &[])], &bonds));
    }

    #[test]
    fn calculate_next_layer_picks_max_sender_seq() {
        // m0's parent is m0_later (same sender, higher sender_seq).
        let m0_later = msg(3, 0, 1, &[], &[]);
        let m0 = msg(0, 0, 0, &[3], &[]);
        let m1 = msg(1, 1, 0, &[], &[]);
        let m2 = msg(2, 2, 0, &[], &[]);
        let map: BTreeMap<i32, Message<i32, i32>> =
            [(0, m0.clone()), (1, m1.clone()), (2, m2.clone()), (3, m0_later.clone())]
                .into_iter()
                .collect();
        let finalizer: Finalizer<i32, i32> = Finalizer::new(&map);
        let min_msgs = [m0, m1, m2];
        let next = finalizer.calculate_next_layer(&min_msgs);
        assert_eq!(next[&0].id, 3); // validator 0's later parent wins
        assert_eq!(next[&1].id, 1);
        assert_eq!(next[&2].id, 2);
    }

    #[test]
    fn calculate_finalization_advances_fringe_on_fork() {
        // Genesis by a non-bonded sender (99); three bonded senders 0/1/2.
        let genesis = msg(99, 99, 0, &[], &[99]);
        // Layer 1: a three-way fork — each sees only genesis.
        let a1 = msg(10, 0, 1, &[99], &[99, 10]);
        let b1 = msg(11, 1, 1, &[99], &[99, 11]);
        let c1 = msg(12, 2, 1, &[99], &[99, 12]);
        // Layer 2 + 3: convergence.
        let a2 = msg(20, 0, 2, &[10, 11, 12], &[99, 10, 11, 12, 20]);
        let b2 = msg(21, 1, 2, &[10, 11, 12], &[99, 10, 11, 12, 21]);
        let c2 = msg(22, 2, 2, &[10, 11, 12], &[99, 10, 11, 12, 22]);
        let a3 = msg(30, 0, 3, &[20, 21, 22], &[99, 10, 11, 12, 20, 21, 22, 30]);
        let b3 = msg(31, 1, 3, &[20, 21, 22], &[99, 10, 11, 12, 20, 21, 22, 31]);
        let c3 = msg(32, 2, 3, &[20, 21, 22], &[99, 10, 11, 12, 20, 21, 22, 32]);

        let map: BTreeMap<i32, Message<i32, i32>> = [
            genesis.clone(), a1.clone(), b1.clone(), c1.clone(), a2.clone(), b2.clone(), c2.clone(),
            a3.clone(), b3.clone(), c3.clone(),
        ]
        .into_iter()
        .map(|m| (m.id, m))
        .collect();
        let bonds: BTreeMap<i32, NonNegI64> =
            [(0, 10), (1, 10), (2, 10)].into_iter().map(|(k, v)| (k, NonNegI64::try_from(v).unwrap())).collect();

        let justifications: BTreeSet<Message<i32, i32>> =
            [a3, b3, c3].into_iter().collect();
        let finalizer: Finalizer<i32, i32> = Finalizer::new(&map);
        let (_parent, new_fringe) = finalizer.calculate_finalization(&justifications, &bonds);
        let ids: BTreeSet<i32> = new_fringe.expect("fringe should advance").into_iter().map(|m| m.id).collect();
        assert_eq!(ids, [10, 11, 12].into_iter().collect());
    }

    #[test]
    fn calculate_finalization_returns_none_without_supermajority_support() {
        // A lockstep chain: only one sender's message per layer, so no full-partition support.
        let genesis = msg(99, 99, 0, &[], &[99]);
        let a1 = msg(10, 0, 1, &[99], &[99, 10]);
        let b1 = msg(11, 1, 1, &[10], &[99, 10, 11]);
        let c1 = msg(12, 2, 1, &[11], &[99, 10, 11, 12]);
        let map: BTreeMap<i32, Message<i32, i32>> =
            [genesis, a1, b1, c1].into_iter().map(|m| (m.id, m)).collect();
        let bonds: BTreeMap<i32, NonNegI64> =
            [(0, 10), (1, 10), (2, 10)].into_iter().map(|(k, v)| (k, NonNegI64::try_from(v).unwrap())).collect();
        let justifications: BTreeSet<Message<i32, i32>> =
            [map[&10].clone(), map[&11].clone(), map[&12].clone()].into_iter().collect();
        let finalizer: Finalizer<i32, i32> = Finalizer::new(&map);
        let (_parent, new_fringe) = finalizer.calculate_finalization(&justifications, &bonds);
        assert!(new_fringe.is_none(), "lockstep chain must not finalize");
    }
}
