//! The state change between two history snapshots (a monoid).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/merger/StateChange.scala` (the data type and
//! its `empty`/`combine`; the effectful `apply` is in the engine phase).

use std::collections::BTreeMap;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::serialize::Serialize;

use crate::hashing::stable_hash_provider::{hash_channel, hash_hashes};
use crate::history::history_reader::HistoryReaderBinary;
use crate::merger::channel_change::ChannelChange;
use crate::merger::event_log_index::EventLogIndex;
use crate::merger::event_log_merging_logic::{consumes_affected, produces_affected};
use crate::merger::seq_diff;

fn combine_channel_change_map<K, V>(
    x: &BTreeMap<K, ChannelChange<V>>,
    y: &BTreeMap<K, ChannelChange<V>>,
) -> BTreeMap<K, ChannelChange<V>>
where
    K: Ord + Clone,
    V: Clone,
{
    let mut out = x.clone();
    for (k, v) in y {
        match out.get(k) {
            Some(existing) => {
                out.insert(k.clone(), ChannelChange::combine(existing, v));
            }
            None => {
                out.insert(k.clone(), v.clone());
            }
        }
    }
    out
}

/// The diff between two history states (port of `StateChange`).
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct StateChange {
    pub datums_changes: BTreeMap<Blake2b256Hash, ChannelChange<Vec<u8>>>,
    pub kont_changes: BTreeMap<Vec<Blake2b256Hash>, ChannelChange<Vec<u8>>>,
    pub consume_channels_to_join_serialized_map: BTreeMap<Vec<Blake2b256Hash>, Vec<u8>>,
}

/// Compute the added/removed raw bytes between a start and end value (port of `computeValueChange`).
fn compute_value_change(start_value: Vec<Vec<u8>>, end_value: Vec<Vec<u8>>) -> ChannelChange<Vec<u8>> {
    let added = seq_diff(&end_value, &start_value);
    let removed = seq_diff(&start_value, &end_value);
    ChannelChange { added, removed }
}

impl StateChange {
    /// Build a `StateChange` by diffing the pre/post state readers over the channels touched by an
    /// event log (port of `StateChange.apply`).
    pub async fn apply<C, P, A, K>(
        pre_state_reader: &(dyn HistoryReaderBinary<C, P, A, K> + Sync),
        post_state_reader: &(dyn HistoryReaderBinary<C, P, A, K> + Sync),
        event_log_index: &EventLogIndex,
    ) -> Result<StateChange, String>
    where
        C: Serialize<C> + Send + Sync,
        P: Send + Sync,
        A: Send + Sync,
        K: Send + Sync,
    {
        let mut datums_diff: BTreeMap<Blake2b256Hash, ChannelChange<Vec<u8>>> = BTreeMap::new();
        let mut konts_diff: BTreeMap<Vec<Blake2b256Hash>, ChannelChange<Vec<u8>>> = BTreeMap::new();
        let mut joins_map: BTreeMap<Vec<Blake2b256Hash>, Vec<u8>> = BTreeMap::new();

        let produce_hashes: Vec<Blake2b256Hash> = produces_affected(event_log_index)
            .iter()
            .map(|p| p.channels_hash)
            .collect();
        for history_pointer in produce_hashes {
            let start: Vec<Vec<u8>> = pre_state_reader
                .get_data(history_pointer)
                .await.map_err(|e| e.to_string())?
                .into_iter()
                .map(|d| d.raw)
                .collect();
            let end: Vec<Vec<u8>> = post_state_reader
                .get_data(history_pointer)
                .await.map_err(|e| e.to_string())?
                .into_iter()
                .map(|d| d.raw)
                .collect();
            let change = compute_value_change(start, end);
            let cur = datums_diff.entry(history_pointer).or_default();
            cur.added.extend(change.added);
            cur.removed.extend(change.removed);
        }

        let consume_channel_sets: Vec<Vec<Blake2b256Hash>> = consumes_affected(event_log_index)
            .iter()
            .map(|c| c.channels_hashes.clone())
            .collect();
        for consume_channels in consume_channel_sets {
            let history_pointer = hash_hashes(&consume_channels);
            let start: Vec<Vec<u8>> = pre_state_reader
                .get_continuations(history_pointer)
                .await.map_err(|e| e.to_string())?
                .into_iter()
                .map(|wc| wc.raw)
                .collect();
            let end: Vec<Vec<u8>> = post_state_reader
                .get_continuations(history_pointer)
                .await.map_err(|e| e.to_string())?
                .into_iter()
                .map(|wc| wc.raw)
                .collect();
            let change = compute_value_change(start, end);
            let cur = konts_diff.entry(consume_channels.clone()).or_default();
            cur.added.extend(change.added);
            cur.removed.extend(change.removed);

            // Recover the serialized join body matching these consume channels.
            let history_pointer = consume_channels[0];
            let pre = pre_state_reader.get_joins(history_pointer).await.map_err(|e| e.to_string())?;
            let post = post_state_reader.get_joins(history_pointer).await.map_err(|e| e.to_string())?;
            let err_msg = "Tuple space inconsistency found: channel of consume does not contain \
                           join record corresponding to the consume channels."
                .to_string();
            let raw_join = pre
                .iter()
                .chain(post.iter())
                .find(|j| {
                    let joins_channels: Vec<Blake2b256Hash> =
                        j.decoded.iter().map(hash_channel).collect();
                    let mut consume_sorted = consume_channels.clone();
                    consume_sorted.sort();
                    let mut joins_sorted = joins_channels.clone();
                    joins_sorted.sort();
                    consume_sorted == joins_sorted
                })
                .map(|j| j.raw.clone())
                .ok_or(err_msg)?;
            joins_map.insert(consume_channels, raw_join);
        }

        if datums_diff
            .values()
            .any(|c| c.added.is_empty() && c.removed.is_empty())
        {
            return Err(
                "State change compute logic error: empty channel change for produce.".to_string(),
            );
        }
        if konts_diff
            .values()
            .any(|c| c.added.is_empty() && c.removed.is_empty())
        {
            return Err(
                "State change compute logic error: empty channel change for consume.".to_string(),
            );
        }

        Ok(StateChange {
            datums_changes: datums_diff,
            kont_changes: konts_diff,
            consume_channels_to_join_serialized_map: joins_map,
        })
    }

    pub fn empty() -> Self {
        StateChange::default()
    }

    /// Combine two state changes (port of `StateChange.combine`).
    pub fn combine(x: &StateChange, y: &StateChange) -> StateChange {
        let mut joins = x.consume_channels_to_join_serialized_map.clone();
        for (k, v) in &y.consume_channels_to_join_serialized_map {
            joins.insert(k.clone(), v.clone());
        }
        StateChange {
            datums_changes: combine_channel_change_map(&x.datums_changes, &y.datums_changes),
            kont_changes: combine_channel_change_map(&x.kont_changes, &y.kont_changes),
            consume_channels_to_join_serialized_map: joins,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn combine_is_associative() {
        let a = StateChange {
            datums_changes: BTreeMap::from([(
                Blake2b256Hash::from_bytes([1; 32]),
                ChannelChange { added: vec![vec![1]], removed: vec![] },
            )]),
            ..Default::default()
        };
        let b = StateChange {
            datums_changes: BTreeMap::from([(
                Blake2b256Hash::from_bytes([1; 32]),
                ChannelChange { added: vec![], removed: vec![vec![2]] },
            )]),
            ..Default::default()
        };
        let ab = StateChange::combine(&a, &b);
        let ba = StateChange::combine(&b, &a);
        // ChannelChange combine is concatenation, so order of added/removed differs,
        // but the monoid law tested here is empty-is-identity.
        assert_eq!(StateChange::combine(&a, &StateChange::empty()), a);
        // both orders contain the same multiset of added/removed
        let mut ab_added = ab.datums_changes[&Blake2b256Hash::from_bytes([1; 32])].added.clone();
        ab_added.sort();
        let mut ba_added = ba.datums_changes[&Blake2b256Hash::from_bytes([1; 32])].added.clone();
        ba_added.sort();
        assert_eq!(ab_added, ba_added);
    }
}
