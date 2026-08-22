//! Turning a [`StateChange`] into hash-addressed trie actions (Law 9: deterministic merge).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/merger/StateChangeMerger.scala`.

use std::collections::BTreeMap;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::hashing::stable_hash_provider::hash_hashes;
use crate::history::history_reader::HistoryReaderBinary;
use crate::hot_store_trie_action::HotStoreTrieAction;
use crate::merger::channel_change::ChannelChange;
use crate::merger::event_log_index::NumberChannelsDiff;
use crate::merger::seq_diff;
use crate::merger::state_change::StateChange;

/// A join to add or remove on merge (port of `StateChangeMerger.JoinAction`).
enum JoinAction {
    AddJoin(Vec<Blake2b256Hash>),
    RemoveJoin(Vec<Blake2b256Hash>),
}

impl JoinAction {
    fn channels(&self) -> &[Blake2b256Hash] {
        match self {
            JoinAction::AddJoin(channels) | JoinAction::RemoveJoin(channels) => channels,
        }
    }
}

/// Produce a single trie action from a channel change against the base reader (port of
/// `StateChangeMerger.mkTrieAction`).
async fn mk_trie_action<C, P, A, K, R, U>(
    history_pointer: Blake2b256Hash,
    init_value: Vec<Vec<u8>>,
    changes: &ChannelChange<Vec<u8>>,
    remove_action: R,
    update_action: U,
) -> Result<HotStoreTrieAction<C, P, A, K>, String>
where
    R: FnOnce(Blake2b256Hash) -> HotStoreTrieAction<C, P, A, K>,
    U: FnOnce(Blake2b256Hash, Vec<Vec<u8>>) -> HotStoreTrieAction<C, P, A, K>,
{
    let new_val = {
        let mut v = seq_diff(&init_value, &changes.removed);
        v.extend(changes.added.clone());
        v
    };
    if new_val.is_empty() && !init_value.is_empty() {
        Ok(remove_action(history_pointer))
    } else if init_value != new_val {
        Ok(update_action(history_pointer, new_val))
    } else {
        Err(
            "Merging logic error: empty channel change for produce or join when computing trie action."
                .to_string(),
        )
    }
}

/// Compute the trie actions that apply a [`StateChange`] on top of a base state (port of
/// `StateChangeMerger.computeTrieActions`).
///
/// `handle_channel_change` is the mergeable-channel override hook; in the Scala it is effectful
/// (`=> F[Option[HotStoreTrieAction]]`), here it is a plain `Option`-returning closure.
pub async fn compute_trie_actions<C, P, A, K, F>(
    changes: &StateChange,
    base_reader: &(dyn HistoryReaderBinary<C, P, A, K> + Sync),
    mergeable_chs: NumberChannelsDiff,
    handle_channel_change: F,
) -> Result<Vec<HotStoreTrieAction<C, P, A, K>>, String>
where
    C: Send + Sync,
    P: Send + Sync,
    A: Send + Sync,
    K: Send + Sync,
    F: Fn(
        &Blake2b256Hash,
        &ChannelChange<Vec<u8>>,
        &NumberChannelsDiff,
    ) -> Option<HotStoreTrieAction<C, P, A, K>>,
{
    // Consume trie actions and the joins they add/remove.
    let mut consume_trie_actions: Vec<HotStoreTrieAction<C, P, A, K>> = Vec::new();
    let mut join_actions: Vec<JoinAction> = Vec::new();

    for (consume_channels, channel_change) in &changes.kont_changes {
        let history_pointer = hash_hashes(consume_channels);
        let init: Vec<Vec<u8>> = base_reader
            .get_continuations(history_pointer)
            .await.map_err(|e| e.to_string())?
            .into_iter()
            .map(|wc| wc.raw)
            .collect();
        let new_val = {
            let mut v = seq_diff(&init, &channel_change.removed);
            v.extend(channel_change.added.clone());
            v
        };
        if init == new_val {
            return Err(
                "Merging logic error: empty consume change when computing trie action.".to_string(),
            );
        }
        if new_val.is_empty() && !init.is_empty() {
            consume_trie_actions.push(HotStoreTrieAction::TrieDeleteConsume(history_pointer));
            join_actions.push(JoinAction::RemoveJoin(consume_channels.clone()));
        } else if init.is_empty() {
            consume_trie_actions.push(HotStoreTrieAction::TrieInsertBinaryConsume(
                history_pointer,
                new_val,
            ));
            join_actions.push(JoinAction::AddJoin(consume_channels.clone()));
        } else {
            consume_trie_actions.push(HotStoreTrieAction::TrieInsertBinaryConsume(
                history_pointer,
                new_val,
            ));
        }
    }

    // Produce trie actions.
    let mut produce_trie_actions: Vec<HotStoreTrieAction<C, P, A, K>> = Vec::new();
    for (history_pointer, channel_change) in &changes.datums_changes {
        match handle_channel_change(history_pointer, channel_change, &mergeable_chs) {
            Some(action) => produce_trie_actions.push(action),
            None => {
                let init: Vec<Vec<u8>> = base_reader
                    .get_data(*history_pointer)
                    .await.map_err(|e| e.to_string())?
                    .into_iter()
                    .map(|d| d.raw)
                    .collect();
                let action = mk_trie_action(
                    *history_pointer,
                    init,
                    channel_change,
                    HotStoreTrieAction::TrieDeleteProduce,
                    HotStoreTrieAction::TrieInsertBinaryProduce,
                )
                .await?;
                produce_trie_actions.push(action);
            }
        }
    }

    // Join trie actions: spread each join add/remove over its member channels.
    let mut joins_changes: BTreeMap<Blake2b256Hash, ChannelChange<Vec<u8>>> = BTreeMap::new();
    for join_action in &join_actions {
        let join_channels = join_action.channels();
        let join = changes
            .consume_channels_to_join_serialized_map
            .get(join_channels)
            .ok_or_else(|| {
                "No ByteVector value for join found when merging when computing trie action."
                    .to_string()
            })?;
        for c in join_channels {
            let cur = joins_changes.remove(c).unwrap_or_default();
            let new_val = match join_action {
                JoinAction::AddJoin(_) => ChannelChange {
                    added: [vec![join.clone()], cur.added].concat(),
                    removed: cur.removed,
                },
                JoinAction::RemoveJoin(_) => ChannelChange {
                    added: cur.added,
                    removed: [vec![join.clone()], cur.removed].concat(),
                },
            };
            joins_changes.insert(*c, new_val);
        }
    }

    let mut joins_trie_actions: Vec<HotStoreTrieAction<C, P, A, K>> = Vec::new();
    for (history_pointer, channel_change) in &joins_changes {
        let init: Vec<Vec<u8>> = base_reader
            .get_joins(*history_pointer)
            .await.map_err(|e| e.to_string())?
            .into_iter()
            .map(|j| j.raw)
            .collect();
        let action = mk_trie_action(
            *history_pointer,
            init,
            channel_change,
            HotStoreTrieAction::TrieDeleteJoins,
            HotStoreTrieAction::TrieInsertBinaryJoins,
        )
        .await?;
        joins_trie_actions.push(action);
    }

    let mut result = produce_trie_actions;
    result.extend(consume_trie_actions);
    result.extend(joins_trie_actions);
    Ok(result)
}
