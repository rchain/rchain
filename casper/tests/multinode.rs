//! Multi-node consensus integration tests (CBC-Casper DAG convergence).
//!
//! Mirrors the Scala `batch1`/`addblock` specs at the DAG-message layer: N bonded validators
//! propose interleaved messages and converge on a shared view (the `seen` closure each message
//! carries).
//!
//! NOTE: asserting *finalization* (fringe advancement) requires (a) the full `BlockCreator` →
//! `validate` → `insert` pipeline, whose `bondsCache` step reads the PoS bonds back from the
//! post-state (blocked on the genesis blessed terms / rholang parser), and (b) the specific
//! "full-partition" block structure the `Finalizer` needs. Until those land, this exercises the
//! convergence + monotonicity invariants on the real `DagMessageState`, which is where finality is
//! ultimately decided.

use std::collections::{BTreeMap, BTreeSet};

use rchain_block_storage::dag::message_state::DagMessageState;
use rchain_models::block_hash::BlockHash;
use rchain_models::validator::Validator;
use rchain_shared::refined::{BlockHeight, NonNegI64, SeqNum};

fn validator(byte: u8) -> Validator {
    Validator::new([byte; 65])
}

/// A deterministic message id from (sender, height).
fn msg_id(sender: &Validator, height: BlockHeight) -> BlockHash {
    let mut bytes = [0u8; 32];
    bytes[0] = sender.as_bytes()[0];
    bytes[1] = sender.as_bytes()[1];
    bytes[2] = (i64::from(height) & 0xff) as u8;
    bytes[3] = ((i64::from(height) >> 8) & 0xff) as u8;
    BlockHash::new(bytes)
}

/// Scala `test_finalizes_block` (convergence slice): three bonded validators (60/20/15) propose
/// interleaved messages; each message justifies the current latest messages, so every later message
/// sees the full shared prefix.
#[test]
fn three_validators_converge_on_shared_view() {
    let v0 = validator(1);
    let v1 = validator(2);
    let v2 = validator(3);
    let bonds: BTreeMap<Validator, NonNegI64> = [
        (v0.clone(), NonNegI64::try_from(60).unwrap()),
        (v1.clone(), NonNegI64::try_from(20).unwrap()),
        (v2.clone(), NonNegI64::try_from(15).unwrap()),
    ]
    .into_iter()
    .collect();

    let state: DagMessageState<BlockHash, Validator> = DagMessageState::empty();

    // Genesis: a single message from v0 with empty justifications.
    let genesis = state
        .create_message(
            msg_id(&v0, BlockHeight::zero()),
            BlockHeight::zero(),
            v0.clone(),
            SeqNum::zero(),
            bonds.clone(),
            &BTreeSet::new(),
        );
    let mut state = state.insert_msg(&genesis);

    // Interleave proposals; each message justifies the current latest messages.
    let mut ids = vec![genesis.id];
    let order = [&v0, &v1, &v2, &v0, &v1, &v2, &v0];
    for creator in order {
        let (next, msg) = state
            .create_msg_and_update_sender(creator, |s, h| msg_id(s, h))
            .expect("create message");
        state = next;
        ids.push(msg.id);
    }

    // Every message is tracked in the message map.
    for id in &ids {
        assert!(state.msg_map.contains_key(id), "message {id:?} tracked");
    }

    // The highest message sees the entire shared prefix (convergence: no validator misses a block).
    let tip = state.latest_msgs.values().max_by_key(|m| m.height).unwrap();
    for id in &ids {
        assert!(tip.seen.contains(id), "tip {:?} sees {id:?}", tip.id);
    }

    // Law 15: one latest message per sender, and the retained one has the highest sender_seq.
    let senders: BTreeSet<Validator> = state.latest_msgs.values().map(|m| m.sender.clone()).collect();
    assert_eq!(senders.len(), 3, "one latest message per validator");
    for m in state.latest_msgs.values() {
        let max_seq = ids
            .iter()
            .filter_map(|id| state.msg_map.get(id))
            .filter(|msg| msg.sender == m.sender)
            .map(|msg| msg.sender_seq)
            .max()
            .unwrap();
        assert_eq!(m.sender_seq, max_seq, "latest message has the max sender_seq");
    }
}
