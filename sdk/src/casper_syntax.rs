//! Casper DAG validation predicates (port of
//! `sdk/casper/syntax/{CasperDagDataSyntax,CasperDagViewSyntax}.scala`).
//!
//! The Scala syntax extensions are ported as plain functions over the already-ported `DagData`
//! and `DagView` traits; the cats-effect `F[_]`/`fs2.Stream` effects are simplified to synchronous
//! calls.

use std::collections::BTreeSet;

use rchain_shared::refined::NonNegI64;

use crate::dag::data::{DagData, DagView};

/// Message sender should be present and have non-zero stake in the bonds map (port of
/// `CasperDagDataMessageOps.inactiveSender`).
pub fn inactive_sender<M, MId, S, SId: PartialEq>(
    dag_data: &dyn DagData<M, MId, S, SId>,
    msg: &M,
) -> bool {
    let sender = dag_data.sender(msg);
    !dag_data
        .bonds_map(msg)
        .iter()
        .any(|(s, stake)| s == &sender && *stake > NonNegI64::zero())
}

/// Message should have sequence number equal to the sequence number of its self justification + 1
/// (port of `CasperDagDataMessageOps.checkSequenceNumber`).
pub fn check_sequence_number<M, MId, S, SId>(
    dag_data: &dyn DagData<M, MId, S, SId>,
    msg: &M,
    self_justification: &M,
) -> bool {
    dag_data.seq_num(msg) != dag_data.seq_num(self_justification) + 1
}

/// Message should have block number equal to the block number of the highest justification + 1
/// (port of `CasperDagViewOps.checkBlockNumber`).
///
/// The original calls `List.max` on the justification block numbers, which throws on an empty list;
/// the port yields `0` for a justification-free message instead.
pub fn check_block_number<M, MId, S, SId>(
    dag_view: &dyn DagView<M, MId, S, SId>,
    dag_data: &dyn DagData<M, MId, S, SId>,
    msg: &M,
) -> bool {
    let next = dag_data
        .justifications(msg)
        .iter()
        .map(|mid| dag_view.load_message(mid))
        .map(|m| dag_data.block_num(&m))
        .max()
        .map(|max| max + 1)
        .unwrap_or(0);
    next != dag_data.block_num(msg)
}

/// Message should not have justifications past the justification of its previous message from the
/// same sender (port of `CasperDagViewOps.invalidJustificationRegression`).
///
/// Fails with the original's message when the message has no self justification. Ported faithfully:
/// the upstream implementation discards its `.map { ... }` result and returns `js.nonEmpty` (upstream
/// TODO: "Check this function! it created as an example for DagManager").
pub fn invalid_justification_regression<M, MId, S, SId: PartialEq>(
    dag_view: &dyn DagView<M, MId, S, SId>,
    dag_data: &dyn DagData<M, MId, S, SId>,
    msg: &M,
) -> Result<bool, String> {
    let js: Vec<M> = dag_data
        .justifications(msg)
        .iter()
        .map(|mid| dag_view.load_message(mid))
        .collect();
    let sender = dag_data.sender(msg);
    let self_j = js
        .iter()
        .find(|j| dag_data.sender(j) == sender)
        .ok_or_else(|| "Message does not have self justification.".to_string())?;

    // Loaded in the original, but its contents are unused by the (discarded) map result.
    let _self_jjs: Vec<M> = dag_data
        .justifications(self_j)
        .iter()
        .map(|mid| dag_view.load_message(mid))
        .collect();

    Ok(!js.is_empty())
}

/// Message should have a justification for each sender in the bonds map (port of
/// `CasperDagViewOps.invalidJustificationFollows`).
pub fn invalid_justification_follows<M, MId, S, SId: Ord>(
    dag_view: &dyn DagView<M, MId, S, SId>,
    dag_data: &dyn DagData<M, MId, S, SId>,
    msg: &M,
    bonded_senders: &BTreeSet<SId>,
) -> bool {
    let senders: BTreeSet<SId> = dag_data
        .justifications(msg)
        .iter()
        .map(|mid| dag_view.load_message(mid))
        .map(|m| dag_data.sender(&m))
        .collect();
    &senders != bonded_senders
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    #[derive(Clone, Debug, PartialEq)]
    struct Msg {
        sender: i32,
        seq: i64,
        block: i64,
        justifications: Vec<i32>,
        bonds: Vec<(i32, NonNegI64)>,
    }

    impl Msg {
        fn new(sender: i32, seq: i64, block: i64) -> Self {
            Msg {
                sender,
                seq,
                block,
                justifications: vec![],
                bonds: vec![],
            }
        }
    }

    struct MockData;

    impl DagData<Msg, i32, i32, i32> for MockData {
        fn mid(&self, _m: &Msg) -> i32 {
            0
        }
        fn seq_num(&self, m: &Msg) -> i64 {
            m.seq
        }
        fn block_num(&self, m: &Msg) -> i64 {
            m.block
        }
        fn justifications(&self, m: &Msg) -> Vec<i32> {
            m.justifications.clone()
        }
        fn sender(&self, m: &Msg) -> i32 {
            m.sender
        }
        fn bonds_map(&self, m: &Msg) -> Vec<(i32, NonNegI64)> {
            m.bonds.clone()
        }
        fn sid(&self, s: &i32) -> i32 {
            *s
        }
    }

    struct MockView {
        msgs: BTreeMap<i32, Msg>,
    }

    impl DagView<Msg, i32, i32, i32> for MockView {
        fn seen_by(&self) -> Msg {
            unreachable!()
        }
        fn messages(&self) -> Vec<(Msg, Vec<Msg>)> {
            vec![]
        }
        fn load_message(&self, mid: &i32) -> Msg {
            self.msgs.get(mid).cloned().expect("missing message")
        }
        fn load_sender(&self, sid: &i32) -> i32 {
            *sid
        }
    }

    #[test]
    fn inactive_sender_detects_missing_or_zero_stake_bond() {
        let active = Msg {
            sender: 1,
            bonds: vec![(1, NonNegI64::try_from(10).unwrap())],
            ..Msg::new(1, 1, 1)
        };
        assert!(!inactive_sender(&MockData, &active));

        let zero_stake = Msg {
            sender: 1,
            bonds: vec![(1, NonNegI64::zero())],
            ..Msg::new(1, 1, 1)
        };
        assert!(inactive_sender(&MockData, &zero_stake));

        let absent = Msg {
            sender: 2,
            bonds: vec![(1, NonNegI64::try_from(10).unwrap())],
            ..Msg::new(2, 1, 1)
        };
        assert!(inactive_sender(&MockData, &absent));
    }

    #[test]
    fn check_sequence_number_detects_wrong_seq() {
        let msg = Msg::new(1, 3, 3);
        let self_j = Msg::new(1, 2, 2);
        assert!(!check_sequence_number(&MockData, &msg, &self_j));

        let wrong = Msg::new(1, 5, 5);
        assert!(check_sequence_number(&MockData, &wrong, &self_j));
    }

    #[test]
    fn check_block_number_detects_wrong_block() {
        let parent = Msg::new(1, 1, 4);
        let mut view = MockView {
            msgs: BTreeMap::new(),
        };
        view.msgs.insert(10, parent);

        let msg = Msg {
            sender: 1,
            seq: 2,
            block: 5,
            justifications: vec![10],
            bonds: vec![],
        };
        assert!(!check_block_number(&view, &MockData, &msg));

        let wrong = Msg {
            sender: 1,
            seq: 2,
            block: 7,
            justifications: vec![10],
            bonds: vec![],
        };
        assert!(check_block_number(&view, &MockData, &wrong));
    }

    #[test]
    fn invalid_justification_regression_requires_self_justification() {
        let no_self = Msg::new(1, 1, 1);
        let view = MockView {
            msgs: BTreeMap::new(),
        };
        assert!(invalid_justification_regression(&view, &MockData, &no_self).is_err());

        let self_j = Msg::new(1, 1, 1);
        let mut view = MockView {
            msgs: BTreeMap::new(),
        };
        view.msgs.insert(10, self_j);
        let msg = Msg {
            sender: 1,
            seq: 2,
            block: 2,
            justifications: vec![10],
            bonds: vec![],
        };
        assert_eq!(
            invalid_justification_regression(&view, &MockData, &msg),
            Ok(true)
        );
    }

    #[test]
    fn invalid_justification_follows_compares_sender_set() {
        let mut view = MockView {
            msgs: BTreeMap::new(),
        };
        view.msgs.insert(10, Msg::new(1, 1, 1));
        view.msgs.insert(20, Msg::new(2, 1, 1));

        let msg = Msg {
            sender: 1,
            seq: 2,
            block: 2,
            justifications: vec![10, 20],
            bonds: vec![],
        };
        let bonded: BTreeSet<i32> = [1, 2].into_iter().collect();
        assert!(!invalid_justification_follows(&view, &MockData, &msg, &bonded));

        let bonded_missing: BTreeSet<i32> = [1, 2, 3].into_iter().collect();
        assert!(invalid_justification_follows(
            &view,
            &MockData,
            &msg,
            &bonded_missing
        ));
    }
}
