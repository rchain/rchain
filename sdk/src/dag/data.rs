//! DAG management interfaces (port of `sdk/dag/data/{DagData,DagManager,DagView}.scala`).

use rchain_shared::refined::NonNegI64;

/// Field accessors for messages and senders (port of `DagData[M, MId, S, SId]`).
pub trait DagData<M, MId, S, SId> {
    fn mid(&self, m: &M) -> MId;

    fn seq_num(&self, m: &M) -> i64;

    fn block_num(&self, m: &M) -> i64;

    fn justifications(&self, m: &M) -> Vec<MId>;

    fn sender(&self, m: &M) -> SId;

    fn bonds_map(&self, m: &M) -> Vec<(SId, NonNegI64)>;

    fn sid(&self, s: &S) -> SId;
}

/// High-level module for managing DAG persistent state and producing read-only views (port of
/// `DagManager[F, M, MId, S, SId]`; the `F[_]` effect is simplified to synchronous calls).
pub trait DagManager<M, MId, S, SId> {
    /// Returns a read-only DAG view starting from the specified latest message.
    fn get_dag_view(&self, seen_by: &MId) -> Box<dyn DagView<M, MId, S, SId>>;

    /// Latest messages (tips) seen in the whole DAG.
    fn latest_messages(&self) -> Vec<(S, Vec<M>)>;

    /// Thread-safe insert of a new message with the corresponding finalized messages.
    fn insert(&self, msg: M, finalized: Vec<MId>, provisionally_finalized: bool);

    fn load_message(&self, mid: &MId) -> M;

    fn load_sender(&self, sid: &SId) -> S;
}

/// A read-only view of the DAG (port of `DagView[F, M, MId, S, SId]`; the `F[_]` effect and
/// `fs2.Stream` are simplified to synchronous calls and a `Vec`).
pub trait DagView<M, MId, S, SId> {
    /// The top message from which this view is seen.
    fn seen_by(&self) -> M;

    /// Traversal through the DAG: `(message, parents)` pairs.
    fn messages(&self) -> Vec<(M, Vec<M>)>;

    fn load_message(&self, mid: &MId) -> M;

    fn load_sender(&self, sid: &SId) -> S;
}

/// The chain of self-justifications of a message (port of
/// `DagViewSyntax.selfJustificationChain`). Returns the message's justification from the same
/// sender, then that message's own, and so on (the seed message is excluded).
pub fn self_justification_chain<M: Clone, MId, S, SId: PartialEq>(
    dag_view: &dyn DagView<M, MId, S, SId>,
    dag_data: &dyn DagData<M, MId, S, SId>,
    message: M,
) -> Vec<M> {
    let mut chain = Vec::new();
    let mut current = message;
    loop {
        let sender = dag_data.sender(&current);
        let mut next = None;
        for mid in dag_data.justifications(&current) {
            let candidate = dag_view.load_message(&mid);
            if dag_data.sender(&candidate) == sender {
                next = Some(candidate);
                break;
            }
        }
        match next {
            Some(m) => {
                chain.push(m.clone());
                current = m;
            }
            None => break,
        }
    }
    chain
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockData;

    impl DagData<i32, i32, i32, i32> for MockData {
        fn mid(&self, m: &i32) -> i32 {
            *m
        }
        fn seq_num(&self, m: &i32) -> i64 {
            *m as i64
        }
        fn block_num(&self, m: &i32) -> i64 {
            *m as i64
        }
        fn justifications(&self, m: &i32) -> Vec<i32> {
            if *m == 0 {
                vec![]
            } else {
                vec![m - 1]
            }
        }
        fn sender(&self, _m: &i32) -> i32 {
            0
        }
        fn bonds_map(&self, _m: &i32) -> Vec<(i32, NonNegI64)> {
            vec![]
        }
        fn sid(&self, s: &i32) -> i32 {
            *s
        }
    }

    struct MockView;

    impl DagView<i32, i32, i32, i32> for MockView {
        fn seen_by(&self) -> i32 {
            0
        }
        fn messages(&self) -> Vec<(i32, Vec<i32>)> {
            vec![]
        }
        fn load_message(&self, mid: &i32) -> i32 {
            *mid
        }
        fn load_sender(&self, sid: &i32) -> i32 {
            *sid
        }
    }

    #[test]
    fn self_justification_chain_follows_same_sender() {
        let chain = self_justification_chain(&MockView, &MockData, 2);
        assert_eq!(chain, vec![1, 0]);
    }

    #[test]
    fn self_justification_chain_is_empty_for_genesis() {
        let chain = self_justification_chain(&MockView, &MockData, 0);
        assert!(chain.is_empty());
    }
}
