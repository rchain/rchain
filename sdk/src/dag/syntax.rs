//! DAG field-access syntax (port of `sdk/dag/syntax/DagDataSyntax.scala`).
//!
//! Scala's implicit-resolution boilerplate for field-like access to the opaque `M`/`S` types; the
//! Rust port delegates straight to `DagData`.

use rchain_shared::refined::NonNegI64;

use crate::dag::data::DagData;

/// Field accessors on a message `M` (port of `DagDataMessageOps`).
pub trait DagDataMessageOps<M, MId, S, SId> {
    fn mid(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> MId;

    fn seq_num(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> i64;

    fn block_num(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> i64;

    fn justifications(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> Vec<MId>;

    fn sender(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> SId;

    fn bonds_map(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> Vec<(SId, NonNegI64)>;
}

impl<M, MId, S, SId> DagDataMessageOps<M, MId, S, SId> for M {
    fn mid(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> MId {
        dag_data.mid(self)
    }

    fn seq_num(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> i64 {
        dag_data.seq_num(self)
    }

    fn block_num(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> i64 {
        dag_data.block_num(self)
    }

    fn justifications(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> Vec<MId> {
        dag_data.justifications(self)
    }

    fn sender(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> SId {
        dag_data.sender(self)
    }

    fn bonds_map(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> Vec<(SId, NonNegI64)> {
        dag_data.bonds_map(self)
    }
}

/// Field accessor on a sender `S` (port of `DagDataSenderOps`).
pub trait DagDataSenderOps<M, MId, S, SId> {
    fn sid(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> SId;
}

impl<M, MId, S, SId> DagDataSenderOps<M, MId, S, SId> for S {
    fn sid(&self, dag_data: &dyn DagData<M, MId, S, SId>) -> SId {
        dag_data.sid(self)
    }
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
        fn justifications(&self, _m: &i32) -> Vec<i32> {
            vec![]
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

    #[test]
    fn message_ops_delegate_to_dag_data() {
        let m = 5i32;
        assert_eq!(m.mid(&MockData), 5);
        assert_eq!(m.seq_num(&MockData), 5);
        assert_eq!(m.block_num(&MockData), 5);
        assert_eq!(m.sender(&MockData), 0);
        assert!(m.justifications(&MockData).is_empty());
    }

    #[test]
    fn sender_ops_delegate_to_dag_data() {
        let s = 7i32;
        assert_eq!(s.sid(&MockData), 7);
    }
}
