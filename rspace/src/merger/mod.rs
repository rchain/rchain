//! Event-log merging (Law 9: merge is a monoid; non-conflicting logs commute).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/merger/`.

pub mod channel_change;
pub mod event_log_index;
pub mod event_log_merging_logic;
pub mod state_change;
pub mod state_change_merger;

/// Multiset difference of two slices (port of Scala `Seq.diff`): remove the first occurrence of
/// each element of `b` from `a`, preserving order.
pub(crate) fn seq_diff<T: PartialEq + Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut out: Vec<T> = a.to_vec();
    for item in b {
        if let Some(pos) = out.iter().position(|x| x == item) {
            out.remove(pos);
        }
    }
    out
}
