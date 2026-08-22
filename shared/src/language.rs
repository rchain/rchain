//! Language helpers (port of `shared/Language.scala`).

/// Run a computation for its side effects, discarding its value (port of `Language.ignore`).
pub fn ignore<A>(a: impl FnOnce() -> A) {
    let _ = a();
}

/// Remove the element at `index`, leaving the sequence unchanged for out-of-range indices
/// (port of `Language.removeIndex`).
pub fn remove_index<E: Clone>(col: &[E], index: usize) -> Vec<E> {
    let mut out = col.to_vec();
    if index < out.len() {
        out.remove(index);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignore_runs_and_discards() {
        let mut called = false;
        ignore(|| {
            called = true;
            42
        });
        assert!(called);
    }

    #[test]
    fn remove_index_removes_element() {
        assert_eq!(remove_index(&[1, 2, 3, 4], 1), vec![1, 3, 4]);
        assert_eq!(remove_index(&[1, 2, 3], 0), vec![2, 3]);
    }

    #[test]
    fn remove_index_clamps_out_of_range() {
        assert_eq!(remove_index(&[1, 2, 3], 3), vec![1, 2, 3]);
        assert_eq!(remove_index(&[1, 2, 3], 5), vec![1, 2, 3]);
    }
}
