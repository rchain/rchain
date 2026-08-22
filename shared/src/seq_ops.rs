//! Sequence operations (port of `shared/SeqOps.scala`).

/// Drops the `n`-th element of a sequence (port of `SeqOps.dropIndex`).
///
/// Panics if `n` is out of bounds, mirroring the original's `IndexOutOfBoundsException`.
pub fn drop_index<T: Clone>(xs: &[T], n: usize) -> Vec<T> {
    let mut out = xs.to_vec();
    out.remove(n);
    out
}

/// Removes the first occurrence of an element matching the predicate (port of
/// `SeqOps.removeFirst`).
pub fn remove_first<T: Clone>(xs: &[T], p: impl Fn(&T) -> bool) -> Vec<T> {
    match xs.iter().position(p) {
        Some(i) => {
            let mut out = xs.to_vec();
            out.remove(i);
            out
        }
        None => xs.to_vec(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn drop_index_removes_the_nth_element() {
        assert_eq!(drop_index(&[1, 2, 3, 4], 1), vec![1, 3, 4]);
        assert_eq!(drop_index(&[1, 2, 3], 0), vec![2, 3]);
        assert_eq!(drop_index(&[1, 2, 3], 2), vec![1, 2]);
    }

    #[test]
    #[should_panic]
    fn drop_index_panics_out_of_bounds() {
        drop_index(&[1, 2, 3], 3);
    }

    #[test]
    fn remove_first_removes_first_match() {
        assert_eq!(remove_first(&[1, 2, 3, 2], |&x| x == 2), vec![1, 3, 2]);
        assert_eq!(remove_first(&[1, 2, 3], |&x| x == 9), vec![1, 2, 3]);
    }
}
