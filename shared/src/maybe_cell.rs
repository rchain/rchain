//! A mutable `Option` cell (port of `shared/MaybeCell.scala`).
//!
//! The cats-effect `Ref[F, Option[A]]` is simplified to a `Mutex<Option<A>>`.

use std::sync::{Mutex, MutexGuard};

/// A cell that is either empty or holds a value (port of `MaybeCell[F, A]`).
#[derive(Debug)]
pub struct MaybeCell<A> {
    state: Mutex<Option<A>>,
}

impl<A> MaybeCell<A> {
    /// An empty cell (port of `MaybeCell.of`).
    pub fn empty() -> Self {
        MaybeCell {
            state: Mutex::new(None),
        }
    }

    /// A cell initialized with `init` (port of `MaybeCell.unsafe`).
    pub fn new(init: Option<A>) -> Self {
        MaybeCell {
            state: Mutex::new(init),
        }
    }

    /// Read the current value (port of `get`).
    pub fn get(&self) -> Option<A>
    where
        A: Clone,
    {
        lock(&self.state).clone()
    }

    /// Set the value (port of `set`).
    pub fn set(&self, a: A) {
        *lock(&self.state) = Some(a);
    }
}

fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|p| p.into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_cell_reads_none() {
        let cell = MaybeCell::<i32>::empty();
        assert_eq!(cell.get(), None);
    }

    #[test]
    fn set_then_get_round_trips() {
        let cell = MaybeCell::new(Some(1));
        assert_eq!(cell.get(), Some(1));
        cell.set(2);
        assert_eq!(cell.get(), Some(2));
    }

    #[test]
    fn new_with_none_is_empty() {
        let cell = MaybeCell::<i32>::new(None);
        assert_eq!(cell.get(), None);
    }
}
