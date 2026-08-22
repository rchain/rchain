//! A blocking synchronized variable (port of `shared/SyncVarOps.scala`).
//!
//! Scala's `scala.concurrent.SyncVar` (an empty-or-full cell with blocking `take`/`put`) is
//! simplified to a `Mutex<Option<A>>` + `Condvar`.

use std::sync::{Condvar, Mutex, MutexGuard};

/// A blocking cell that is either empty or holds a value (port of `SyncVar`).
#[derive(Debug)]
pub struct SyncVar<A> {
    state: Mutex<Option<A>>,
    ready: Condvar,
}

impl<A> SyncVar<A> {
    /// A cell pre-filled with `a` (port of `SyncVarOps.create`).
    pub fn create(a: A) -> Self {
        SyncVar {
            state: Mutex::new(Some(a)),
            ready: Condvar::new(),
        }
    }

    /// Take the value, apply `f`, and put the result back (port of `RichSyncVar.update`).
    pub fn update(&self, f: impl FnOnce(A) -> A) {
        let curr = self.take();
        self.put(f(curr));
    }

    /// Block until non-empty, then return and remove the value.
    pub fn take(&self) -> A {
        let mut guard = lock(&self.state);
        loop {
            if let Some(a) = guard.take() {
                self.ready.notify_one();
                return a;
            }
            guard = self.ready.wait(guard).unwrap_or_else(|p| p.into_inner());
        }
    }

    /// Block until empty, then set the value.
    pub fn put(&self, a: A) {
        let mut guard = lock(&self.state);
        while guard.is_some() {
            guard = self.ready.wait(guard).unwrap_or_else(|p| p.into_inner());
        }
        *guard = Some(a);
        self.ready.notify_one();
    }
}

fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|p| p.into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::thread;

    #[test]
    fn create_put_and_take_round_trip() {
        let var = SyncVar::create(1);
        assert_eq!(var.take(), 1);
        var.put(2);
        assert_eq!(var.take(), 2);
    }

    #[test]
    fn update_applies_the_function() {
        let var = SyncVar::create(1);
        var.update(|x| x + 1);
        assert_eq!(var.take(), 2);
    }

    #[test]
    fn take_blocks_until_put() {
        let var = Arc::new(SyncVar::<i32>::create(0));
        var.take(); // now empty

        let producer = Arc::clone(&var);
        let handle = thread::spawn(move || {
            producer.put(42);
        });

        assert_eq!(var.take(), 42);
        handle.join().unwrap();
    }
}
