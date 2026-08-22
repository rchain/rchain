//! Poison-aware lock accessors.
//!
//! The engine uses `std::sync::RwLock`/`Mutex` for shared mutable state. A lock is only poisoned if
//! a panic occurred while it was held; these accessors recover the guard via `PoisonError::into_inner`
//! instead of panicking, making the poison recovery explicit and total (per `TYPE-SYSTEM.md` §3.2).

use std::sync::{Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};

/// Acquire a read guard, recovering from poison.
pub(crate) fn rlock<T>(l: &RwLock<T>) -> RwLockReadGuard<'_, T> {
    l.read().unwrap_or_else(std::sync::PoisonError::into_inner)
}

/// Acquire a write guard, recovering from poison.
pub(crate) fn wlock<T>(l: &RwLock<T>) -> RwLockWriteGuard<'_, T> {
    l.write().unwrap_or_else(std::sync::PoisonError::into_inner)
}

/// Acquire a mutex guard, recovering from poison.
pub(crate) fn mlock<T>(l: &Mutex<T>) -> MutexGuard<'_, T> {
    l.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
}
