//! Primitive-type extensions (port of `sdk/primitive/*.scala`).

use std::collections::BTreeMap;
use std::error::Error;

/// Map extensions (port of `MapSyntax`; the Scala immutable/mutable split is collapsed onto
/// `BTreeMap`).
pub trait MapOps<K, V> {
    /// Get a value, panicking if the key is absent (port of `getUnsafe`).
    fn get_unsafe(&self, key: &K) -> &V;
}

impl<K: Ord + std::fmt::Debug, V> MapOps<K, V> for BTreeMap<K, V> {
    fn get_unsafe(&self, key: &K) -> &V {
        self.get(key)
            .unwrap_or_else(|| panic!("No key {key:?} in a map."))
    }
}

/// Error extensions (port of `ThrowableSyntax`).
pub trait ThrowableOps {
    /// The error message, or the `to_string` form when the message is null (port of
    /// `getMessageSafe`). In Rust the message is never null, so this is `to_string`.
    fn get_message_safe(&self) -> String;
}

impl<E: Error> ThrowableOps for E {
    fn get_message_safe(&self) -> String {
        self.to_string()
    }
}

/// `Result` extensions (port of `TrySyntax`).
pub trait TryOps<T, E> {
    /// Get the value, panicking on error (port of `getUnsafe`).
    fn get_unsafe(self) -> T;

    /// Map the error case (port of `mapFailure`).
    fn map_failure(self, f: impl FnOnce(E) -> E) -> Result<T, E>;
}

impl<T, E: std::fmt::Debug> TryOps<T, E> for Result<T, E> {
    fn get_unsafe(self) -> T {
        self.unwrap()
    }

    fn map_failure(self, f: impl FnOnce(E) -> E) -> Result<T, E> {
        self.map_err(f)
    }
}

/// Discard a value (port of `VoidSyntax`).
pub trait VoidOps {
    fn void(self);
}

impl<T> VoidOps for T {
    fn void(self) {
        let _ = self;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    #[test]
    fn map_get_unsafe_returns_value() {
        let mut m = BTreeMap::new();
        m.insert(1, 10);
        assert_eq!(*m.get_unsafe(&1), 10);
    }

    #[test]
    #[should_panic]
    fn map_get_unsafe_panics_on_missing() {
        let m: BTreeMap<i32, i32> = BTreeMap::new();
        let _ = m.get_unsafe(&1);
    }

    #[test]
    fn throwable_message_safe_returns_message() {
        let e = std::io::Error::other("boom");
        assert_eq!(e.get_message_safe(), "boom");
    }

    #[test]
    fn try_get_unsafe_returns_value() {
        let ok: Result<i32, String> = Ok(5);
        assert_eq!(ok.get_unsafe(), 5);
    }

    #[test]
    #[should_panic]
    fn try_get_unsafe_panics_on_error() {
        let err: Result<i32, String> = Err("boom".to_string());
        let _ = err.get_unsafe();
    }

    #[test]
    fn try_map_failure_maps_error() {
        let err: Result<i32, String> = Err("boom".to_string());
        assert_eq!(
            err.map_failure(|e| format!("{e}!")),
            Err("boom!".to_string())
        );
    }

    #[test]
    fn void_discards_value() {
        let _: () = 42.void();
        let _: () = String::from("x").void();
    }
}
