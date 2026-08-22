//! Monix effect bridge.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/monix/Monixable.scala`. Monix `Task` has no Rust
//! analogue, so this is a documented, identity-shaped shim: `to_task`/`from_task` collapse to the
//! identity conversion. The concrete effect runtime for the Rust port is tokio (see
//! [`crate::typed_store`]), which has no need for a `Task` bridge.

/// Bridge between an abstract effect and a concrete runtime (port of `Monixable[F]`); identity in
/// Rust because there is no separate `Task` type.
pub trait Monixable<T>: Sized {
    fn to_task(self) -> T;
    fn from_task(task: T) -> Self;
}

/// The identity instance (port of `Monixable.MonixableTask`).
impl<T> Monixable<T> for T {
    fn to_task(self) -> T {
        self
    }
    fn from_task(task: T) -> Self {
        task
    }
}
