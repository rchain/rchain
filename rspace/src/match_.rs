//! Pattern-matching typeclass.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/Match.scala`.

/// Typeclass for matching a pattern against a datum (port of `Match[F, P, A]`).
pub trait Match<P, A>: Send + Sync {
    fn get(&self, p: &P, a: &A) -> Option<A>;
}
