//! The `Sorted` refinement: Law 1's canonical order carried by a type.
//!
//! `Par` derives `Eq`/`Ord` on its raw (unsorted) fields, so order-sensitive equality and a
//! different content hash are representable unless `sort_par_term` has been applied. `Sorted` makes
//! the canonical order structural: it is constructed only by [`Sorted::new`] (which sorts), and its
//! `Eq`/`Ord`/`Hash`/`Serialize` are canonical by construction.

use std::hash::{Hash, Hasher};

use rchain_shared::serialize::Serialize;

use crate::ast::{NameSort, Par, ProcSort, Sort};
use crate::sorter::sort_par_term;

/// A canonically-sorted process (a `Par` in process position).
pub type SortedProc = Sorted<ProcSort>;
/// A canonically-sorted name (a `Par` in name position).
pub type SortedName = Sorted<NameSort>;

/// A canonically-sorted `Par`.
///
/// Invariant: `self.0 == sort_par_term(&self.0)`, established once by [`Sorted::new`] and preserved
/// because the inner `Par` is never exposed mutably. The derived `Eq`/`Ord` (on the sorted inner)
/// and the `Hash`/`Serialize` (on the canonical bytes) are therefore order-insensitive.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Sorted<S: Sort>(Par<S>);

impl<S: Sort> Sorted<S> {
    /// Canonicalize a `Par` (total: the sorted form is always canonical).
    pub fn new(par: Par<S>) -> Sorted<S> {
        Sorted(sort_par_term(&par))
    }

    /// The canonical (sorted) term.
    pub fn as_par(&self) -> &Par<S> {
        &self.0
    }
}

/// One-way boundary discharge: the sorted term re-enters the general `Par`.
impl<S: Sort> From<Sorted<S>> for Par<S> {
    fn from(s: Sorted<S>) -> Par<S> {
        s.0
    }
}

/// The empty process is already canonical.
impl<S: Sort> Default for Sorted<S> {
    fn default() -> Self {
        Sorted(Par::default())
    }
}

/// serde round-trips the sorted inner `Par`; deserialization re-sorts so the invariant holds across
/// a JSON round-trip.
impl<S: Sort> serde::Serialize for Sorted<S> {
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'de, S: Sort> serde::Deserialize<'de> for Sorted<S> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Par::<S>::deserialize(deserializer).map(Sorted::new)
    }
}

/// `Hash` on the canonical serialized form (order-insensitive; avoids deriving `Hash` on `Par` and
/// its sub-types, which don't implement it).
impl<S: Sort> Hash for Sorted<S> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        <Par<S> as Serialize<Par<S>>>::encode(&self.0).hash(state);
    }
}

/// Serialization of the already-sorted inner `Par`; `decode` re-sorts so the invariant is preserved
/// across a wire round-trip.
impl<S: Sort> Serialize<Sorted<S>> for Sorted<S> {
    fn encode(a: &Sorted<S>) -> Vec<u8> {
        <Par<S> as Serialize<Par<S>>>::encode(&a.0)
    }

    fn decode(bytes: &[u8]) -> Result<Sorted<S>, String> {
        <Par<S> as Serialize<Par<S>>>::decode(bytes).map(Sorted::new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr;

    fn par(exprs: Vec<Expr>) -> Par {
        Par {
            exprs,
            ..Default::default()
        }
    }

    #[test]
    fn sorted_is_order_insensitive() {
        let a = Sorted::new(par(vec![Expr::GInt(1), Expr::GInt(2)]));
        let b = Sorted::new(par(vec![Expr::GInt(2), Expr::GInt(1)]));
        assert_eq!(a, b);
    }
}
