//! Typed `Par` constructors/extractors (port of `models/rholang/RhoType.scala`).
//!
//! Each extractor mirrors the Scala `object` of the same name: `apply` builds a single-`Expr`
//! (or single-`GUnforgeable`) `Par`, and `unapply` recovers the underlying Scala value when the
//! `Par` is exactly that shape.

use crate::ast::{
    EList, ETuple, Expr, GDeployId, GDeployerId, GPrivate, GUnforgeable, Par, ParMap, ParSet,
};
use crate::par_ops::{from_expr, is_nil, single_expr, single_unforgeable};

/// Wrap a single unforgeable in a `Par` (the `GUnforgeable` → `Par` implicit).
fn from_unforgeable(u: GUnforgeable) -> Par {
    Par {
        unforgeables: vec![u],
        ..Default::default()
    }
}

/// The `RhoType` namespace (port of `object RhoType`).
#[allow(non_snake_case)]
pub mod RhoType {
    use super::*;

    /// `RhoNil` — the empty process.
    pub struct RhoNil;
    impl RhoNil {
        pub fn apply() -> Par {
            Par::default()
        }
        pub fn unapply(p: &Par) -> bool {
            is_nil(p)
        }
    }

    /// `RhoByteArray`.
    pub struct RhoByteArray;
    impl RhoByteArray {
        pub fn apply(bytes: Vec<u8>) -> Par {
            from_expr(Expr::GByteArray(bytes))
        }
        pub fn unapply(p: &Par) -> Option<&[u8]> {
            match single_expr(p) {
                Some(Expr::GByteArray(bs)) => Some(bs),
                _ => None,
            }
        }
    }

    /// `RhoString`.
    pub struct RhoString;
    impl RhoString {
        pub fn apply(s: String) -> Par {
            from_expr(Expr::GString(s))
        }
        pub fn unapply(p: &Par) -> Option<&str> {
            match single_expr(p) {
                Some(Expr::GString(s)) => Some(s.as_str()),
                _ => None,
            }
        }
    }

    /// `RhoBoolean`.
    pub struct RhoBoolean;
    impl RhoBoolean {
        pub fn apply(b: bool) -> Par {
            from_expr(Expr::GBool(b))
        }
        pub fn unapply(p: &Par) -> Option<bool> {
            match single_expr(p) {
                Some(Expr::GBool(b)) => Some(*b),
                _ => None,
            }
        }
    }

    /// `RhoNumber` (the Scala `Long`).
    pub struct RhoNumber;
    impl RhoNumber {
        pub fn apply(i: i64) -> Par {
            from_expr(Expr::GInt(i))
        }
        pub fn unapply(p: &Par) -> Option<i64> {
            match single_expr(p) {
                Some(Expr::GInt(v)) => Some(*v),
                _ => None,
            }
        }
    }

    /// `RhoUri`.
    pub struct RhoUri;
    impl RhoUri {
        pub fn apply(s: String) -> Par {
            from_expr(Expr::GUri(s))
        }
        pub fn unapply(p: &Par) -> Option<&str> {
            match single_expr(p) {
                Some(Expr::GUri(s)) => Some(s.as_str()),
                _ => None,
            }
        }
    }

    /// `RhoTupleN` — a tuple of processes.
    pub struct RhoTupleN;
    impl RhoTupleN {
        pub fn apply(ps: Vec<Par>) -> Par {
            from_expr(Expr::ETuple(ETuple {
                ps,
                ..ETuple::default()
            }))
        }
        pub fn unapply(p: &Par) -> Option<&[Par]> {
            match single_expr(p) {
                Some(Expr::ETuple(t)) => Some(&t.ps),
                _ => None,
            }
        }
    }

    /// `RhoList` — a list of processes.
    pub struct RhoList;
    impl RhoList {
        pub fn apply(ps: Vec<Par>) -> Par {
            from_expr(Expr::EList(EList {
                ps,
                ..EList::default()
            }))
        }
        pub fn unapply(p: &Par) -> Option<&[Par]> {
            match single_expr(p) {
                Some(Expr::EList(l)) => Some(&l.ps),
                _ => None,
            }
        }
    }

    /// `RhoSet` — a set of processes.
    pub struct RhoSet;
    impl RhoSet {
        pub fn apply(ps: Vec<Par>) -> Par {
            from_expr(Expr::ESet(ParSet {
                ps,
                ..ParSet::default()
            }))
        }
        pub fn unapply(p: &Par) -> Option<&[Par]> {
            match single_expr(p) {
                Some(Expr::ESet(s)) => Some(&s.ps),
                _ => None,
            }
        }
    }

    /// `RhoMap` — a map from process keys to process values.
    pub struct RhoMap;
    impl RhoMap {
        pub fn apply(kvs: Vec<(Par, Par)>) -> Par {
            from_expr(Expr::EMap(ParMap {
                kvs,
                ..ParMap::default()
            }))
        }
        pub fn unapply(p: &Par) -> Option<&[(Par, Par)]> {
            match single_expr(p) {
                Some(Expr::EMap(m)) => Some(&m.kvs),
                _ => None,
            }
        }
    }

    /// `RhoDeployerId`.
    pub struct RhoDeployerId;
    impl RhoDeployerId {
        pub fn apply(bytes: Vec<u8>) -> Par {
            from_unforgeable(GUnforgeable::GDeployerId(GDeployerId {
                public_key: bytes,
            }))
        }
        pub fn unapply(p: &Par) -> Option<&[u8]> {
            match single_unforgeable(p) {
                Some(GUnforgeable::GDeployerId(d)) => Some(&d.public_key),
                _ => None,
            }
        }
    }

    /// `RhoDeployId`.
    pub struct RhoDeployId;
    impl RhoDeployId {
        pub fn apply(bytes: Vec<u8>) -> Par {
            from_unforgeable(GUnforgeable::GDeployId(GDeployId { sig: bytes }))
        }
        pub fn unapply(p: &Par) -> Option<&[u8]> {
            match single_unforgeable(p) {
                Some(GUnforgeable::GDeployId(d)) => Some(&d.sig),
                _ => None,
            }
        }
    }

    /// `RhoName` — an unforgeable `GPrivate`.
    pub struct RhoName;
    impl RhoName {
        pub fn apply(gprivate: GPrivate) -> Par {
            from_unforgeable(GUnforgeable::GPrivate(gprivate))
        }
        pub fn apply_bytes(bytes: Vec<u8>) -> Par {
            Self::apply(GPrivate { id: bytes })
        }
        pub fn unapply(p: &Par) -> Option<&GPrivate> {
            match single_unforgeable(p) {
                Some(GUnforgeable::GPrivate(g)) => Some(g),
                _ => None,
            }
        }
    }

    /// `RhoUnforgeable` — any unforgeable.
    pub struct RhoUnforgeable;
    impl RhoUnforgeable {
        pub fn apply(u: GUnforgeable) -> Par {
            from_unforgeable(u)
        }
        pub fn unapply(p: &Par) -> Option<&GUnforgeable> {
            single_unforgeable(p)
        }
    }

    /// `RhoExpression` — any single expression.
    pub struct RhoExpression;
    impl RhoExpression {
        pub fn apply(e: Expr) -> Par {
            from_expr(e)
        }
        pub fn unapply(p: &Par) -> Option<&Expr> {
            single_expr(p)
        }
    }

    /// `RhoSysAuthToken` — the system auth token unforgeable (unit in the Rust AST).
    pub struct RhoSysAuthToken;
    impl RhoSysAuthToken {
        pub fn apply() -> Par {
            from_unforgeable(GUnforgeable::GSysAuthToken)
        }
        pub fn unapply(p: &Par) -> bool {
            matches!(single_unforgeable(p), Some(GUnforgeable::GSysAuthToken))
        }
    }
}
