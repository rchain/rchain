//! `locallyFree` / `connectiveUsed` computation and `Par` builder helpers.
//!
//! Mirrors `models/src/main/scala/coop/rchain/models/rholang/implicits.scala` (`HasLocallyFree`
//! instances, `prepend`, `++`, `single*`). The free-variable level cache is a sorted `Vec<i32>`
//! (the Scala `scala.collection.immutable.BitSet`).

use std::collections::BTreeSet;

use crate::ast::{
    AlwaysEqual, BitSet, Bundle, Connective, Expr, GUnforgeable, Match, New, Par, Receive, Send,
    Sort, SortJoin, Var, VarRef,
};

fn union_free(a: &[i32], b: &[i32]) -> Vec<i32> {
    let mut set: BTreeSet<i32> = a.iter().copied().collect();
    set.extend(b.iter().copied());
    set.into_iter().collect()
}

/// The Scala `BitSet.until(n)` — keep only levels `< n`.
pub fn until_free(b: &[i32], n: i32) -> Vec<i32> {
    b.iter().copied().filter(|&x| x < n).collect()
}

// --- Var ---------------------------------------------------------------

pub fn locally_free_of_var(v: &Var, depth: i32) -> BitSet {
    match v {
        Var::BoundVar(index) if depth == 0 => vec![*index],
        Var::BoundVar(_) | Var::FreeVar(_) | Var::Wildcard | Var::Empty => Vec::new(),
    }
}

pub fn connective_used_of_var(v: &Var) -> bool {
    matches!(v, Var::FreeVar(_) | Var::Wildcard)
}

// --- Expr --------------------------------------------------------------

pub fn locally_free_of_expr(e: &Expr, depth: i32) -> BitSet {
    match e {
        Expr::GBool(_)
        | Expr::GInt(_)
        | Expr::GBigInt(_)
        | Expr::GString(_)
        | Expr::GUri(_)
        | Expr::GByteArray(_) => Vec::new(),
        Expr::EList(e) => e.locally_free.0.clone(),
        Expr::ETuple(e) => e.locally_free.0.clone(),
        Expr::ESet(e) => e.locally_free.0.clone(),
        Expr::EMap(e) => e.locally_free.0.clone(),
        Expr::EVar(v) => locally_free_of_var(v, depth),
        Expr::ENot(p) | Expr::ENeg(p) => p.locally_free.0.clone(),
        Expr::EMult(p1, p2)
        | Expr::EDiv(p1, p2)
        | Expr::EMod(p1, p2)
        | Expr::EPlus(p1, p2)
        | Expr::EMinus(p1, p2)
        | Expr::ELt(p1, p2)
        | Expr::ELte(p1, p2)
        | Expr::EGt(p1, p2)
        | Expr::EGte(p1, p2)
        | Expr::EEq(p1, p2)
        | Expr::ENeq(p1, p2)
        | Expr::EAnd(p1, p2)
        | Expr::EOr(p1, p2)
        | Expr::EShortAnd(p1, p2)
        | Expr::EShortOr(p1, p2)
        | Expr::EPercentPercent(p1, p2)
        | Expr::EPlusPlus(p1, p2)
        | Expr::EMinusMinus(p1, p2) => union_free(&p1.locally_free.0, &p2.locally_free.0),
        Expr::EMethod(e) => e.locally_free.0.clone(),
        Expr::EMatches(target, pattern) => {
            union_free(&target.locally_free.0, &pattern.locally_free.0)
        }
    }
}

pub fn connective_used_of_expr(e: &Expr) -> bool {
    match e {
        Expr::GBool(_)
        | Expr::GInt(_)
        | Expr::GBigInt(_)
        | Expr::GString(_)
        | Expr::GUri(_)
        | Expr::GByteArray(_) => false,
        Expr::EList(e) => e.connective_used,
        Expr::ETuple(e) => e.connective_used,
        Expr::ESet(e) => e.connective_used,
        Expr::EMap(e) => e.connective_used,
        Expr::EVar(v) => connective_used_of_var(v),
        Expr::ENot(p) | Expr::ENeg(p) => p.connective_used,
        Expr::EMult(p1, p2)
        | Expr::EDiv(p1, p2)
        | Expr::EMod(p1, p2)
        | Expr::EPlus(p1, p2)
        | Expr::EMinus(p1, p2)
        | Expr::ELt(p1, p2)
        | Expr::ELte(p1, p2)
        | Expr::EGt(p1, p2)
        | Expr::EGte(p1, p2)
        | Expr::EEq(p1, p2)
        | Expr::ENeq(p1, p2)
        | Expr::EAnd(p1, p2)
        | Expr::EOr(p1, p2)
        | Expr::EShortAnd(p1, p2)
        | Expr::EShortOr(p1, p2)
        | Expr::EPercentPercent(p1, p2)
        | Expr::EPlusPlus(p1, p2)
        | Expr::EMinusMinus(p1, p2) => p1.connective_used || p2.connective_used,
        Expr::EMethod(e) => e.connective_used,
        Expr::EMatches(target, _pattern) => target.connective_used,
    }
}

// --- Connective --------------------------------------------------------

pub fn locally_free_of_connective(c: &Connective, depth: i32) -> BitSet {
    match c {
        Connective::VarRef(VarRef { index, depth: var_depth }) if depth == *var_depth => {
            vec![*index]
        }
        _ => Vec::new(),
    }
}

pub fn connective_used_of_connective(c: &Connective) -> bool {
    !matches!(c, Connective::VarRef(_) | Connective::Empty)
}

// --- New ---------------------------------------------------------------

pub fn connective_used_of_new(n: &New) -> bool {
    n.p.connective_used
}

// --- Par builders (mirrors `prepend` / `++` / `single*`) --------------

/// The Scala `p ++ that` (argument's fields first, then receiver's).
pub fn par_concat<A: Sort + SortJoin<B>, B: Sort>(p: &Par<A>, that: &Par<B>) -> Par<<A as SortJoin<B>>::Output> {
    let mut sends = that.sends.clone();
    sends.extend(p.sends.iter().cloned());
    let mut receives = that.receives.clone();
    receives.extend(p.receives.iter().cloned());
    let mut news = that.news.clone();
    news.extend(p.news.iter().cloned());
    let mut exprs = that.exprs.clone();
    exprs.extend(p.exprs.iter().cloned());
    let mut matches = that.matches.clone();
    matches.extend(p.matches.iter().cloned());
    let mut unforgeables = that.unforgeables.clone();
    unforgeables.extend(p.unforgeables.iter().cloned());
    let mut bundles = that.bundles.clone();
    bundles.extend(p.bundles.iter().cloned());
    let mut connectives = that.connectives.clone();
    connectives.extend(p.connectives.iter().cloned());
    Par {
        sends,
        receives,
        news,
        exprs,
        matches,
        unforgeables,
        bundles,
        connectives,
        locally_free: AlwaysEqual(union_free(&that.locally_free.0, &p.locally_free.0)),
        connective_used: that.connective_used || p.connective_used,
        ..Default::default()
    }
}

pub fn prepend_send(par: &Par, s: Send) -> Par {
    let mut sends = vec![s.clone()];
    sends.extend(par.sends.iter().cloned());
    Par {
        sends,
        locally_free: AlwaysEqual(union_free(&par.locally_free.0, &s.locally_free.0)),
        connective_used: par.connective_used || s.connective_used,
        ..par.clone()
    }
}

pub fn prepend_receive(par: &Par, r: Receive) -> Par {
    let mut receives = vec![r.clone()];
    receives.extend(par.receives.iter().cloned());
    Par {
        receives,
        locally_free: AlwaysEqual(union_free(&par.locally_free.0, &r.locally_free.0)),
        connective_used: par.connective_used || r.connective_used,
        ..par.clone()
    }
}

pub fn prepend_new(par: &Par, n: New) -> Par {
    let mut news = vec![n.clone()];
    news.extend(par.news.iter().cloned());
    Par {
        news,
        locally_free: AlwaysEqual(union_free(&par.locally_free.0, &n.locally_free.0)),
        connective_used: par.connective_used || n.p.connective_used,
        ..par.clone()
    }
}

pub fn prepend_expr<S: Sort>(par: &Par<S>, e: Expr, depth: i32) -> Par<S> {
    let mut exprs = vec![e.clone()];
    exprs.extend(par.exprs.iter().cloned());
    Par {
        exprs,
        locally_free: AlwaysEqual(union_free(
            &par.locally_free.0,
            &locally_free_of_expr(&e, depth),
        )),
        connective_used: par.connective_used || connective_used_of_expr(&e),
        ..par.clone()
    }
}

pub fn prepend_match<S: Sort>(par: &Par<S>, m: Match) -> Par<S> {
    let mut matches = vec![m.clone()];
    matches.extend(par.matches.iter().cloned());
    Par {
        matches,
        locally_free: AlwaysEqual(union_free(&par.locally_free.0, &m.locally_free.0)),
        connective_used: par.connective_used || m.connective_used,
        ..par.clone()
    }
}

pub fn prepend_bundle<S: Sort>(par: &Par<S>, b: Bundle) -> Par<S> {
    let mut bundles = vec![b.clone()];
    bundles.extend(par.bundles.iter().cloned());
    Par {
        bundles,
        locally_free: AlwaysEqual(union_free(&b.body.locally_free.0, &par.locally_free.0)),
        ..par.clone()
    }
}

pub fn prepend_connective<S: Sort>(par: &Par<S>, c: Connective, depth: i32) -> Par<S> {
    let mut connectives = vec![c.clone()];
    connectives.extend(par.connectives.iter().cloned());
    Par {
        connectives,
        connective_used: par.connective_used || connective_used_of_connective(&c),
        // Faithful to Scala: the connective prepend *replaces* (does not union) locallyFree.
        locally_free: AlwaysEqual(locally_free_of_connective(&c, depth)),
        ..par.clone()
    }
}

// --- `single*` accessors ----------------------------------------------

fn par_empty_except<S: Sort>(p: &Par<S>, which: usize) -> bool {
    let empties = [
        p.sends.is_empty(),
        p.receives.is_empty(),
        p.news.is_empty(),
        p.exprs.is_empty(),
        p.matches.is_empty(),
        p.unforgeables.is_empty(),
        p.bundles.is_empty(),
        p.connectives.is_empty(),
    ];
    empties.iter().enumerate().all(|(i, &e)| i == which || e)
}

pub fn single_expr<S: Sort>(p: &Par<S>) -> Option<&Expr> {
    if par_empty_except(p, 3) && p.exprs.len() == 1 {
        p.exprs.first()
    } else {
        None
    }
}

pub fn single_bundle<S: Sort>(p: &Par<S>) -> Option<&Bundle> {
    if par_empty_except(p, 6) && p.bundles.len() == 1 {
        p.bundles.first()
    } else {
        None
    }
}

pub fn single_unforgeable<S: Sort>(p: &Par<S>) -> Option<&GUnforgeable> {
    if par_empty_except(p, 5) && p.unforgeables.len() == 1 {
        p.unforgeables.first()
    } else {
        None
    }
}

pub fn single_connective<S: Sort>(p: &Par<S>) -> Option<&Connective> {
    if par_empty_except(p, 7) && p.connectives.len() == 1 {
        p.connectives.first()
    } else {
        None
    }
}

/// The Scala `isNil`.
pub fn is_nil<S: Sort>(p: &Par<S>) -> bool {
    p.sends.is_empty()
        && p.receives.is_empty()
        && p.news.is_empty()
        && p.exprs.is_empty()
        && p.matches.is_empty()
        && p.unforgeables.is_empty()
        && p.bundles.is_empty()
        && p.connectives.is_empty()
}

/// Wrap an expression in a single-`Expr` `Par` (port of `Par.apply(e: Expr)` / `fromExpr`).
pub fn from_expr(expr: Expr) -> Par {
    Par {
        exprs: vec![expr.clone()],
        locally_free: AlwaysEqual(locally_free_of_expr(&expr, 0)),
        connective_used: connective_used_of_expr(&expr),
        ..Default::default()
    }
}

/// The rholang type name of an expression (port of `RichExprInstance.typ`).
pub fn typ(expr: &Expr) -> &'static str {
    match expr {
        Expr::GBool(_) => "Bool",
        Expr::GInt(_) => "Int",
        Expr::GBigInt(_) => "BigInt",
        Expr::GString(_) => "String",
        Expr::GUri(_) => "Uri",
        Expr::GByteArray(_) => "ByteArray",
        Expr::EList(_) => "List",
        Expr::ETuple(_) => "Tuple",
        Expr::ESet(_) => "Set",
        Expr::EMap(_) => "Map",
        _ => "Unit",
    }
}
