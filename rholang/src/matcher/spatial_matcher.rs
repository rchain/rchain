//! The spatial matcher (port of `matcher/SpatialMatcher.scala`).
//!
//! Concrete backtracking over `Vec<FreeMap>` replaces the Scala `StreamT`/`StateT` stack: each
//! matcher returns every possible resulting [`FreeMap`], in order, and `spatial_match_result`
//! takes the first.

use std::collections::BTreeSet;

use rchain_models::ast::{
    Bundle, Connective, ConnectiveBody, EList, ETuple, Expr, GUnforgeable, Match, MatchCase, New,
    Par, ParMap, ParSet, Receive, ReceiveBind, Send, Sort, Var,
};
use rchain_models::par_ops::{connective_used_of_expr, locally_free_of_expr, single_expr};
use rchain_models::sorter::{par_map, par_set};

use crate::errors::RholangError;
use crate::matcher::maximum_bipartite_match::find_matches;
use crate::matcher::par_count::ParCount;
use crate::matcher::par_spatial_matcher_utils::{no_frees, no_frees_exprs, sub_pars};
use crate::matcher::FreeMap;

type MResult = Result<Vec<FreeMap>, RholangError>;

/// Terminals whose matching is driven by `connective_used` / `locally_free`.
pub trait MatchableTerm: PartialEq + Clone {
    fn connective_used(&self) -> bool;
    fn locally_free_empty(&self) -> bool;
}

macro_rules! impl_matchable {
    ($ty:ty) => {
        impl MatchableTerm for $ty {
            fn connective_used(&self) -> bool {
                self.connective_used
            }
            fn locally_free_empty(&self) -> bool {
                self.locally_free.0.is_empty()
            }
        }
    };
}

impl<S: Sort> MatchableTerm for Par<S> {
    fn connective_used(&self) -> bool {
        self.connective_used
    }
    fn locally_free_empty(&self) -> bool {
        self.locally_free.0.is_empty()
    }
}
impl_matchable!(Send);
impl_matchable!(Receive);
impl_matchable!(Match);

impl MatchableTerm for New {
    fn connective_used(&self) -> bool {
        self.p.connective_used
    }
    fn locally_free_empty(&self) -> bool {
        self.locally_free.0.is_empty()
    }
}

impl MatchableTerm for Expr {
    fn connective_used(&self) -> bool {
        connective_used_of_expr(self)
    }
    fn locally_free_empty(&self) -> bool {
        locally_free_of_expr(self, 0).is_empty()
    }
}

impl MatchableTerm for Bundle {
    fn connective_used(&self) -> bool {
        false
    }
    fn locally_free_empty(&self) -> bool {
        self.body.locally_free.0.is_empty()
    }
}

impl MatchableTerm for GUnforgeable {
    fn connective_used(&self) -> bool {
        false
    }
    fn locally_free_empty(&self) -> bool {
        true
    }
}

impl MatchableTerm for (Par, Par) {
    fn connective_used(&self) -> bool {
        self.0.connective_used || self.1.connective_used
    }
    fn locally_free_empty(&self) -> bool {
        self.0.locally_free.0.is_empty() && self.1.locally_free.0.is_empty()
    }
}

impl MatchableTerm for ReceiveBind {
    fn connective_used(&self) -> bool {
        self.source.connective_used
    }
    fn locally_free_empty(&self) -> bool {
        false
    }
}

impl MatchableTerm for MatchCase {
    fn connective_used(&self) -> bool {
        self.source.connective_used
    }
    fn locally_free_empty(&self) -> bool {
        false
    }
}

fn guard(fm: &FreeMap, cond: bool) -> MResult {
    if cond {
        Ok(vec![fm.clone()])
    } else {
        Ok(Vec::new())
    }
}

fn is_free_var_expr(expr: &Expr) -> Option<i32> {
    match expr {
        Expr::EVar(v) => match **v {
            Var::FreeVar(level) => Some(level),
            _ => None,
        },
        _ => None,
    }
}

fn is_wildcard_expr(expr: &Expr) -> bool {
    matches!(expr, Expr::EVar(v) if matches!(**v, Var::Wildcard))
}

fn with_sends(p: &Par, s: &[Send]) -> Par {
    Par { sends: s.to_vec(), ..p.clone() }
}
fn with_receives(p: &Par, r: &[Receive]) -> Par {
    Par { receives: r.to_vec(), ..p.clone() }
}
fn with_news(p: &Par, n: &[New]) -> Par {
    Par { news: n.to_vec(), ..p.clone() }
}
fn with_exprs(p: &Par, e: &[Expr]) -> Par {
    Par { exprs: e.to_vec(), ..p.clone() }
}
fn with_matches(p: &Par, m: &[Match]) -> Par {
    Par { matches: m.to_vec(), ..p.clone() }
}
fn with_bundles(p: &Par, b: &[Bundle]) -> Par {
    Par { bundles: b.to_vec(), ..p.clone() }
}
fn with_unforgeables(p: &Par, u: &[GUnforgeable]) -> Par {
    Par { unforgeables: u.to_vec(), ..p.clone() }
}

/// The public entry point (port of `SpatialMatcher.spatialMatchResult`).
pub fn spatial_match_result<S: Sort>(target: &Par<S>, pattern: &Par<S>) -> Result<Option<FreeMap>, RholangError> {
    Ok(spatial_match(target, pattern, &FreeMap::new())?.into_iter().next())
}

pub fn spatial_match<S: Sort>(target: &Par<S>, pattern: &Par<S>, fm: &FreeMap) -> MResult {
    if !pattern.connective_used {
        return guard(fm, pattern == target);
    }

    let var_level: Option<i32> = pattern.exprs.iter().find_map(is_free_var_expr);
    let wildcard: bool = pattern.exprs.iter().any(is_wildcard_expr);

    let filtered_pattern = no_frees(pattern);
    let pc = ParCount::from_par(&filtered_pattern);
    let min_rem = pc.clone();
    let max_rem = if wildcard || var_level.is_some() {
        ParCount::max_count()
    } else {
        pc
    };

    let individual_bounds: Vec<(ParCount, ParCount)> = filtered_pattern
        .connectives
        .iter()
        .map(ParCount::min_max_connective)
        .collect();

    // scanRight((minRem, maxRem))((bounds, acc) => (bounds._1 + acc._1, bounds._2 + acc._2)).tail
    let mut remainder_bounds: Vec<(ParCount, ParCount)> = vec![(min_rem.clone(), max_rem.clone())];
    let mut acc = (min_rem, max_rem);
    for bounds in individual_bounds.iter().rev() {
        acc = (bounds.0.add(&acc.0), bounds.1.add(&acc.1));
        remainder_bounds.push(acc.clone());
    }
    remainder_bounds.reverse();
    remainder_bounds.remove(0);

    // Fold over connectives: match each against a sub-`Par` of the running remainder.
    let mut states: Vec<(Par<S>, FreeMap)> = vec![(target.clone(), fm.clone())];
    for (i, con) in filtered_pattern.connectives.iter().enumerate() {
        let (min_b, max_b) = &individual_bounds[i];
        let (min_prune, max_prune) = &remainder_bounds[i];
        let mut next: Vec<(Par<S>, FreeMap)> = Vec::new();
        for (rem, fm_state) in states {
            for (sub, comp) in sub_pars(&rem, min_b, max_b, min_prune, max_prune)? {
                for fm_match in spatial_match_connective(&sub, con, &fm_state)? {
                    next.push((comp.clone(), fm_match));
                }
            }
        }
        states = next;
    }

    let mut out: Vec<FreeMap> = Vec::new();
    for (rem, fm_state) in states {
        for fm1 in list_match_single(
            &rem.sends,
            &pattern.sends,
            &with_sends,
            var_level,
            wildcard,
            &fm_state,
            &spatial_match_send,
        )? {
            for fm2 in list_match_single(
                &rem.receives,
                &pattern.receives,
                &with_receives,
                var_level,
                wildcard,
                &fm1,
                &spatial_match_receive,
            )? {
                for fm3 in list_match_single(
                    &rem.news,
                    &pattern.news,
                    &with_news,
                    var_level,
                    wildcard,
                    &fm2,
                    &spatial_match_new,
                )? {
                    for fm4 in list_match_single(
                        &rem.exprs,
                        &no_frees_exprs(&pattern.exprs),
                        &with_exprs,
                        var_level,
                        wildcard,
                        &fm3,
                        &spatial_match_expr,
                    )? {
                        for fm5 in list_match_single(
                            &rem.matches,
                            &pattern.matches,
                            &with_matches,
                            var_level,
                            wildcard,
                            &fm4,
                            &spatial_match_match,
                        )? {
                            for fm6 in list_match_single(
                                &rem.bundles,
                                &pattern.bundles,
                                &with_bundles,
                                var_level,
                                wildcard,
                                &fm5,
                                &spatial_match_bundle,
                            )? {
                                for fm7 in list_match_single(
                                    &rem.unforgeables,
                                    &pattern.unforgeables,
                                    &with_unforgeables,
                                    var_level,
                                    wildcard,
                                    &fm6,
                                    &spatial_match_unforgeable,
                                )? {
                                    out.push(fm7);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(out)
}

pub fn spatial_match_connective<S: Sort>(target: &Par<S>, con: &Connective, fm: &FreeMap) -> MResult {
    match con {
        Connective::ConnAnd(ConnectiveBody { ps }) => {
            let mut states = vec![fm.clone()];
            for p in ps {
                let mut next = Vec::new();
                for fm_state in states {
                    next.extend(spatial_match(target, &p.clone().re_sort(), &fm_state)?);
                }
                states = next;
            }
            Ok(states)
        }
        Connective::ConnOr(ConnectiveBody { ps }) => {
            let mut all = Vec::new();
            for p in ps {
                all.extend(spatial_match(target, &p.clone().re_sort(), fm)?);
            }
            Ok(all.into_iter().take(1).collect())
        }
        Connective::ConnNot(p) => {
            let matches = spatial_match(target, &p.clone().re_sort(), fm)?;
            guard(fm, matches.is_empty())
        }
        Connective::Empty | Connective::VarRef(_) => Ok(Vec::new()),
        Connective::ConnBool(_) => match single_expr(target) {
            Some(Expr::GBool(_)) => guard(fm, true),
            _ => Ok(Vec::new()),
        },
        Connective::ConnInt(_) => match single_expr(target) {
            Some(Expr::GInt(_)) => guard(fm, true),
            _ => Ok(Vec::new()),
        },
        Connective::ConnBigInt(_) => match single_expr(target) {
            Some(Expr::GBigInt(_)) => guard(fm, true),
            _ => Ok(Vec::new()),
        },
        Connective::ConnString(_) => match single_expr(target) {
            Some(Expr::GString(_)) => guard(fm, true),
            _ => Ok(Vec::new()),
        },
        Connective::ConnUri(_) => match single_expr(target) {
            Some(Expr::GUri(_)) => guard(fm, true),
            _ => Ok(Vec::new()),
        },
        Connective::ConnByteArray(_) => match single_expr(target) {
            Some(Expr::GByteArray(_)) => guard(fm, true),
            _ => Ok(Vec::new()),
        },
    }
}

pub fn spatial_match_send(target: &Send, pattern: &Send, fm: &FreeMap) -> MResult {
    if target.persistent != pattern.persistent {
        return Ok(Vec::new());
    }
    let mut out = Vec::new();
    for fm1 in spatial_match(target.chan.as_ref(), pattern.chan.as_ref(), fm)? {
        for (_, fm2) in fold_match(&target.data, &pattern.data, None, &fm1, &spatial_match)? {
            out.push(fm2);
        }
    }
    Ok(out)
}

pub fn spatial_match_receive(target: &Receive, pattern: &Receive, fm: &FreeMap) -> MResult {
    if target.persistent != pattern.persistent {
        return Ok(Vec::new());
    }
    let mut out = Vec::new();
    let identity: &dyn Fn(&Par, &[ReceiveBind]) -> Par = &|p, _| p.clone();
    for fm1 in list_match_single(
        &target.binds,
        &pattern.binds,
        identity,
        None,
        false,
        fm,
        &spatial_match_receive_bind,
    )? {
        for fm2 in spatial_match(&target.body, &pattern.body, &fm1)? {
            out.push(fm2);
        }
    }
    Ok(out)
}

pub fn spatial_match_new(target: &New, pattern: &New, fm: &FreeMap) -> MResult {
    if target.bind_count != pattern.bind_count {
        return Ok(Vec::new());
    }
    spatial_match(&target.p, &pattern.p, fm)
}

pub fn spatial_match_match(target: &Match, pattern: &Match, fm: &FreeMap) -> MResult {
    let mut out = Vec::new();
    for fm1 in spatial_match(target.target.as_ref(), pattern.target.as_ref(), fm)? {
        for (_, fm2) in fold_match(
            &target.cases,
            &pattern.cases,
            None,
            &fm1,
            &spatial_match_match_case,
        )? {
            out.push(fm2);
        }
    }
    Ok(out)
}

pub fn spatial_match_bundle(target: &Bundle, pattern: &Bundle, fm: &FreeMap) -> MResult {
    guard(fm, pattern == target)
}

pub fn spatial_match_unforgeable(
    target: &GUnforgeable,
    pattern: &GUnforgeable,
    fm: &FreeMap,
) -> MResult {
    guard(fm, target == pattern)
}

pub fn spatial_match_receive_bind(
    target: &ReceiveBind,
    pattern: &ReceiveBind,
    fm: &FreeMap,
) -> MResult {
    if target.patterns != pattern.patterns {
        return Ok(Vec::new());
    }
    spatial_match(&target.source, &pattern.source, fm)
}

pub fn spatial_match_match_case(
    target: &MatchCase,
    pattern: &MatchCase,
    fm: &FreeMap,
) -> MResult {
    if target.pattern != pattern.pattern {
        return Ok(Vec::new());
    }
    spatial_match(&target.source, &pattern.source, fm)
}

pub fn spatial_match_expr(target: &Expr, pattern: &Expr, fm: &FreeMap) -> MResult {
    match (target, pattern) {
        (Expr::EList(EList { ps: tlist, .. }), Expr::EList(EList { ps: plist, remainder: rem, .. })) => {
            let mut out = Vec::new();
            for (matched_rem, fm1) in fold_match(tlist, plist, rem.as_deref(), fm, &spatial_match)? {
                match rem.as_deref() {
                    Some(Var::FreeVar(level)) => {
                        let mut fm2 = fm1;
                        fm2.insert(
                            *level,
                            Par {
                                exprs: vec![Expr::EList(EList {
                                    ps: matched_rem,
                                    ..Default::default()
                                })],
                                ..Default::default()
                            },
                        );
                        out.push(fm2);
                    }
                    _ => out.push(fm1),
                }
            }
            Ok(out)
        }
        (Expr::ETuple(ETuple { ps: tlist, .. }), Expr::ETuple(ETuple { ps: plist, .. })) => {
            let mut out = Vec::new();
            for (_, fm1) in fold_match(tlist, plist, None, fm, &spatial_match)? {
                out.push(fm1);
            }
            Ok(out)
        }
        (
            Expr::ESet(ParSet { ps: tlist, remainder: rem, .. }),
            Expr::ESet(ParSet { ps: plist, .. }),
        ) => {
            let is_wildcard = matches!(rem.as_deref(), Some(Var::Wildcard));
            let remainder_var = match rem.as_deref() {
                Some(Var::FreeVar(level)) => Some(*level),
                _ => None,
            };
            let merger: &dyn Fn(&Par, &[Par]) -> Par = &|p, r| {
                Par {
                    exprs: vec![Expr::ESet(par_set(r.to_vec()))],
                    ..p.clone()
                }
            };
            list_match_single(tlist, plist, merger, remainder_var, is_wildcard, fm, &spatial_match)
        }
        (
            Expr::EMap(ParMap { kvs: tlist, remainder: rem, .. }),
            Expr::EMap(ParMap { kvs: plist, .. }),
        ) => {
            let is_wildcard = matches!(rem.as_deref(), Some(Var::Wildcard));
            let remainder_var = match rem.as_deref() {
                Some(Var::FreeVar(level)) => Some(*level),
                _ => None,
            };
            let merger: &dyn Fn(&Par, &[(Par, Par)]) -> Par = &|p, r| {
                Par {
                    exprs: vec![Expr::EMap(par_map(r.to_vec()))],
                    ..p.clone()
                }
            };
            list_match_single(
                tlist,
                plist,
                merger,
                remainder_var,
                is_wildcard,
                fm,
                &spatial_match_pair,
            )
        }
        (Expr::EVar(vp), Expr::EVar(vt)) => guard(fm, vp == vt),
        (Expr::ENot(t), Expr::ENot(p)) => spatial_match(t, p, fm),
        (Expr::ENeg(t), Expr::ENeg(p)) => spatial_match(t, p, fm),
        (Expr::EMult(t1, t2), Expr::EMult(p1, p2)) => binary(t1, t2, p1, p2, fm),
        (Expr::EDiv(t1, t2), Expr::EDiv(p1, p2)) => binary(t1, t2, p1, p2, fm),
        (Expr::EMod(t1, t2), Expr::EMod(p1, p2)) => binary(t1, t2, p1, p2, fm),
        (Expr::EPercentPercent(t1, t2), Expr::EPercentPercent(p1, p2)) => {
            binary(t1, t2, p1, p2, fm)
        }
        (Expr::EPlus(t1, t2), Expr::EPlus(p1, p2)) => binary(t1, t2, p1, p2, fm),
        (Expr::EPlusPlus(t1, t2), Expr::EPlusPlus(p1, p2)) => binary(t1, t2, p1, p2, fm),
        (Expr::EMinusMinus(t1, t2), Expr::EMinusMinus(p1, p2)) => binary(t1, t2, p1, p2, fm),
        _ => Ok(Vec::new()),
    }
}

fn binary(t1: &Par, t2: &Par, p1: &Par, p2: &Par, fm: &FreeMap) -> MResult {
    let mut out = Vec::new();
    for fm1 in spatial_match(t1, p1, fm)? {
        out.extend(spatial_match(t2, p2, &fm1)?);
    }
    Ok(out)
}

fn spatial_match_pair(pair1: &(Par, Par), pair2: &(Par, Par), fm: &FreeMap) -> MResult {
    let mut out = Vec::new();
    for fm1 in spatial_match(&pair1.0, &pair2.0, fm)? {
        out.extend(spatial_match(&pair1.1, &pair2.1, &fm1)?);
    }
    Ok(out)
}

/// Match a list of targets against a list of patterns, capturing remainders (port of `foldMatch`).
pub(crate) fn fold_match<T: MatchableTerm>(
    tlist: &[T],
    plist: &[T],
    remainder: Option<&Var>,
    fm: &FreeMap,
    spatial_match_fn: &dyn Fn(&T, &T, &FreeMap) -> MResult,
) -> Result<Vec<(Vec<T>, FreeMap)>, RholangError> {
    match (tlist.is_empty(), plist.is_empty()) {
        (true, true) => Ok(vec![(Vec::new(), fm.clone())]),
        (true, false) => Ok(Vec::new()),
        (false, true) => match remainder {
            None => Ok(Vec::new()),
            Some(Var::FreeVar(_)) => {
                if tlist.iter().all(|t| t.locally_free_empty()) {
                    Ok(vec![(tlist.to_vec(), fm.clone())])
                } else {
                    Ok(Vec::new())
                }
            }
            Some(Var::Wildcard) => Ok(vec![(Vec::new(), fm.clone())]),
            Some(_) => Ok(Vec::new()),
        },
        (false, false) => {
            let mut out = Vec::new();
            for fm1 in spatial_match_fn(&tlist[0], &plist[0], fm)? {
                for (matched, fm2) in
                    fold_match(&tlist[1..], &plist[1..], remainder, &fm1, spatial_match_fn)?
                {
                    out.push((matched, fm2));
                }
            }
            Ok(out)
        }
    }
}

fn handle_remainder<T: Clone>(
    fm: &FreeMap,
    targets: &[T],
    level: i32,
    merger: &dyn Fn(&Par, &[T]) -> Par,
) -> FreeMap {
    let mut out = fm.clone();
    let remainder_par = out.get(&level).cloned().unwrap_or_default();
    let updated = merger(&remainder_par, targets);
    out.insert(level, updated);
    out
}

fn aggregate_updates(fm: &FreeMap, free_maps: &[FreeMap]) -> Result<FreeMap, RholangError> {
    let current_vars: BTreeSet<i32> = fm.keys().copied().collect();
    let mut added_vars: Vec<i32> = Vec::new();
    for f in free_maps {
        for k in f.keys() {
            if !current_vars.contains(k) {
                added_vars.push(*k);
            }
        }
    }
    let unique: BTreeSet<i32> = added_vars.iter().copied().collect();
    if unique.len() != added_vars.len() {
        return Err(RholangError::BugFoundError(format!(
            "Aggregated updates conflicted with each other: {free_maps:?}"
        )));
    }
    let mut out = fm.clone();
    for f in free_maps {
        out.extend(f.clone());
    }
    Ok(out)
}

#[derive(Clone)]
enum MbmPattern<T> {
    Term(T),
    Remainder,
}

fn list_match_single<T: MatchableTerm>(
    targets: &[T],
    patterns: &[T],
    merger: &dyn Fn(&Par, &[T]) -> Par,
    remainder: Option<i32>,
    wildcard: bool,
    fm: &FreeMap,
    spatial_match_fn: &dyn Fn(&T, &T, &FreeMap) -> MResult,
) -> MResult {
    let exact_match = !wildcard && remainder.is_none();
    let plen = patterns.len();
    let tlen = targets.len();

    if exact_match && plen != tlen {
        return Ok(Vec::new());
    }
    if plen > tlen {
        return Ok(Vec::new());
    }
    if plen == 0 && tlen == 0 && remainder.is_none() {
        return Ok(vec![fm.clone()]);
    }
    if plen == 0 {
        return match remainder {
            Some(level) => {
                if targets.iter().all(|t| t.locally_free_empty()) {
                    Ok(vec![handle_remainder(fm, targets, level, merger)])
                } else {
                    Ok(Vec::new())
                }
            }
            None => Ok(Vec::new()),
        };
    }
    list_match(targets, patterns, merger, remainder, wildcard, fm, spatial_match_fn)
}

fn list_match<T: MatchableTerm>(
    targets: &[T],
    patterns: &[T],
    merger: &dyn Fn(&Par, &[T]) -> Par,
    remainder: Option<i32>,
    wildcard: bool,
    fm: &FreeMap,
    spatial_match_fn: &dyn Fn(&T, &T, &FreeMap) -> MResult,
) -> MResult {
    let mut all_patterns: Vec<MbmPattern<T>> = Vec::new();
    if remainder.is_some() {
        for _ in 0..(targets.len() - patterns.len()) {
            all_patterns.push(MbmPattern::Remainder);
        }
    }
    for p in patterns {
        all_patterns.push(MbmPattern::Term(p.clone()));
    }

    // An internal `RholangError` (e.g. a Law-5 `BugFoundError`) must not be silently swallowed as
    // "no match": record it and propagate it when the bipartite search finds no matching.
    let match_error: std::cell::RefCell<Option<RholangError>> = std::cell::RefCell::new(None);
    let match_fn = |pattern: &MbmPattern<T>, t: &T| -> Option<FreeMap> {
        match pattern {
            MbmPattern::Term(p) => {
                if !p.connective_used() {
                    if t == p {
                        Some(FreeMap::new())
                    } else {
                        None
                    }
                } else {
                    match spatial_match_fn(t, p, &FreeMap::new()) {
                        Ok(results) => results.into_iter().next(),
                        Err(e) => {
                            *match_error.borrow_mut() = Some(e);
                            None
                        }
                    }
                }
            }
            MbmPattern::Remainder => {
                if t.locally_free_empty() {
                    Some(FreeMap::new())
                } else {
                    None
                }
            }
        }
    };

    let matches = match find_matches(&all_patterns, targets, &match_fn) {
        Some(m) => m,
        None => {
            if let Some(e) = match_error.borrow().clone() {
                return Err(e);
            }
            return Ok(Vec::new());
        }
    };

    let free_maps: Vec<FreeMap> = matches.iter().map(|(_, _, r)| r.clone()).collect();
    let updated_fm = aggregate_updates(fm, &free_maps)?;

    let remainder_targets: Vec<T> = matches
        .iter()
        .filter(|(_, p, _)| matches!(p, MbmPattern::Remainder))
        .map(|(t, _, _)| t.clone())
        .collect();
    let remainder_targets_sorted: Vec<T> = targets
        .iter()
        .filter(|t| remainder_targets.contains(t))
        .cloned()
        .collect();

    match remainder {
        None => {
            if wildcard || remainder_targets_sorted.is_empty() {
                Ok(vec![updated_fm])
            } else {
                Ok(Vec::new())
            }
        }
        Some(level) => Ok(vec![handle_remainder(
            &updated_fm,
            &remainder_targets_sorted,
            level,
            merger,
        )]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn par(exprs: Vec<Expr>) -> Par {
        Par {
            exprs,
            ..Default::default()
        }
    }

    #[test]
    fn concrete_equality() {
        let t = par(vec![Expr::GInt(42)]);
        assert_eq!(spatial_match_result(&t, &t).unwrap(), Some(FreeMap::new()));
        let p = par(vec![Expr::GInt(43)]);
        assert_eq!(spatial_match_result(&t, &p).unwrap(), None);
    }

    #[test]
    fn binds_free_var() {
        let target = par(vec![Expr::GInt(42)]);
        let pattern = Par {
            exprs: vec![Expr::EVar(Box::new(Var::FreeVar(0)))],
            connective_used: true,
            ..Default::default()
        };
        let expected = FreeMap::from([(0, par(vec![Expr::GInt(42)]))]);
        assert_eq!(spatial_match_result(&target, &pattern).unwrap(), Some(expected));
    }
}
