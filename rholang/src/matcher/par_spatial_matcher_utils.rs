//! Sub-`Par` splitting and free-variable filtering (port of `matcher/ParSpatialMatcherUtils.scala`).

use rchain_models::ast::{Expr, Par, Sort, Var};

use crate::errors::RholangError;
use crate::matcher::par_count::ParCount;

/// Cap on the number of items in a single subset-enumeration dimension. A connective pattern
/// matched against a datum with `n` top-level processes enumerates up to 2ⁿ (subset, complement)
/// splits; beyond this bound the enumeration is a denial-of-service, so it is rejected rather than
/// materialized (C-2, defense-in-depth).
pub const MAX_SUBSET_ITEMS: usize = 20;

/// Cap on the total number of split combinations produced by `sub_pars` (the 7-way Cartesian
/// product of the per-dimension subsets).
pub const MAX_SPLIT_COMBINATIONS: u64 = 1_000_000;

/// Remove free-variable/wildcard exprs from a `Par` (port of `noFrees`).
pub fn no_frees<S: Sort>(par: &Par<S>) -> Par<S> {
    Par {
        exprs: no_frees_exprs(&par.exprs),
        ..par.clone()
    }
}

/// Remove free-variable/wildcard exprs from a list (port of `noFrees(exprs)`).
pub fn no_frees_exprs(exprs: &[Expr]) -> Vec<Expr> {
    exprs
        .iter()
        .filter(|expr| match expr {
            Expr::EVar(v) => matches!(**v, Var::BoundVar(_) | Var::Empty),
            _ => true,
        })
        .cloned()
        .collect()
}

/// Generate every (subset, complement) pair whose subset size is in `[minSize, maxSize]` (port of
/// `minMaxSubsets`).
///
/// Rejects (rather than materializes) enumerations whose input exceeds [`MAX_SUBSET_ITEMS`], so a
/// connective pattern cannot force exponential work.
pub fn min_max_subsets<A: Clone>(
    items: &[A],
    min_size: i32,
    max_size: i32,
) -> Result<Vec<(Vec<A>, Vec<A>)>, RholangError> {
    if items.len() > MAX_SUBSET_ITEMS {
        return Err(RholangError::ReduceError(format!(
            "spatial match subset enumeration too large: {} items exceeds limit {MAX_SUBSET_ITEMS}",
            items.len()
        )));
    }
    Ok(worker(items, min_size, max_size)
        .into_iter()
        .map(|(sub, comp, _)| (sub, comp))
        .collect())
}

fn counted_max_subsets<A: Clone>(items: &[A], max_size: i32) -> Vec<(Vec<A>, Vec<A>, i32)> {
    if items.is_empty() {
        return vec![(Vec::new(), Vec::new(), 0)];
    }
    let head = items[0].clone();
    let rem = &items[1..];
    let mut out = vec![(Vec::new(), items.to_vec(), 0)];
    for (tail, complement, count) in counted_max_subsets(rem, max_size) {
        if count == max_size {
            let mut comp = complement;
            comp.insert(0, head.clone());
            out.push((tail, comp, count));
        } else if tail.is_empty() {
            let mut sub = tail;
            sub.insert(0, head.clone());
            out.push((sub, complement, 1));
        } else {
            let mut comp = complement.clone();
            comp.insert(0, head.clone());
            out.push((tail.clone(), comp, count));
            let mut sub = tail;
            sub.insert(0, head.clone());
            out.push((sub, complement, count + 1));
        }
    }
    out
}

fn worker<A: Clone>(items: &[A], min_size: i32, max_size: i32) -> Vec<(Vec<A>, Vec<A>, i32)> {
    if max_size < 0 || min_size > max_size {
        return Vec::new();
    }
    if min_size <= 0 {
        if max_size == 0 {
            return vec![(Vec::new(), items.to_vec(), 0)];
        }
        return counted_max_subsets(items, max_size);
    }
    if items.is_empty() {
        return Vec::new();
    }
    let head = items[0].clone();
    let rem = &items[1..];
    let decr = min_size - 1;
    let mut out = Vec::new();
    for (tail, complement, count) in worker(rem, decr, max_size) {
        if count == max_size {
            let mut comp = complement;
            comp.insert(0, head.clone());
            out.push((tail, comp, count));
        } else if count == decr {
            let mut sub = tail;
            sub.insert(0, head.clone());
            out.push((sub, complement, min_size));
        } else {
            let mut comp = complement.clone();
            comp.insert(0, head.clone());
            out.push((tail.clone(), comp, count));
            let mut sub = tail;
            sub.insert(0, head.clone());
            out.push((sub, complement, count + 1));
        }
    }
    out
}

/// Split `par` into every (matched sub-`Par`, remainder) pair consistent with the min/max bounds
/// (port of `subPars`).
///
/// The 7-way Cartesian product of the per-dimension subset lists is the exponential blowup a
/// connective pattern can trigger; the total number of splits is capped at
/// [`MAX_SPLIT_COMBINATIONS`] (and each dimension at [`MAX_SUBSET_ITEMS`]).
pub fn sub_pars<S: Sort>(
    par: &Par<S>,
    min: &ParCount,
    max: &ParCount,
    min_prune: &ParCount,
    max_prune: &ParCount,
) -> Result<Vec<(Par<S>, Par<S>)>, RholangError> {
    let send_max = i32::min(max.sends, par.sends.len() as i32 - min_prune.sends);
    let receive_max = i32::min(max.receives, par.receives.len() as i32 - min_prune.receives);
    let news_max = i32::min(max.news, par.news.len() as i32 - min_prune.news);
    let expr_max = i32::min(max.exprs, par.exprs.len() as i32 - min_prune.exprs);
    let match_max = i32::min(max.matches, par.matches.len() as i32 - min_prune.matches);
    let unf_max = i32::min(
        max.unforgeables,
        par.unforgeables.len() as i32 - min_prune.unforgeables,
    );
    let bundle_max = i32::min(max.bundles, par.bundles.len() as i32 - min_prune.bundles);

    let send_min = i32::max(min.sends, par.sends.len() as i32 - max_prune.sends);
    let receive_min = i32::max(min.receives, par.receives.len() as i32 - max_prune.receives);
    let news_min = i32::max(min.news, par.news.len() as i32 - max_prune.news);
    let expr_min = i32::max(min.exprs, par.exprs.len() as i32 - max_prune.exprs);
    let match_min = i32::max(min.matches, par.matches.len() as i32 - max_prune.matches);
    let unf_min = i32::max(
        min.unforgeables,
        par.unforgeables.len() as i32 - max_prune.unforgeables,
    );
    let bundle_min = i32::max(min.bundles, par.bundles.len() as i32 - max_prune.bundles);

    let sub_sends = min_max_subsets(&par.sends, send_min, send_max)?;
    let sub_receives = min_max_subsets(&par.receives, receive_min, receive_max)?;
    let sub_news = min_max_subsets(&par.news, news_min, news_max)?;
    let sub_exprs = min_max_subsets(&par.exprs, expr_min, expr_max)?;
    let sub_matches = min_max_subsets(&par.matches, match_min, match_max)?;
    let sub_unfs = min_max_subsets(&par.unforgeables, unf_min, unf_max)?;
    let sub_bundles = min_max_subsets(&par.bundles, bundle_min, bundle_max)?;

    // Second layer of defense-in-depth: bound the Cartesian product itself, so a pattern with
    // many connective dimensions cannot multiply several large-but-under-limit subset lists into
    // an enormous split count.
    let total = (sub_sends.len() as u64)
        .saturating_mul(sub_receives.len() as u64)
        .saturating_mul(sub_news.len() as u64)
        .saturating_mul(sub_exprs.len() as u64)
        .saturating_mul(sub_matches.len() as u64)
        .saturating_mul(sub_unfs.len() as u64)
        .saturating_mul(sub_bundles.len() as u64);
    if total > MAX_SPLIT_COMBINATIONS {
        return Err(RholangError::ReduceError(format!(
            "spatial match split too large: {total} combinations exceeds limit {MAX_SPLIT_COMBINATIONS}"
        )));
    }

    let mut out = Vec::new();
    for ss in &sub_sends {
        for sr in &sub_receives {
            for sn in &sub_news {
                for se in &sub_exprs {
                    for sm in &sub_matches {
                        for su in &sub_unfs {
                            for sb in &sub_bundles {
                                let sub = Par {
                                    sends: ss.0.clone(),
                                    receives: sr.0.clone(),
                                    news: sn.0.clone(),
                                    exprs: se.0.clone(),
                                    matches: sm.0.clone(),
                                    unforgeables: su.0.clone(),
                                    bundles: sb.0.clone(),
                                    ..Default::default()
                                };
                                let comp = Par {
                                    sends: ss.1.clone(),
                                    receives: sr.1.clone(),
                                    news: sn.1.clone(),
                                    exprs: se.1.clone(),
                                    matches: sm.1.clone(),
                                    unforgeables: su.1.clone(),
                                    bundles: sb.1.clone(),
                                    ..Default::default()
                                };
                                out.push((sub, comp));
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn min_max_subsets_rejects_oversized_input() {
        let items: Vec<i32> = (0..(MAX_SUBSET_ITEMS as i32 + 1)).collect();
        assert!(min_max_subsets(&items, 0, items.len() as i32).is_err());
    }

    #[test]
    fn min_max_subsets_enumerates_small_input() {
        let items = vec![1, 2, 3];
        // min=0, max=3: every subset of the 3 items (2^3 = 8).
        let subs = min_max_subsets(&items, 0, 3).unwrap();
        assert_eq!(subs.len(), 8);
    }
}
