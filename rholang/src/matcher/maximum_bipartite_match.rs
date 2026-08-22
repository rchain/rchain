//! Maximum bipartite matching (port of `matcher/MaximumBipartiteMatch.scala`).
//!
//! The Scala `StateT[F, S, A]` backtracking is ported to an explicit `&mut State` recursion. All
//! patterns must be assigned a match; otherwise `find_matches` returns `None`.

use std::collections::{HashMap, HashSet};

type Pattern<P> = (P, Vec<usize>);

struct State<P, R> {
    matches: HashMap<usize, (Pattern<P>, R)>,
    seen: HashSet<usize>,
}

fn find_match<P, T, R>(
    pattern: &Pattern<P>,
    state: &mut State<P, R>,
    targets: &[T],
    match_fn: &dyn Fn(&P, &T) -> Option<R>,
) -> bool
where
    P: Clone,
    R: Clone,
{
    let (p, candidates) = pattern;
    if candidates.is_empty() {
        return false;
    }
    let candidate = candidates[0];
    let rest = &candidates[1..];
    if state.seen.contains(&candidate) {
        return find_match(&(p.clone(), rest.to_vec()), state, targets, match_fn);
    }
    match match_fn(p, &targets[candidate]) {
        Some(result) => {
            state.seen.insert(candidate);
            try_claim_match(candidate, pattern, result, state, targets, match_fn)
        }
        None => find_match(&(p.clone(), rest.to_vec()), state, targets, match_fn),
    }
}

fn try_claim_match<P, T, R>(
    candidate: usize,
    pattern: &Pattern<P>,
    result: R,
    state: &mut State<P, R>,
    targets: &[T],
    match_fn: &dyn Fn(&P, &T) -> Option<R>,
) -> bool
where
    P: Clone,
    R: Clone,
{
    match state.matches.get(&candidate).cloned() {
        None => {
            state.matches.insert(candidate, (pattern.clone(), result));
            true
        }
        Some((previous_pattern, _)) => {
            if find_match(&previous_pattern, state, targets, match_fn) {
                state.matches.insert(candidate, (pattern.clone(), result));
                true
            } else {
                let rest = pattern.1[1..].to_vec();
                find_match(&(pattern.0.clone(), rest), state, targets, match_fn)
            }
        }
    }
}

/// Find a maximum matching where every pattern is matched (port of `findMatches`).
pub fn find_matches<P, T, R>(
    patterns: &[P],
    targets: &[T],
    match_fn: &dyn Fn(&P, &T) -> Option<R>,
) -> Option<Vec<(T, P, R)>>
where
    P: Clone,
    T: Clone,
    R: Clone,
{
    let candidates: Vec<usize> = (0..targets.len()).collect();
    let mut state = State {
        matches: HashMap::new(),
        seen: HashSet::new(),
    };
    for p in patterns {
        state.seen.clear();
        let pattern = (p.clone(), candidates.clone());
        if !find_match(&pattern, &mut state, targets, match_fn) {
            return None;
        }
    }
    let out: Vec<(T, P, R)> = state
        .matches
        .into_iter()
        .map(|(idx, (pat, res))| (targets[idx].clone(), pat.0, res))
        .collect();
    Some(out)
}
