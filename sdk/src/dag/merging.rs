//! DAG merge conflict-resolution logic.
//!
//! Faithful port of
//! `sdk/src/main/scala/coop/rchain/sdk/dag/merging/ConflictResolutionLogic.scala`.
//!
//! Law 17: the merge outcome is deterministic — `resolve_conflict_set` chooses the unique
//! optimal rejection (min total cost, then min size, then lexicographically smallest set),
//! independent of arrival order; mergeable (numeric) channels must stay non-negative and must
//! not overflow.
//!
//! Note on determinism: the Scala source uses `Set`/`Map` (unordered) and relies on explicit
//! `.sorted` in the hot paths. This port uses `BTreeSet`/`BTreeMap` throughout, which makes
//! iteration deterministic by construction — matching the *intent* of Law 17 and the callers
//! that already sort (`compute_greedy_non_intersecting_branches`, `add_mergeable_overflow_rejections`).

use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// `cats` `|+|` for `Map[D, Set[D]]`: union the value sets under shared keys.
fn union_maps<D: Ord + Clone>(
    mut a: BTreeMap<D, BTreeSet<D>>,
    b: &BTreeMap<D, BTreeSet<D>>,
) -> BTreeMap<D, BTreeSet<D>> {
    for (k, v) in b {
        a.entry(k.clone()).or_default().extend(v.iter().cloned());
    }
    a
}

/// All items in dependency chains reachable from `of` (including `of` itself).
pub fn with_dependencies<D: Ord + Clone>(
    of: &BTreeSet<D>,
    dependency_map: &BTreeMap<D, BTreeSet<D>>,
) -> BTreeSet<D> {
    let mut result = of.clone();
    let mut frontier: BTreeSet<D> = of.clone();
    loop {
        let next: BTreeSet<D> = frontier
            .iter()
            .flat_map(|d| dependency_map.get(d).cloned().unwrap_or_default())
            .collect();
        let new: BTreeSet<D> = next.difference(&result).cloned().collect();
        if new.is_empty() {
            break;
        }
        result.extend(new.iter().cloned());
        frontier = new;
    }
    result
}

/// Items incompatible with the finalized body: conflicts with finally-accepted, or
/// dependents of finally-rejected.
pub fn incompatible_with_final<D: Ord + Clone>(
    accepted_finally: &BTreeSet<D>,
    rejected_finally: &BTreeSet<D>,
    conflicts_map: &BTreeMap<D, BTreeSet<D>>,
    dependency_map: &BTreeMap<D, BTreeSet<D>>,
) -> BTreeSet<D> {
    let mut out = BTreeSet::new();
    for a in accepted_finally {
        if let Some(v) = conflicts_map.get(a) {
            out.extend(v.iter().cloned());
        }
    }
    for r in rejected_finally {
        if let Some(v) = dependency_map.get(r) {
            out.extend(v.iter().cloned());
        }
    }
    out
}

/// Split the scope into non-overlapping partitions, greedily allocating intersecting chunks to
/// the bigger (earlier) view.
pub fn partition_scope<D: Ord + Clone>(views: &[BTreeSet<D>]) -> Vec<BTreeSet<D>> {
    let mut result = Vec::new();
    let mut remaining: Vec<BTreeSet<D>> = views.to_vec();
    while let Some(head) = remaining.first().cloned() {
        result.push(head.clone());
        let tail: Vec<BTreeSet<D>> = remaining[1..]
            .iter()
            .map(|v| v.difference(&head).cloned().collect())
            .collect();
        remaining = tail;
    }
    result
}

/// Build a relation map over `target_set × source_set`. Keys are `source`; values are the
/// related `target`s (plus, when `directed` is false, the symmetric edge).
pub fn compute_relation_map<D, F>(
    directed: bool,
    target_set: &BTreeSet<D>,
    source_set: &BTreeSet<D>,
    relation: F,
) -> BTreeMap<D, BTreeSet<D>>
where
    D: Ord + Clone,
    F: Fn(&D, &D) -> bool,
{
    let mut acc: BTreeMap<D, BTreeSet<D>> = BTreeMap::new();
    for target in target_set {
        for source in source_set {
            if relation(target, source) && target != source {
                acc.entry(source.clone()).or_default().insert(target.clone());
                if !directed {
                    acc.entry(target.clone()).or_default().insert(source.clone());
                }
            }
        }
    }
    acc
}

/// Build the (undirected) conflicts map.
pub fn compute_conflicts_map<D, F>(
    target_set: &BTreeSet<D>,
    source_set: &BTreeSet<D>,
    conflicts: F,
) -> BTreeMap<D, BTreeSet<D>>
where
    D: Ord + Clone,
    F: Fn(&D, &D) -> bool,
{
    compute_relation_map(false, target_set, source_set, conflicts)
}

/// Build the (directed) dependency map.
pub fn compute_dependency_map<D, F>(
    target_set: &BTreeSet<D>,
    source_set: &BTreeSet<D>,
    depends: F,
) -> BTreeMap<D, BTreeSet<D>>
where
    D: Ord + Clone,
    F: Fn(&D, &D) -> bool,
{
    compute_relation_map(true, target_set, source_set, depends)
}

/// Compute branches of depending items: each root's dependents are folded into their dependers,
/// so every tip/root becomes concurrent; target items outside any dependency become empty branches.
pub fn compute_branches<D: Ord + Clone>(
    target: &BTreeSet<D>,
    dependency_map: &BTreeMap<D, BTreeSet<D>>,
) -> BTreeMap<D, BTreeSet<D>> {
    let mut acc = dependency_map.clone();
    let entries: Vec<(D, BTreeSet<D>)> =
        dependency_map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
    for (root, depending) in &entries {
        let root_dependencies: Vec<D> = acc
            .iter()
            .filter(|(_, v)| v.contains(root))
            .map(|(k, _)| k.clone())
            .collect();
        if !root_dependencies.is_empty() {
            acc.remove(root);
            let merged: BTreeSet<D> = depending
                .iter()
                .cloned()
                .chain(std::iter::once(root.clone()))
                .collect();
            for k in &root_dependencies {
                acc.entry(k.clone()).or_default().extend(merged.iter().cloned());
            }
        }
    }
    // Target items that appear neither as a key nor a value get an empty branch.
    let mut all: BTreeSet<D> = BTreeSet::new();
    for (k, v) in dependency_map {
        all.insert(k.clone());
        all.extend(v.iter().cloned());
    }
    for t in target.difference(&all) {
        acc.entry(t.clone()).or_insert_with(BTreeSet::new);
    }
    acc
}

/// Compute branches of depending items that do not intersect, partitioning greedily.
pub fn compute_greedy_non_intersecting_branches<D: Ord + Clone>(
    target: &BTreeSet<D>,
    dependency_map: &BTreeMap<D, BTreeSet<D>>,
) -> Vec<BTreeSet<D>> {
    let concurrent_roots = compute_branches(target, dependency_map);
    let mut sorted: Vec<(usize, D, BTreeSet<D>)> = concurrent_roots
        .iter()
        .map(|(k, v)| (v.len(), k.clone(), v.clone()))
        .collect();
    // sort by (descending size, ascending key)
    sorted.sort_by(|a, b| b.0.cmp(&a.0).then_with(|| a.1.cmp(&b.1)));
    let views: Vec<BTreeSet<D>> = sorted
        .into_iter()
        .map(|(_, k, mut v)| {
            v.insert(k);
            v
        })
        .collect();
    partition_scope(&views)
}

/// Relation map sufficient for a merge set: conflicts/dependencies inside the conflict set and
/// between the conflict set and the final set.
pub fn compute_relation_map_for_merge_set<D, F1, F2>(
    conflict_set: &BTreeSet<D>,
    final_set: &BTreeSet<D>,
    conflicts: F1,
    depends: F2,
) -> (BTreeMap<D, BTreeSet<D>>, BTreeMap<D, BTreeSet<D>>)
where
    D: Ord + Clone,
    F1: Fn(&D, &D) -> bool,
    F2: Fn(&D, &D) -> bool,
{
    let conflicts_map = union_maps(
        compute_conflicts_map(conflict_set, final_set, &conflicts),
        &compute_conflicts_map(conflict_set, conflict_set, &conflicts),
    );
    let dependency_map = union_maps(
        compute_dependency_map(conflict_set, final_set, &depends),
        &compute_dependency_map(conflict_set, conflict_set, &depends),
    );
    (conflicts_map, dependency_map)
}

/// All rejection combinations (sets of rejected items) that resolve the conflict map.
///
/// This is the Scala `computeRejectionOptions` `O(2^n)` search, ported as a breadth-first
/// enumeration over acceptance states.
pub fn compute_rejection_options<D: Ord + Clone>(
    conflicts_map: &BTreeMap<D, BTreeSet<D>>,
) -> BTreeSet<BTreeSet<D>> {
    let all_keys: Vec<D> = conflicts_map.keys().cloned().collect();
    let mut queue: VecDeque<(D, BTreeSet<D>, BTreeSet<D>)> = all_keys
        .iter()
        .map(|k| (k.clone(), BTreeSet::new(), std::iter::once(k.clone()).collect()))
        .collect();
    let mut result: BTreeSet<BTreeSet<D>> = BTreeSet::new();

    while let Some((a, rj_acc, ac_acc)) = queue.pop_front() {
        let mut new_rj = rj_acc.clone();
        if let Some(c) = conflicts_map.get(&a) {
            new_rj.extend(c.iter().cloned());
        }
        let mut new_ac = ac_acc;
        new_ac.insert(a.clone());

        let next: Vec<D> = all_keys
            .iter()
            .filter(|k| !new_rj.contains(k) && !new_ac.contains(k))
            .cloned()
            .collect();
        if next.is_empty() {
            result.insert(new_rj);
        } else {
            for n in next {
                queue.push_back((n, new_rj.clone(), new_ac.clone()));
            }
        }
    }
    result
}

/// Pick the rejection option minimizing (total cost, size, sorted set) lexicographically.
pub fn compute_optimal_rejection<D, F>(
    options: &BTreeSet<BTreeSet<D>>,
    target_f: F,
) -> BTreeSet<D>
where
    D: Ord + Clone,
    F: Fn(&D) -> i64,
{
    options
        .iter()
        .min_by(|a, b| {
            let ca: i64 = a.iter().map(&target_f).sum();
            let cb: i64 = b.iter().map(&target_f).sum();
            let sa: Vec<&D> = a.iter().collect();
            let sb: Vec<&D> = b.iter().collect();
            (ca, a.len(), sa).cmp(&(cb, b.len(), sb))
        })
        .cloned()
        .unwrap_or_default()
}

fn calc_merged_result<D: Ord + Clone, CH: Ord + Clone>(
    deploy: &D,
    balances: &BTreeMap<CH, i64>,
    mergeable_diffs: &BTreeMap<D, BTreeMap<CH, i64>>,
) -> Option<BTreeMap<CH, i64>> {
    let diff = mergeable_diffs.get(deploy).cloned().unwrap_or_default();
    let mut acc = balances.clone();
    for (channel, change) in &diff {
        let current = acc.get(channel).copied().unwrap_or(0);
        let result = current.checked_add(*change)?; // None on overflow
        if result < 0 {
            return None;
        }
        acc.insert(channel.clone(), result);
    }
    Some(acc)
}

fn traverse_tree<D: Ord + Clone, F: Fn(&D) -> BTreeSet<D>>(root: &D, next: &F) -> Vec<D> {
    let mut result = Vec::new();
    let mut frontier: Vec<D> = vec![root.clone()];
    while !frontier.is_empty() {
        frontier.sort();
        result.extend(frontier.iter().cloned());
        let next_frontier: BTreeSet<D> = frontier.iter().flat_map(|d| next(d)).collect();
        frontier = next_frontier.into_iter().collect();
    }
    result
}

fn fold_rejection<D: Ord + Clone, CH: Ord + Clone>(
    base_balance: &BTreeMap<CH, i64>,
    to_merge: &BTreeSet<D>,
    dependency_map: &BTreeMap<D, BTreeSet<D>>,
    mergeable_diffs: &BTreeMap<D, BTreeMap<CH, i64>>,
) -> BTreeSet<D> {
    let branches = compute_branches(to_merge, dependency_map);
    let mut concurrent_roots: Vec<D> = branches.keys().cloned().collect();
    concurrent_roots.sort();

    let mut balances = base_balance.clone();
    let mut rejected: BTreeSet<D> = BTreeSet::new();
    for root in &concurrent_roots {
        let deps_of = |d: &D| dependency_map.get(d).cloned().unwrap_or_default();
        let branch = traverse_tree(root, &deps_of);
        for deploy in &branch {
            if rejected.contains(deploy) {
                continue;
            }
            match calc_merged_result(deploy, &balances, mergeable_diffs) {
                Some(new_balances) => balances = new_balances,
                None => {
                    let singleton: BTreeSet<D> = std::iter::once(deploy.clone()).collect();
                    let deps = with_dependencies(&singleton, dependency_map);
                    rejected.insert(deploy.clone());
                    rejected.extend(deps);
                }
            }
        }
    }
    rejected
}

/// Extend the rejection options with rejections forced by mergeable-value overflow.
pub fn add_mergeable_overflow_rejections<D: Ord + Clone, CH: Ord + Clone>(
    conflict_set: &BTreeSet<D>,
    dependency_map: &BTreeMap<D, BTreeSet<D>>,
    reject_options: &BTreeSet<BTreeSet<D>>,
    init_mergeable_values: &BTreeMap<CH, i64>,
    mergeable_diffs: &BTreeMap<D, BTreeMap<CH, i64>>,
) -> BTreeSet<BTreeSet<D>> {
    if reject_options.is_empty() {
        let r = fold_rejection(init_mergeable_values, conflict_set, dependency_map, mergeable_diffs);
        std::iter::once(r).collect()
    } else {
        reject_options
            .iter()
            .map(|rj| {
                let diff: BTreeSet<D> = conflict_set.difference(rj).cloned().collect();
                let fr = fold_rejection(init_mergeable_values, &diff, dependency_map, mergeable_diffs);
                rj.union(&fr).cloned().collect()
            })
            .collect()
    }
}

/// Compute the resolution for a conflict set: `(accepted, rejected)`.
#[allow(clippy::too_many_arguments)]
pub fn resolve_conflict_set<D, CH, F>(
    conflict_set: &BTreeSet<D>,
    accepted_finally: &BTreeSet<D>,
    rejected_finally: &BTreeSet<D>,
    cost: F,
    conflicts_map: &BTreeMap<D, BTreeSet<D>>,
    dependency_map: &BTreeMap<D, BTreeSet<D>>,
    mergeable_diffs: &BTreeMap<D, BTreeMap<CH, i64>>,
    init_mergeable_values: &BTreeMap<CH, i64>,
) -> (BTreeSet<D>, BTreeSet<D>)
where
    D: Ord + Clone,
    CH: Ord + Clone,
    F: Fn(&D) -> i64,
{
    let enforce_rejected = with_dependencies(
        &incompatible_with_final(accepted_finally, rejected_finally, conflicts_map, dependency_map),
        dependency_map,
    );
    let conflict_set_compatible: BTreeSet<D> =
        conflict_set.difference(&enforce_rejected).cloned().collect();

    let full_conflicts_map: BTreeMap<D, BTreeSet<D>> = conflicts_map
        .iter()
        .map(|(k, vs)| {
            let deps = with_dependencies(vs, dependency_map);
            (k.clone(), vs.union(&deps).cloned().collect())
        })
        .collect();

    let rejection_options = compute_rejection_options(&full_conflicts_map);
    let mergeable_overflow_rejection_options = add_mergeable_overflow_rejections(
        conflict_set,
        dependency_map,
        &rejection_options,
        init_mergeable_values,
        mergeable_diffs,
    );
    let resolved = compute_optimal_rejection(&mergeable_overflow_rejection_options, &cost);
    (
        conflict_set_compatible.difference(&resolved).cloned().collect(),
        resolved.union(&enforce_rejected).cloned().collect(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{BTreeMap, BTreeSet};

    fn set<D: Ord + Clone>(items: impl IntoIterator<Item = D>) -> BTreeSet<D> {
        items.into_iter().collect()
    }

    fn map<D: Ord + Clone>(items: impl IntoIterator<Item = (D, BTreeSet<D>)>) -> BTreeMap<D, BTreeSet<D>> {
        items.into_iter().collect()
    }

    #[test]
    fn with_dependencies_collects_transitive_closure() {
        let dependents_map = map([
            (1, set([3, 9])),
            (3, set([5])),
            (5, set([6])),
            (4, set([6])),
        ]);
        let rejects = with_dependencies(&set([1]), &dependents_map);
        assert_eq!(rejects, set([1, 3, 9, 5, 6]));
    }

    #[test]
    fn incompatible_with_final_combines_conflicts_and_dependents() {
        let accepted_finally = set([1, 2]);
        let rejected_finally = set([5, 6]);
        let conflicts_map = map([(1, set([11, 12])), (2, set([21, 22])), (3, set([31, 32]))]);
        let dependency_map = map([(5, set([51, 52])), (6, set([61, 62])), (7, set([71, 72]))]);
        let r = incompatible_with_final(
            &accepted_finally,
            &rejected_finally,
            &conflicts_map,
            &dependency_map,
        );
        assert_eq!(r, set([11, 12, 21, 22, 51, 52, 61, 62]));
    }

    #[test]
    fn partition_scope_yields_non_intersecting_partitions() {
        let views = vec![
            set([1, 2, 3, 4]),
            set([4, 5, 6, 7]),
            set([7, 8, 9]),
            set([9, 10]),
        ];
        let r = partition_scope(&views);
        assert_eq!(
            r,
            vec![set([1, 2, 3, 4]), set([5, 6, 7]), set([8, 9]), set([10])]
        );
    }

    #[test]
    fn compute_conflicts_map_is_bidirectional_without_self() {
        let set_all = set([1, 2, 3, 4, 5, 6]);
        let conflicts_map = map([(1, set([2, 3])), (4, set([5])), (6, set([6]))]);
        // mirror: v -> {k} for each (k, v)
        let mut reference: BTreeMap<i32, BTreeSet<i32>> = BTreeMap::new();
        for (k, vs) in &conflicts_map {
            for v in vs {
                reference.entry(*v).or_default().insert(*k);
            }
        }
        for (k, vs) in &conflicts_map {
            reference.entry(*k).or_default().extend(vs.iter().cloned());
        }
        reference.remove(&6); // self-conflict excluded

        let conflicts = |a: &i32, b: &i32| reference.get(a).map(|s| s.contains(b)).unwrap_or(false);
        assert_eq!(
            compute_conflicts_map(&set_all, &set_all, conflicts),
            reference
        );
    }

    #[test]
    fn compute_dependency_map_keys_are_dependencies() {
        let set_all = set([1, 2, 3, 4, 5, 6]);
        let dependency_map = map([
            (1, set([2, 3])),
            (4, set([5])),
            (3, set([1])),
            (6, set([6])),
        ]);
        let depends =
            |target: &i32, maybe_dependency: &i32| {
                dependency_map
                    .get(maybe_dependency)
                    .map(|s| s.contains(target))
                    .unwrap_or(false)
            };
        let mut expected = dependency_map.clone();
        expected.remove(&6);
        assert_eq!(compute_dependency_map(&set_all, &set_all, depends), expected);
    }

    #[test]
    fn compute_branches_covers_target_with_concurrent_roots() {
        let set_all = set([1, 2, 3, 4, 5, 6, 7, 100, 101]);
        let dependency_map = map([
            (1, set([4, 5])),
            (4, set([5, 6])),
            (2, set([4, 5, 6])),
            (3, set([6, 7])),
        ]);
        let expected = map([
            (1, set([4, 5, 6])),
            (2, set([4, 5, 6])),
            (3, set([6, 7])),
            (100, set([])),
            (101, set([])),
        ]);
        assert_eq!(compute_branches(&set_all, &dependency_map), expected);
    }

    #[test]
    fn compute_relation_map_for_merge_set_combines_internal_and_final() {
        let conflict_set = set([1, 2]);
        let final_set = set([3, 4]);
        let conflicts_map = map([(1, set([2])), (2, set([1]))]);
        let depends_map = map([(2, set([1])), (3, set([2]))]);
        let conflicts =
            |a: &i32, b: &i32| conflicts_map.get(a).map(|s| s.contains(b)).unwrap_or(false);
        let depends =
            |t: &i32, m: &i32| depends_map.get(m).map(|s| s.contains(t)).unwrap_or(false);
        let r = compute_relation_map_for_merge_set(&conflict_set, &final_set, conflicts, depends);
        assert_eq!(r, (conflicts_map, depends_map));
    }

    #[test]
    fn compute_rejection_options_matches_oracle() {
        assert_eq!(
            compute_rejection_options(&map([
                (1, set([2, 3, 4])),
                (2, set([1])),
                (3, set([1, 2])),
                (4, set([1])),
            ])),
            set([set([1, 2]), set([2, 3, 4])])
        );

        assert_eq!(
            compute_rejection_options(&map([
                (1, set([2, 3, 4])),
                (2, set([1, 3, 4])),
                (3, set([1, 2, 4])),
                (4, set([1, 2, 3])),
            ])),
            set([set([2, 3, 4]), set([1, 3, 4]), set([1, 2, 4]), set([1, 2, 3])])
        );

        assert_eq!(
            compute_rejection_options(&map([
                (1, set([2, 3, 4])),
                (2, set([1])),
                (3, set([1, 4])),
                (4, set([1, 3])),
            ])),
            set([set([2, 3, 4]), set([1, 3]), set([1, 4])])
        );

        assert_eq!(
            compute_rejection_options(&map([
                (1, set::<i32>([])),
                (2, set([3])),
                (3, set([2, 4])),
                (4, set([3])),
            ])),
            set([set([3]), set([2, 4])])
        );

        // Full graph on 1000 nodes.
        let all: BTreeSet<i32> = (1..=1000).collect();
        let conflicts_map: BTreeMap<i32, BTreeSet<i32>> = (1..=1000)
            .map(|i| {
                let mut v = all.clone();
                v.remove(&i);
                (i, v)
            })
            .collect();
        let expected: BTreeSet<BTreeSet<i32>> = (1..=1000)
            .map(|i| {
                let mut v = all.clone();
                v.remove(&i);
                v
            })
            .collect();
        assert_eq!(compute_rejection_options(&conflicts_map), expected);
    }

    #[test]
    fn compute_optimal_rejection_minimizes_cost_then_size() {
        let rejection_options = set([set([1, 2, 3]), set([2, 3, 4]), set([1, 2]), set([2]), set([1])]);
        // Every deploy costs 1; among the minimal-cost options the smallest set (then the
        // lexicographically smallest) wins — {1}.
        let cost_fn = |_d: &i32| 1i64;
        assert_eq!(compute_optimal_rejection(&rejection_options, cost_fn), set([1]));
    }

    #[test]
    fn add_mergeable_overflow_rejections_folds_by_branch() {
        let conflict_set = set([1, 2, 3, 4, 5, 6, 7]);
        let dependency_map = map([(1, set([2])), (3, set([4])), (4, set([5]))]);
        let reject_options: BTreeSet<BTreeSet<i32>> = set([]);
        let init: BTreeMap<String, i64> = [("a".to_string(), 0)].into_iter().collect();
        let mut md: BTreeMap<i32, BTreeMap<String, i64>> = BTreeMap::new();
        md.insert(1, [("a".to_string(), 10)].into_iter().collect());
        md.insert(2, [("a".to_string(), -5)].into_iter().collect());
        md.insert(3, [("a".to_string(), 15)].into_iter().collect());
        md.insert(4, [("a".to_string(), 10)].into_iter().collect());
        md.insert(5, [("a".to_string(), -20)].into_iter().collect());
        md.insert(6, [("a".to_string(), -10)].into_iter().collect());
        md.insert(7, [("a".to_string(), -10)].into_iter().collect());
        let r = add_mergeable_overflow_rejections(
            &conflict_set,
            &dependency_map,
            &reject_options,
            &init,
            &md,
        );
        assert_eq!(r, set([set([7])]));
    }

    #[test]
    fn add_mergeable_overflow_rejections_rejects_dependent_tree() {
        let conflict_set = set([1, 2, 3, 4, 5, 6, 7, 12]);
        let dependency_map = map([(1, set([2])), (2, set([12])), (3, set([4, 12])), (4, set([5]))]);
        let reject_options: BTreeSet<BTreeSet<i32>> = set([]);
        let init: BTreeMap<String, i64> = [("a".to_string(), 5)].into_iter().collect();
        let mut md: BTreeMap<i32, BTreeMap<String, i64>> = BTreeMap::new();
        md.insert(1, [("a".to_string(), -10)].into_iter().collect());
        md.insert(2, [("a".to_string(), -5)].into_iter().collect());
        md.insert(3, [("a".to_string(), 15)].into_iter().collect());
        md.insert(4, [("a".to_string(), 10)].into_iter().collect());
        md.insert(5, [("a".to_string(), -20)].into_iter().collect());
        md.insert(6, [("a".to_string(), -10)].into_iter().collect());
        md.insert(7, [("a".to_string(), -10)].into_iter().collect());
        md.insert(12, [("a".to_string(), 10)].into_iter().collect());
        let r = add_mergeable_overflow_rejections(
            &conflict_set,
            &dependency_map,
            &reject_options,
            &init,
            &md,
        );
        assert_eq!(r, set([set([1, 2, 12, 7])]));
    }
}
