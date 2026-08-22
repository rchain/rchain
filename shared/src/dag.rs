//! DAG traversal helpers.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/dag/DagOps.scala`. The lazy `Stream` result becomes
//! an eager `Vec`; the `F[_]`-effectful `bfTraverseF` is deferred until the async runtime lands.

use std::collections::{HashSet, VecDeque};
use std::hash::Hash;

/// Breadth-first traversal of the graph reachable from `start` via `neighbours`, each node once.
pub fn bf_traverse<A: Eq + Hash + Clone>(start: &[A], neighbours: impl Fn(&A) -> Vec<A>) -> Vec<A> {
    let mut queue: VecDeque<A> = start.iter().cloned().collect();
    let mut visited: HashSet<A> = HashSet::new();
    let mut result = Vec::new();

    while let Some(curr) = queue.pop_front() {
        if visited.contains(&curr) {
            continue;
        }
        visited.insert(curr.clone());
        result.push(curr.clone());
        for n in neighbours(&curr) {
            if !visited.contains(&n) {
                queue.push_back(n);
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::bf_traverse;
    use std::collections::BTreeMap;

    #[test]
    fn traverses_in_breadth_first_order() {
        // 1 -> [2, 3]; 2 -> [4]; 3 -> [5]; 4/5 -> []
        let graph: BTreeMap<i32, Vec<i32>> = [
            (1, vec![2, 3]),
            (2, vec![4]),
            (3, vec![5]),
            (4, vec![]),
            (5, vec![]),
        ]
        .into_iter()
        .collect();
        let order = bf_traverse(&[1], |n| graph.get(n).cloned().unwrap_or_default());
        assert_eq!(order, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn deduplicates_shared_neighbours() {
        // 1 -> [2, 3]; 2 -> [4]; 3 -> [4] (4 reachable from both)
        let graph: BTreeMap<i32, Vec<i32>> =
            [(1, vec![2, 3]), (2, vec![4]), (3, vec![4]), (4, vec![])]
                .into_iter()
                .collect();
        let order = bf_traverse(&[1], |n| graph.get(n).cloned().unwrap_or_default());
        assert_eq!(order, vec![1, 2, 3, 4]);
    }
}
