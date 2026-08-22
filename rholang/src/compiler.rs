//! Normalizer variable contexts (port of `interpreter/compiler/{BoundContext,FreeContext,
//! BoundMap,BoundMapChain,FreeMap,package}.scala`).
//!
//! These track free variables (de Bruijn *levels*, 0-based) and bound variables (de Bruijn
//! *indices*) during normalization of the concrete rholang AST into `Par`.

use std::collections::BTreeMap;

use rchain_models::ast::{Connective, Expr, Par};

use crate::errors::SourcePosition;

/// An identifier binding: `(name, sort, source position)` (port of `IdContext`).
pub type IdContext<T> = (String, T, SourcePosition);

/// A bound variable context (port of `BoundContext`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoundContext<T> {
    pub index: i32,
    pub typ: T,
    pub source_position: SourcePosition,
}

/// A free variable context (port of `FreeContext`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FreeContext<T> {
    pub level: i32,
    pub typ: T,
    pub source_position: SourcePosition,
}

/// Bound-variable map using de Bruijn indices (port of `BoundMap`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoundMap<T> {
    next_index: i32,
    index_bindings: BTreeMap<String, BoundContext<T>>,
}

impl<T: Clone> BoundMap<T> {
    pub fn empty() -> Self {
        BoundMap {
            next_index: 0,
            index_bindings: BTreeMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<BoundContext<T>> {
        self.index_bindings.get(name).map(|bc| BoundContext {
            index: self.next_index - bc.index - 1,
            typ: bc.typ.clone(),
            source_position: bc.source_position.clone(),
        })
    }

    pub fn put(&self, binding: &IdContext<T>) -> Self {
        let (name, typ, source_position) = binding.clone();
        let mut bindings = self.index_bindings.clone();
        bindings.insert(
            name,
            BoundContext {
                index: self.next_index,
                typ,
                source_position,
            },
        );
        BoundMap {
            next_index: self.next_index + 1,
            index_bindings: bindings,
        }
    }

    pub fn put_all(&self, bindings: &[IdContext<T>]) -> Self {
        bindings.iter().fold(self.clone(), |m, b| m.put(b))
    }

    /// Absorb a free map's bindings as bound contexts, shifting their levels by `next_index`.
    pub fn absorb_free(&self, level_map: &FreeMap<T>) -> Self {
        let mut bindings = self.index_bindings.clone();
        for (name, FreeContext { level, typ, source_position }) in &level_map.level_bindings {
            bindings.insert(
                name.clone(),
                BoundContext {
                    index: level + self.next_index,
                    typ: typ.clone(),
                    source_position: source_position.clone(),
                },
            );
        }
        BoundMap {
            next_index: self.next_index + level_map.next_level,
            index_bindings: bindings,
        }
    }

    pub fn count(&self) -> i32 {
        self.next_index
    }
}

/// A chain of bound maps for nested patterns (port of `BoundMapChain`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoundMapChain<T> {
    chain: Vec<BoundMap<T>>,
}

impl<T: Clone> BoundMapChain<T> {
    pub fn empty() -> Self {
        BoundMapChain {
            chain: vec![BoundMap::empty()],
        }
    }

    pub fn get(&self, name: &str) -> Option<BoundContext<T>> {
        self.chain.first().and_then(|m| m.get(name))
    }

    pub fn find(&self, name: &str) -> Option<(BoundContext<T>, i32)> {
        self.chain
            .iter()
            .enumerate()
            .find_map(|(depth, m)| m.get(name).map(|bc| (bc, depth as i32)))
    }

    pub fn put(&self, binding: &IdContext<T>) -> Self {
        let mut chain = self.chain.clone();
        chain[0] = chain[0].put(binding);
        BoundMapChain { chain }
    }

    pub fn put_all(&self, bindings: &[IdContext<T>]) -> Self {
        let mut chain = self.chain.clone();
        chain[0] = chain[0].put_all(bindings);
        BoundMapChain { chain }
    }

    pub fn absorb_free(&self, binders: &FreeMap<T>) -> Self {
        let mut chain = self.chain.clone();
        chain[0] = chain[0].absorb_free(binders);
        BoundMapChain { chain }
    }

    pub fn push(&self) -> Self {
        let mut chain = vec![BoundMap::empty()];
        chain.extend(self.chain.iter().cloned());
        BoundMapChain { chain }
    }

    pub fn count(&self) -> i32 {
        self.chain.first().map(|m| m.count()).unwrap_or(0)
    }

    pub fn depth(&self) -> i32 {
        self.chain.len() as i32 - 1
    }
}

/// Free-variable map using de Bruijn levels (port of `FreeMap`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FreeMap<T> {
    next_level: i32,
    pub level_bindings: BTreeMap<String, FreeContext<T>>,
    pub wildcards: Vec<SourcePosition>,
    pub connectives: Vec<(Connective, SourcePosition)>,
}

impl<T: Clone> FreeMap<T> {
    pub fn empty() -> Self {
        FreeMap {
            next_level: 0,
            level_bindings: BTreeMap::new(),
            wildcards: Vec::new(),
            connectives: Vec::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<FreeContext<T>> {
        self.level_bindings.get(name).cloned()
    }

    pub fn put(&self, binding: &IdContext<T>) -> Self {
        let (name, typ, source_position) = binding.clone();
        let mut bindings = self.level_bindings.clone();
        bindings.insert(
            name,
            FreeContext {
                level: self.next_level,
                typ,
                source_position,
            },
        );
        FreeMap {
            next_level: self.next_level + 1,
            level_bindings: bindings,
            wildcards: self.wildcards.clone(),
            connectives: self.connectives.clone(),
        }
    }

    pub fn put_all(&self, bindings: &[IdContext<T>]) -> Self {
        bindings.iter().fold(self.clone(), |m, b| m.put(b))
    }

    /// Merge another free map, shifting its levels by `next_level`; returns the shadowed names as
    /// `(name, first_use, second_use)` so the caller does not need a second (fallible) lookup.
    pub fn merge(
        &self,
        free_map: &FreeMap<T>,
    ) -> (FreeMap<T>, Vec<(String, SourcePosition, SourcePosition)>) {
        let mut acc = self.level_bindings.clone();
        let mut shadowed = Vec::new();
        for (name, FreeContext { level, typ, source_position }) in &free_map.level_bindings {
            acc.insert(
                name.clone(),
                FreeContext {
                    level: level + self.next_level,
                    typ: typ.clone(),
                    source_position: source_position.clone(),
                },
            );
            if let Some(first) = self.level_bindings.get(name) {
                shadowed.push((
                    name.clone(),
                    first.source_position.clone(),
                    source_position.clone(),
                ));
            }
        }
        let mut wildcards = self.wildcards.clone();
        wildcards.extend(free_map.wildcards.iter().cloned());
        let mut connectives = self.connectives.clone();
        connectives.extend(free_map.connectives.iter().cloned());
        (
            FreeMap {
                next_level: self.next_level + free_map.next_level,
                level_bindings: acc,
                wildcards,
                connectives,
            },
            shadowed,
        )
    }

    pub fn add_wildcard(&self, source_position: SourcePosition) -> Self {
        let mut wildcards = self.wildcards.clone();
        wildcards.push(source_position);
        FreeMap {
            wildcards,
            ..self.clone()
        }
    }

    pub fn add_connective(&self, connective: Connective, source_position: SourcePosition) -> Self {
        let mut connectives = self.connectives.clone();
        connectives.push((connective, source_position));
        FreeMap {
            connectives,
            ..self.clone()
        }
    }

    pub fn count(&self) -> i32 {
        self.next_level + self.wildcards.len() as i32 + self.connectives.len() as i32
    }

    pub fn count_no_wildcards(&self) -> i32 {
        self.next_level
    }

    /// The de Bruijn level assigned to the next variable (port of `FreeMap.nextLevel`).
    pub fn next_level(&self) -> i32 {
        self.next_level
    }
}

/// The sort of a name/process variable (port of `VarSort`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VarSort {
    ProcSort,
    NameSort,
}

/// Input data to the process normalizer (port of `ProcVisitInputs`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcVisitInputs {
    pub par: Par,
    pub bound_map_chain: BoundMapChain<VarSort>,
    pub free_map: FreeMap<VarSort>,
    /// The normalizer environment: URI names (e.g. `sys:casper:deployerId`) to `Par` values, used
    /// to populate `New.injections`.
    pub env: BTreeMap<String, Par>,
}

/// Output of the process normalizer (port of `ProcVisitOutputs`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcVisitOutputs {
    pub par: Par,
    pub free_map: FreeMap<VarSort>,
}

/// Input data to the name normalizer (port of `NameVisitInputs`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NameVisitInputs {
    pub bound_map_chain: BoundMapChain<VarSort>,
    pub free_map: FreeMap<VarSort>,
    pub env: BTreeMap<String, Par>,
}

/// Output of the name normalizer (port of `NameVisitOutputs`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NameVisitOutputs {
    pub par: Par,
    pub free_map: FreeMap<VarSort>,
}

/// Input data to the collection normalizer (port of `CollectVisitInputs`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CollectVisitInputs {
    pub bound_map_chain: BoundMapChain<VarSort>,
    pub free_map: FreeMap<VarSort>,
    pub env: BTreeMap<String, Par>,
}

/// Output of the collection normalizer (port of `CollectVisitOutputs`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CollectVisitOutputs {
    pub expr: Expr,
    pub free_map: FreeMap<VarSort>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pos() -> SourcePosition {
        SourcePosition { row: 1, column: 1 }
    }

    #[test]
    fn free_map_assigns_sequential_levels() {
        let m: FreeMap<()> = FreeMap::empty();
        let m = m.put(&("x".to_string(), (), pos()));
        let m = m.put(&("y".to_string(), (), pos()));
        assert_eq!(m.get("x").unwrap().level, 0);
        assert_eq!(m.get("y").unwrap().level, 1);
        assert_eq!(m.count_no_wildcards(), 2);
    }

    #[test]
    fn bound_map_computes_indices() {
        let m: BoundMap<()> = BoundMap::empty();
        let m = m.put(&("x".to_string(), (), pos()));
        let m = m.put(&("y".to_string(), (), pos()));
        // de Bruijn index: 0 = innermost, 1 = next outer.
        assert_eq!(m.get("y").unwrap().index, 0);
        assert_eq!(m.get("x").unwrap().index, 1);
    }

    #[test]
    fn free_map_merge_shifts_levels_and_tracks_shadowing() {
        let a: FreeMap<()> = FreeMap::empty().put(&("x".to_string(), (), pos()));
        let b: FreeMap<()> = FreeMap::empty().put(&("y".to_string(), (), pos()));
        let (merged, shadowed) = a.merge(&b);
        assert_eq!(merged.get("x").unwrap().level, 0);
        assert_eq!(merged.get("y").unwrap().level, 1);
        assert!(shadowed.is_empty());

        // Shadowing: merging a map that re-binds `x`.
        let c: FreeMap<()> = FreeMap::empty().put(&("x".to_string(), (), pos()));
        let (_, shadowed) = a.merge(&c);
        assert_eq!(shadowed.len(), 1);
        assert_eq!(shadowed[0].0, "x");
    }

    #[test]
    fn bound_map_chain_find_reports_depth() {
        let chain: BoundMapChain<()> = BoundMapChain::empty();
        let chain = chain.put(&("x".to_string(), (), pos()));
        let chain = chain.push();
        let chain = chain.put(&("y".to_string(), (), pos()));
        assert_eq!(chain.find("y").unwrap().1, 0);
        assert_eq!(chain.find("x").unwrap().1, 1);
        assert_eq!(chain.depth(), 1);
    }
}
