//! The ρ-calculus spatial matcher (Law 5: a free variable is bound at most once).
//!
//! Mirrors `rholang/.../interpreter/matcher/`. The Scala `MatcherMonadT`/`StreamT`/`StateT` effect
//! stack is ported to concrete backtracking over `Vec<FreeMap>`.

use std::collections::BTreeMap;

use rchain_models::ast::Par;

pub mod maximum_bipartite_match;
pub mod par_count;
pub mod par_spatial_matcher_utils;
pub mod spatial_matcher;

/// The mapping from free-variable levels to the `Par`s they capture (port of `FreeMap`).
pub type FreeMap = BTreeMap<i32, Par>;

pub use spatial_matcher::{spatial_match, spatial_match_result, MatchableTerm};
pub(crate) use spatial_matcher::fold_match;
