//! Runtime wrapper types used by the evaluator (port of the protobuf `TaggedContinuation`,
//! `ParWithRandom`, `ListParWithRandom`, `BindPattern` messages).

use rchain_crypto::hash::blake2b512_random::Blake2b512Random;

use crate::ast::Var;
use crate::sorted::SortedProc;

/// A continuation: either rholang code or a reference to built-in code (port of `TaggedContinuation`).
#[derive(Clone, Debug)]
pub enum TaggedContinuation {
    ParBody(ParWithRandom),
    ScalaBodyRef(i64),
    Empty,
}

/// Rholang code plus the state of a split random generator (port of `ParWithRandom`).
#[derive(Clone, Debug)]
pub struct ParWithRandom {
    pub body: SortedProc,
    pub random_state: Blake2b512Random,
}

/// A list of canonically-sorted `Par`s plus a split random state (port of `ListParWithRandom`).
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ListParWithRandom {
    pub pars: Vec<SortedProc>,
    pub random_state: Blake2b512Random,
}

/// A bound receive pattern (port of `BindPattern`).
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BindPattern {
    pub patterns: Vec<SortedProc>,
    pub remainder: Option<Var>,
    pub free_count: i32,
}
