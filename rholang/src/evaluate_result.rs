//! The result of evaluating a deploy (port of `interpreter/EvaluateResult`).

use std::collections::BTreeSet;

use rchain_models::ast::Par;

use crate::accounting::Cost;
use crate::errors::RholangError;

/// The result of reducing a term: the gas consumed, any interpreter errors, and the mergeable
/// (number) channels produced.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EvaluateResult {
    pub cost: Cost,
    pub errors: Vec<RholangError>,
    pub mergeable: BTreeSet<Par>,
}

impl EvaluateResult {
    pub fn failed(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn succeeded(&self) -> bool {
        self.errors.is_empty()
    }
}
