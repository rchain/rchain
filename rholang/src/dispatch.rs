//! Continuation dispatch (port of `dispatch.scala`).

use std::collections::BTreeMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};

use async_trait::async_trait;
use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::ast::Par;
use rchain_models::runtime::{ListParWithRandom, TaggedContinuation};

use crate::env::Env;
use crate::errors::RholangError;
use crate::reduce::Dispatch;

/// A built-in continuation handler (port of the dispatch-table function).
pub type ScalaBodyFn = Box<
    dyn Fn(Vec<ListParWithRandom>) -> Pin<Box<dyn Future<Output = Result<(), RholangError>> + Send>>
        + Send
        + Sync,
>;

/// The `ParBody` continuation evaluator: evals a body in the env built from the matched data with
/// the merged random state.
pub type EvalBodyFn = Box<
    dyn Fn(Par, Env<Par>, Blake2b512Random)
        -> Pin<Box<dyn Future<Output = Result<(), RholangError>> + Send>>
        + Send
        + Sync,
>;

/// Build an environment from the data captured by a match (port of `Dispatch.buildEnv`).
pub fn build_env(data_list: &[ListParWithRandom]) -> Env<Par> {
    Env::make_env(data_list.iter().flat_map(|d| d.pars.iter().map(|p| p.as_par().clone())))
}

/// Dispatches a continuation: eval `ParBody`, invoke the built-in handler for `ScalaBodyRef`, or
/// no-op for `Empty` (port of `RholangAndScalaDispatcher`).
///
/// `eval` is set after construction to break the reducer↔dispatcher cycle.
pub struct RholangAndScalaDispatcher {
    dispatch_table: Mutex<BTreeMap<i64, ScalaBodyFn>>,
    eval: Mutex<Option<EvalBodyFn>>,
}

impl RholangAndScalaDispatcher {
    pub fn new(dispatch_table: BTreeMap<i64, ScalaBodyFn>) -> Self {
        RholangAndScalaDispatcher {
            dispatch_table: Mutex::new(dispatch_table),
            eval: Mutex::new(None),
        }
    }

    pub fn set_eval(&self, eval: EvalBodyFn) {
        *self.eval.lock().unwrap_or_else(|p| p.into_inner()) = Some(eval);
    }

    pub fn set_dispatch_table(&self, table: BTreeMap<i64, ScalaBodyFn>) {
        *self.dispatch_table.lock().unwrap_or_else(|p| p.into_inner()) = table;
    }
}

#[async_trait]
impl Dispatch for RholangAndScalaDispatcher {
    async fn dispatch(
        &self,
        continuation: TaggedContinuation,
        data_list: Vec<ListParWithRandom>,
    ) -> Result<(), RholangError> {
        match &continuation {
            TaggedContinuation::ParBody(pwr) => {
                let env = build_env(&data_list);
                let mut randoms: Vec<Blake2b512Random> = vec![pwr.random_state.clone()];
                randoms.extend(data_list.iter().map(|d| d.random_state.clone()));
                let merged = Blake2b512Random::merge(&randoms);
                let fut = {
                    let eval = self.eval.lock().unwrap_or_else(|p| p.into_inner());
                    let f = eval.as_ref().ok_or_else(|| {
                        RholangError::BugFoundError("dispatcher eval not set".to_string())
                    })?;
                    f(pwr.body.as_par().clone(), env, merged)
                };
                fut.await
            }
            TaggedContinuation::ScalaBodyRef(r) => {
                let fut = {
                    let table = self.dispatch_table.lock().unwrap_or_else(|p| p.into_inner());
                    match table.get(r) {
                        Some(f) => f(data_list),
                        None => {
                            return Err(RholangError::ReduceError(format!(
                                "dispatch: no function for {r}"
                            )))
                        }
                    }
                };
                fut.await
            }
            TaggedContinuation::Empty => Ok(()),
        }
    }
}

#[async_trait]
impl Dispatch for Arc<RholangAndScalaDispatcher> {
    async fn dispatch(
        &self,
        continuation: TaggedContinuation,
        data_list: Vec<ListParWithRandom>,
    ) -> Result<(), RholangError> {
        self.as_ref().dispatch(continuation, data_list).await
    }
}
