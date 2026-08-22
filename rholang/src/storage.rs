//! The rholang↔rspace runtime bridge (port of `interpreter/storage/`).
//!
//! `RhoHistoryRepository` specializes `rspace::HistoryRepository` to the rholang types;
//! [`ChargingRSpace`] adapts the async rspace `Tuplespace` to the async rholang `reduce::Tuplespace`.

use std::collections::BTreeSet;
use std::sync::Arc;

use async_trait::async_trait;
use rchain_models::ast::{EList, Expr, Par, Var};
use rchain_models::par_ops::from_expr;
use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_rspace::history::history_repository::HistoryRepository;
use rchain_rspace::match_::Match;
use rchain_rspace::tuple_space::{
    ContResult, Result as RSpaceResult, Tuplespace as RSpaceTuplespace,
};

use crate::accounting::{CostAccounting, Costs};
use crate::errors::RholangError;
use crate::matcher::{fold_match, spatial_match, FreeMap};
use crate::reduce::{Application, Tuplespace};

/// The rholang history repository (port of `RhoHistoryRepository`).
pub type RhoHistoryRepository =
    Arc<HistoryRepository<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>>;

/// The rholang tuplespace (port of `RhoTuplespace`).
pub type RhoTuplespace =
    Arc<dyn RSpaceTuplespace<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>>;

/// Convert an rspace produce/consume result into the rholang `Application` (port of
/// `unpackOptionWithPeek`).
pub fn to_application(
    r: Option<(
        ContResult<SortedProc, BindPattern, TaggedContinuation>,
        Vec<RSpaceResult<SortedProc, ListParWithRandom>>,
    )>,
) -> Application {
    r.map(|(cont, data)| {
        (
            cont.continuation,
            data.into_iter()
                .map(|d| (d.channel, d.matched_datum, d.removed_datum, d.persistent))
                .collect(),
            cont.peek,
        )
    })
}

/// The spatial matcher instance for `(BindPattern, ListParWithRandom)` (port of `matchListPar`).
#[derive(Clone)]
pub struct RhoMatch;

impl Match<BindPattern, ListParWithRandom> for RhoMatch {
    fn get(&self, pattern: &BindPattern, data: &ListParWithRandom) -> Option<ListParWithRandom> {
        let data_pars: Vec<Par> = data.pars.iter().map(|p| p.as_par().clone()).collect();
        let pattern_pars: Vec<Par> = pattern.patterns.iter().map(|p| p.as_par().clone()).collect();
        let matches = fold_match(
            &data_pars,
            &pattern_pars,
            pattern.remainder.as_ref(),
            &FreeMap::new(),
            &spatial_match,
        )
        .ok()?;
        let (caught_rem, free_map) = matches.into_iter().next()?;

        let mut remainder_map = free_map;
        if let Some(Var::FreeVar(level)) = pattern.remainder.as_ref() {
            remainder_map.insert(
                *level,
                from_expr(Expr::EList(EList {
                    ps: caught_rem,
                    ..Default::default()
                })),
            );
        }

        let pars = (0..pattern.free_count)
            .map(|i| SortedProc::new(remainder_map.get(&i).cloned().unwrap_or_default()))
            .collect();
        Some(ListParWithRandom {
            pars,
            random_state: data.random_state.clone(),
        })
    }
}

/// The charging tuplespace bridge: adapts the async rspace to the async rholang `Tuplespace` (port
/// of `ChargingRSpace`). Charges the produce/consume storage + event/COMM costs (C-2); the Scala
/// storage *refund* on a matched continuation is not yet modeled (safe over-charge).
#[derive(Clone)]
pub struct ChargingRSpace {
    space: RhoTuplespace,
    cost: Arc<CostAccounting>,
}

impl ChargingRSpace {
    /// Wrap `space` with the cost cell, charging produce/consume (port of `chargingRSpace`).
    pub fn new(space: RhoTuplespace, cost: Arc<CostAccounting>) -> Self {
        ChargingRSpace { space, cost }
    }
}

#[async_trait]
impl Tuplespace for ChargingRSpace {
    async fn produce(
        &self,
        channel: &SortedProc,
        data: ListParWithRandom,
        persist: bool,
    ) -> Result<Application, RholangError> {
        self.cost.charge(Costs::storage_cost_produce(channel, &data))?;
        let result = self
            .space
            .produce(channel.clone(), data, persist)
            .await
            .map_err(|e| RholangError::ReduceError(e.to_string()))?;
        match &result {
            None => self.cost.charge(Costs::event_storage_cost(1))?,
            Some((cont, _)) => {
                if !persist {
                    self.cost.charge(Costs::event_storage_cost(1))?;
                }
                self.cost
                    .charge(Costs::comm_event_storage_cost(cont.channels.len() as i64))?;
            }
        }
        Ok(to_application(result))
    }

    async fn consume(
        &self,
        channels: &[SortedProc],
        patterns: &[BindPattern],
        continuation: TaggedContinuation,
        persist: bool,
        peeks: BTreeSet<usize>,
    ) -> Result<Application, RholangError> {
        self.cost
            .charge(Costs::storage_cost_consume(channels, patterns, &continuation))?;
        let result = self
            .space
            .consume(channels, patterns, continuation, persist, peeks)
            .await
            .map_err(|e| RholangError::ReduceError(e.to_string()))?;
        match &result {
            None => self
                .cost
                .charge(Costs::event_storage_cost(channels.len() as i64))?,
            Some((cont, _)) => {
                if !persist {
                    self.cost
                        .charge(Costs::event_storage_cost(channels.len() as i64))?;
                }
                self.cost
                    .charge(Costs::comm_event_storage_cost(cont.channels.len() as i64))?;
            }
        }
        Ok(to_application(result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
    use rchain_models::ast::Par;
    use rchain_rspace::errors::RSpaceError;
    use std::sync::Mutex;

    use crate::accounting::Cost;

    fn par(exprs: Vec<Expr>) -> Par {
        Par {
            exprs,
            ..Default::default()
        }
    }

    fn lpw(pars: Vec<Par>) -> ListParWithRandom {
        ListParWithRandom {
            pars: pars.into_iter().map(SortedProc::new).collect(),
            random_state: Blake2b512Random::new_random(128),
        }
    }

    struct MockSpace {
        produced: Mutex<Vec<(SortedProc, ListParWithRandom, bool)>>,
    }

    #[async_trait]
    impl RSpaceTuplespace<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation> for MockSpace {
        async fn consume(
            &self,
            _channels: &[SortedProc],
            _patterns: &[BindPattern],
            _continuation: TaggedContinuation,
            _persist: bool,
            _peeks: BTreeSet<usize>,
        ) -> Result<
            Option<(
                ContResult<SortedProc, BindPattern, TaggedContinuation>,
                Vec<RSpaceResult<SortedProc, ListParWithRandom>>,
            )>,
            RSpaceError,
        > {
            Ok(None)
        }

        async fn produce(
            &self,
            channel: SortedProc,
            data: ListParWithRandom,
            persist: bool,
        ) -> Result<
            Option<(
                ContResult<SortedProc, BindPattern, TaggedContinuation>,
                Vec<RSpaceResult<SortedProc, ListParWithRandom>>,
            )>,
            RSpaceError,
        > {
            self.produced
                .lock()
                .unwrap_or_else(|p| p.into_inner())
                .push((channel, data, persist));
            Ok(None)
        }

        async fn install(
            &self,
            _channels: &[SortedProc],
            _patterns: &[BindPattern],
            _continuation: TaggedContinuation,
        ) -> Result<Option<(TaggedContinuation, Vec<ListParWithRandom>)>, RSpaceError> {
            Ok(None)
        }
    }

    #[tokio::test]
    async fn charging_rspace_charges_and_enforces_balance() {
        let mock: RhoTuplespace = Arc::new(MockSpace {
            produced: Mutex::new(Vec::new()),
        });
        let cost = Arc::new(CostAccounting::from_initial(Cost::new(1_000_000, "init")));
        let charging = ChargingRSpace::new(mock, cost.clone());

        charging
            .produce(&SortedProc::new(par(vec![Expr::GInt(1)])), lpw(vec![par(vec![Expr::GInt(2)])]), false)
            .await
            .unwrap();
        assert!(cost.total_charged() > 0, "produce must charge storage/event cost");

        // A near-zero balance is exhausted by the upfront storage charge.
        let tiny_cost = Arc::new(CostAccounting::from_initial(Cost::new(1, "tiny")));
        let tiny = ChargingRSpace::new(
            Arc::new(MockSpace {
                produced: Mutex::new(Vec::new()),
            }),
            tiny_cost,
        );
        let err = tiny
            .produce(&SortedProc::new(par(vec![Expr::GInt(1)])), lpw(vec![par(vec![Expr::GInt(2)])]), false)
            .await;
        assert!(err.is_err(), "exhausted balance must fail produce");
    }

    #[test]
    fn rho_match_binds_free_vars() {
        let pattern = BindPattern {
            patterns: vec![SortedProc::new(Par {
                exprs: vec![Expr::EVar(Box::new(Var::FreeVar(0)))],
                connective_used: true,
                ..Default::default()
            })],
            remainder: None,
            free_count: 1,
        };
        let data = ListParWithRandom {
            pars: vec![SortedProc::new(par(vec![Expr::GInt(42)]))],
            random_state: rchain_crypto::hash::blake2b512_random::Blake2b512Random::new_random(128),
        };
        let result = RhoMatch.get(&pattern, &data).unwrap();
        assert_eq!(result.pars, vec![SortedProc::new(par(vec![Expr::GInt(42)]))]);
    }

    #[test]
    fn to_application_converts() {
        let cont = ContResult {
            continuation: TaggedContinuation::Empty,
            persistent: false,
            channels: vec![SortedProc::new(par(vec![Expr::GInt(1)]))],
            patterns: vec![],
            peek: true,
        };
        let data = RSpaceResult {
            channel: SortedProc::new(par(vec![Expr::GInt(1)])),
            matched_datum: ListParWithRandom {
                pars: vec![],
                random_state: rchain_crypto::hash::blake2b512_random::Blake2b512Random::new_random(128),
            },
            removed_datum: ListParWithRandom {
                pars: vec![],
                random_state: rchain_crypto::hash::blake2b512_random::Blake2b512Random::new_random(128),
            },
            persistent: false,
        };
        let app = to_application(Some((cont, vec![data]))).unwrap();
        assert!(matches!(app.0, TaggedContinuation::Empty));
        assert!(app.2);
        assert_eq!(app.1.len(), 1);
    }
}
