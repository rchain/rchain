//! The rholang runtime façade (port of `RhoRuntime.scala`, core).

use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, Mutex};

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::ast::{Bundle, Expr, Par, Var};
use rchain_models::par_ops::from_expr;
use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_models::types::Closed;
use rchain_rspace::checkpoint::{Checkpoint, SoftCheckpoint};
use rchain_rspace::errors::RSpaceError;
use rchain_rspace::i_replay_space::IReplaySpace;
use rchain_rspace::i_space::ISpace;
use rchain_rspace::internal::{Datum, Row, WaitingContinuation};
use rchain_rspace::native_store::InMemNativeStore;
use rchain_rspace::replay_rspace::ReplayRSpace;
use rchain_rspace::rspace::RSpace;
use rchain_rspace::trace::Log;
use rchain_rspace::tuple_space::Tuplespace as RSpaceTuplespace;
use rchain_rspace::util::ReplayException;

use crate::accounting::CostAccounting;
use crate::dispatch::RholangAndScalaDispatcher;
use crate::env::Env;
use crate::errors::RholangError;
use crate::evaluate_result::EvaluateResult;
use crate::native_state::NativeSystemState;
use crate::reduce::DebruijnInterpreter;
use crate::storage::{ChargingRSpace, RhoHistoryRepository, RhoTuplespace};
use crate::system_processes::{BlockData, SystemProcesses};

/// The concrete rspace type the runtime operates on.
pub type RhoSpace = Arc<RSpace<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>>;

/// The replay rspace type the replay runtime operates on (port of `RhoReplayISpace`).
pub type RhoReplaySpace = Arc<ReplayRSpace<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>>;

/// The reducer type wired to the charging space and dispatcher.
pub type RhoReducer =
    DebruijnInterpreter<ChargingRSpace, Arc<RholangAndScalaDispatcher>>;

/// Wire the reducer and dispatcher together (breaking their mutual recursion) and return the
/// reducer.
pub fn setup_reducer(
    charging_space: ChargingRSpace,
    cost: Arc<CostAccounting>,
    mergeable_tag_name: SortedProc,
) -> Arc<RhoReducer> {
    let dispatcher = Arc::new(RholangAndScalaDispatcher::new(BTreeMap::new()));
    let reducer = Arc::new(DebruijnInterpreter::new(
        charging_space,
        dispatcher.clone(),
        BTreeMap::new(),
        mergeable_tag_name,
    ));
    let reducer_for_eval = reducer.clone();
    let cost_for_eval = cost.clone();
    dispatcher.set_eval(Box::new(move |par, env, rand| {
        let reducer = reducer_for_eval.clone();
        let cost = cost_for_eval.clone();
        Box::pin(async move {
            reducer.eval(&par, &env, &rand, cost.as_ref()).await
        })
    }));
    reducer
}

/// The rholang runtime (port of `RhoRuntime`): `evaluate`/`inj`, checkpointing, and tuplespace reads.
pub struct RhoRuntime {
    reducer: Arc<RhoReducer>,
    space: RhoSpace,
    cost: Arc<CostAccounting>,
    block_data: Arc<Mutex<BlockData>>,
    _history: RhoHistoryRepository,
    proc_defs: Arc<Vec<(Par, i32, bool, i64)>>,
}

/// A write-only bundle over `channel` (port of `Bundle(channel, writeFlag = true)`).
fn write_bundle(channel: Par) -> Par {
    Par {
        bundles: vec![Bundle {
            body: Box::new(channel),
            write_flag: true,
            read_flag: false,
        }],
        ..Default::default()
    }
}

/// Install each system-contract definition as a persistent join on its fixed channel (port of
/// `RhoRuntime.introduceSystemProcesses`). The `space` is a `Tuplespace` so both the play `RSpace`
/// and the `ReplayRSpace` can be installed into.
async fn install_system_processes(
    space: &RhoTuplespace,
    proc_defs: &[(Par, i32, bool, i64)],
) -> Result<(), RholangError> {
    for (name, arity, remainder, body_ref) in proc_defs {
        let patterns = vec![BindPattern {
            patterns: (0..*arity)
                .map(|i| SortedProc::new(from_expr(Expr::EVar(Box::new(Var::FreeVar(i))))))
                .collect(),
            remainder: if *remainder { Some(Var::FreeVar(*arity)) } else { None },
            free_count: if *remainder { *arity + 1 } else { *arity },
        }];
        let continuation = TaggedContinuation::ScalaBodyRef(*body_ref);
        space
            .install(&[SortedProc::new(name.clone())], &patterns, continuation)
            .await
            .map_err(|e| RholangError::ReduceError(e.to_string()))?;
    }
    Ok(())
}

/// The shared reducer/system-process wiring built over a `Tuplespace` (port of `createRhoEnv` +
/// `setupReducer`). The play `RhoRuntime`, the replay `ReplayRhoRuntime`, and the reporting
/// `ReportingRuntime` reuse this core; the only difference is the concrete space each retains for
/// `ISpace`/`IReplaySpace` operations.
pub(crate) struct RuntimeCore {
    pub(crate) reducer: Arc<RhoReducer>,
    pub(crate) cost: Arc<CostAccounting>,
    pub(crate) block_data: Arc<Mutex<BlockData>>,
    pub(crate) proc_defs: Vec<(Par, i32, bool, i64)>,
}

pub(crate) async fn build_runtime_core(
    space: &RhoTuplespace,
    mergeable_tag_name: SortedProc,
    native_store: Arc<InMemNativeStore>,
) -> std::io::Result<RuntimeCore> {
    let cost = Arc::new(CostAccounting::from_initial(crate::accounting::Costs::unsafe_max()));
    let charging_space = ChargingRSpace::new(space.clone(), cost.clone());
    let block_data = Arc::new(Mutex::new(BlockData::empty()));
    let native_state = Arc::new(NativeSystemState::new(native_store));

    // Build the dispatcher (empty), then the system processes, then wire them together.
    let dispatcher = Arc::new(RholangAndScalaDispatcher::new(BTreeMap::new()));
    let system_processes = SystemProcesses::new(
        charging_space.clone(),
        dispatcher.clone(),
        block_data.clone(),
        native_state,
    );

    let mut dispatch_table = BTreeMap::new();
    let mut urn_map = BTreeMap::new();
    let mut proc_defs: Vec<(Par, i32, bool, i64)> = Vec::new();
    for d in system_processes.definitions() {
        dispatch_table.insert(d.body_ref, d.handler);
        urn_map.insert(d.urn, write_bundle(d.fixed_channel.clone()));
        proc_defs.push((d.fixed_channel, d.arity, d.remainder, d.body_ref));
    }

    dispatcher.set_dispatch_table(dispatch_table);
    install_system_processes(space, &proc_defs)
        .await
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;

    let reducer = Arc::new(DebruijnInterpreter::new(
        charging_space,
        dispatcher.clone(),
        urn_map,
        mergeable_tag_name,
    ));
    let reducer_for_eval = reducer.clone();
    let cost_for_eval = cost.clone();
    dispatcher.set_eval(Box::new(move |par, env, rand| {
        let reducer = reducer_for_eval.clone();
        let cost = cost_for_eval.clone();
        Box::pin(async move {
            reducer.eval(&par, &env, &rand, cost.as_ref()).await
        })
    }));

    Ok(RuntimeCore {
        reducer,
        cost,
        block_data,
        proc_defs,
    })
}

impl RhoRuntime {
    pub async fn create(
        space: RhoSpace,
        history: RhoHistoryRepository,
        mergeable_tag_name: SortedProc,
    ) -> std::io::Result<RhoRuntime> {
        let tuplespace: RhoTuplespace = space.clone();
        let native_store = space.native_store();
        let core = build_runtime_core(&tuplespace, mergeable_tag_name, native_store).await?;
        Ok(RhoRuntime {
            reducer: core.reducer,
            space,
            cost: core.cost,
            block_data: core.block_data,
            _history: history,
            proc_defs: Arc::new(core.proc_defs),
        })
    }

    /// Set the per-block data exposed to the `rho:block:data` contract (port of `setBlockData`).
    pub fn set_block_data(&self, block_data: BlockData) {
        *self.block_data.lock().unwrap_or_else(|p| p.into_inner()) = block_data;
    }

    /// Execute a `Closed` process in the given environment (port of `inj`). The `Closed` proof is
    /// discharged at this boundary; reduction then operates on the flat `Par` sub-terms.
    pub async fn inj(
        &self,
        par: &Closed,
        env: &Env<Par>,
        rand: &Blake2b512Random,
    ) -> Result<(), RholangError> {
        self.reducer
            .eval(&Par::from(par.clone()), env, rand, self.cost.as_ref())
            .await
    }

    /// Parse + run a rholang term (port of `evaluate`).
    pub async fn evaluate(
        &self,
        term: &str,
        rand: &Blake2b512Random,
    ) -> Result<EvaluateResult, RholangError> {
        self.evaluate_with_env(term, &BTreeMap::new(), rand).await
    }

    /// Parse + run a rholang term with an explicit normalizer environment (port of `evaluate`).
    pub async fn evaluate_with_env(
        &self,
        term: &str,
        env: &BTreeMap<String, Par>,
        rand: &Blake2b512Random,
    ) -> Result<EvaluateResult, RholangError> {
        let par = crate::normalizer::source_to_adt_with_env(term, env)?;
        let before = self.cost.total_charged();
        let errors = match self.inj(&par, &Env::new(), rand).await {
            Ok(()) => Vec::new(),
            Err(e) => vec![e],
        };
        let cost = self.cost.total_charged() - before;
        Ok(EvaluateResult {
            cost: crate::accounting::Cost::new(cost, "evaluate"),
            errors,
            mergeable: BTreeSet::new(),
        })
    }

    /// The empty-state hash: reset to the empty root, bootstrap the registry, and checkpoint (port
    /// of `emptyStateHash`).
    pub async fn empty_state_hash(&self) -> Result<Blake2b256Hash, String> {
        self.space
            .reset(rchain_rspace::history::history::empty_root_hash_value())
            .await
            .map_err(|e| e.to_string())?;
        // Re-install the system processes (stdout/crypto/registry/…): the reset above wipes the
        // installed continuations, so without this the genesis contracts would send to fixed channels
        // with no consumer (the silent-empty-registry failure mode). The registry handlers are native
        // `Definition`s, so there is no separate bootstrap AST to install.
        let ts: RhoTuplespace = self.space.clone();
        install_system_processes(&ts, &self.proc_defs)
            .await
            .map_err(|e| e.to_string())?;
        let checkpoint = self.space.create_checkpoint().await.map_err(|e| e.to_string())?;
        Ok(checkpoint.root)
    }

    pub fn space(&self) -> &RhoSpace {
        &self.space
    }

    /// The native system-contract store (shared with the system processes).
    pub fn native_store(&self) -> Arc<InMemNativeStore> {
        self.space.native_store()
    }

    pub fn cost(&self) -> &CostAccounting {
        self.cost.as_ref()
    }

    pub async fn create_checkpoint(&self) -> Result<Checkpoint, String> {
        self.space.create_checkpoint().await
    }

    pub async fn reset(&self, root: Blake2b256Hash) -> Result<(), String> {
        self.space.reset(root).await
    }

    /// Capture a soft (in-memory) checkpoint for rollback (port of `createSoftCheckpoint`).
    pub async fn create_soft_checkpoint(
        &self,
    ) -> SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation> {
        self.space.create_soft_checkpoint().await
    }

    /// Roll back to a soft checkpoint (port of `revertToSoftCheckpoint`).
    pub async fn revert_to_soft_checkpoint(
        &self,
        checkpoint: SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>,
    ) {
        self.space.revert_to_soft_checkpoint(checkpoint).await
    }

    pub async fn get_data(
        &self,
        channel: &SortedProc,
    ) -> Result<Vec<Datum<ListParWithRandom>>, RSpaceError> {
        self.space.get_data(channel).await
    }

    pub async fn get_joins(&self, channel: &SortedProc) -> Result<Vec<Vec<SortedProc>>, RSpaceError> {
        self.space.get_joins(channel).await
    }

    pub async fn get_continuation(
        &self,
        channels: &[SortedProc],
    ) -> Result<Vec<WaitingContinuation<BindPattern, TaggedContinuation>>, RSpaceError> {
        self.space.get_waiting_continuations(channels).await
    }

    /// Read all `Par`s at a channel (port of `getDataPar`).
    pub async fn get_data_par(&self, channel: &SortedProc) -> Result<Vec<Par>, RSpaceError> {
        let data = self.space.get_data(channel).await?;
        Ok(data.into_iter().flat_map(|d| d.a.pars.into_iter().map(|p| p.as_par().clone())).collect())
    }

    /// Read the waiting `ParBody` continuations as `(patterns, body)` (port of
    /// `getContinuationPar`).
    pub async fn get_continuation_par(
        &self,
        channels: &[SortedProc],
    ) -> Result<Vec<(Vec<BindPattern>, Par)>, RSpaceError> {
        let conts = self.space.get_waiting_continuations(channels).await?;
        Ok(conts
            .into_iter()
            .filter_map(|wc| match wc.continuation {
                TaggedContinuation::ParBody(pwr) => Some((wc.patterns, pwr.body.as_par().clone())),
                _ => None,
            })
            .collect())
    }

    /// Consume the result at a channel with a pattern (port of `consumeResult`).
    pub async fn consume_result(
        &self,
        channels: &[SortedProc],
        patterns: &[BindPattern],
    ) -> Result<Option<(TaggedContinuation, Vec<ListParWithRandom>)>, RSpaceError> {
        let result = self
            .space
            .consume(channels, patterns, TaggedContinuation::Empty, false, BTreeSet::new())
            .await?;
        Ok(result.map(|(cont, data)| {
            (
                cont.continuation,
                data.into_iter().map(|d| d.matched_datum).collect(),
            )
        }))
    }

    pub async fn get_hot_changes(
        &self,
    ) -> BTreeMap<Vec<SortedProc>, Row<BindPattern, ListParWithRandom, TaggedContinuation>> {
        self.space.to_map().await
    }
}

/// The replay runtime (port of `ReplayRhoRuntime`). Wraps a `ReplayRSpace` so that `inj`/`evaluate`
/// re-execute against the recorded COMM trace, and exposes `rig`/`check_replay_data` (Law 11).
pub struct ReplayRhoRuntime {
    reducer: Arc<RhoReducer>,
    space: RhoReplaySpace,
    cost: Arc<CostAccounting>,
    block_data: Arc<Mutex<BlockData>>,
    _history: RhoHistoryRepository,
}

impl ReplayRhoRuntime {
    pub async fn create(
        space: RhoReplaySpace,
        history: RhoHistoryRepository,
        mergeable_tag_name: SortedProc,
    ) -> std::io::Result<ReplayRhoRuntime> {
        let tuplespace: RhoTuplespace = space.clone();
        let native_store = space.native_store();
        let core = build_runtime_core(&tuplespace, mergeable_tag_name, native_store).await?;
        Ok(ReplayRhoRuntime {
            reducer: core.reducer,
            space,
            cost: core.cost,
            block_data: core.block_data,
            _history: history,
        })
    }

    /// Set the per-block data exposed to the `rho:block:data` contract (port of `setBlockData`).
    pub fn set_block_data(&self, block_data: BlockData) {
        *self.block_data.lock().unwrap_or_else(|p| p.into_inner()) = block_data;
    }

    /// Execute a `Closed` process in the given environment (port of `inj`). The `Closed` proof is
    /// discharged at this boundary; reduction then operates on the flat `Par` sub-terms.
    pub async fn inj(
        &self,
        par: &Closed,
        env: &Env<Par>,
        rand: &Blake2b512Random,
    ) -> Result<(), RholangError> {
        self.reducer
            .eval(&Par::from(par.clone()), env, rand, self.cost.as_ref())
            .await
    }

    /// Parse + run a rholang term (port of `evaluate`).
    pub async fn evaluate(
        &self,
        term: &str,
        rand: &Blake2b512Random,
    ) -> Result<EvaluateResult, RholangError> {
        self.evaluate_with_env(term, &BTreeMap::new(), rand).await
    }

    /// Parse + run a rholang term with an explicit normalizer environment (port of `evaluate`).
    pub async fn evaluate_with_env(
        &self,
        term: &str,
        env: &BTreeMap<String, Par>,
        rand: &Blake2b512Random,
    ) -> Result<EvaluateResult, RholangError> {
        let par = crate::normalizer::source_to_adt_with_env(term, env)?;
        let before = self.cost.total_charged();
        let errors = match self.inj(&par, &Env::new(), rand).await {
            Ok(()) => Vec::new(),
            Err(e) => vec![e],
        };
        let cost = self.cost.total_charged() - before;
        Ok(EvaluateResult {
            cost: crate::accounting::Cost::new(cost, "evaluate"),
            errors,
            mergeable: BTreeSet::new(),
        })
    }

    pub fn space(&self) -> &RhoReplaySpace {
        &self.space
    }

    /// The native system-contract store (shared with the wrapped play space).
    pub fn native_store(&self) -> Arc<InMemNativeStore> {
        self.space.native_store()
    }

    pub fn cost(&self) -> &CostAccounting {
        self.cost.as_ref()
    }

    pub async fn create_checkpoint(&self) -> Result<Checkpoint, String> {
        self.space.create_checkpoint().await
    }

    pub async fn reset(&self, root: Blake2b256Hash) -> Result<(), String> {
        self.space.reset(root).await
    }

    /// Capture a soft (in-memory) checkpoint for rollback (port of `createSoftCheckpoint`).
    pub async fn create_soft_checkpoint(
        &self,
    ) -> SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation> {
        self.space.create_soft_checkpoint().await
    }

    /// Roll back to a soft checkpoint (port of `revertToSoftCheckpoint`).
    pub async fn revert_to_soft_checkpoint(
        &self,
        checkpoint: SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>,
    ) {
        self.space.revert_to_soft_checkpoint(checkpoint).await
    }

    pub async fn get_data(
        &self,
        channel: &SortedProc,
    ) -> Result<Vec<Datum<ListParWithRandom>>, RSpaceError> {
        self.space.get_data(channel).await
    }

    /// Read all `Par`s at a channel (port of `getDataPar`).
    pub async fn get_data_par(&self, channel: &SortedProc) -> Result<Vec<Par>, RSpaceError> {
        let data = self.space.get_data(channel).await?;
        Ok(data.into_iter().flat_map(|d| d.a.pars.into_iter().map(|p| p.as_par().clone())).collect())
    }

    /// Consume the result at a channel with a pattern (port of `consumeResult`).
    pub async fn consume_result(
        &self,
        channels: &[SortedProc],
        patterns: &[BindPattern],
    ) -> Result<Option<(TaggedContinuation, Vec<ListParWithRandom>)>, RSpaceError> {
        let result = self
            .space
            .consume(channels, patterns, TaggedContinuation::Empty, false, BTreeSet::new())
            .await?;
        Ok(result.map(|(cont, data)| {
            (
                cont.continuation,
                data.into_iter().map(|d| d.matched_datum).collect(),
            )
        }))
    }

    /// Load the replay trace (port of `rig`).
    pub async fn rig(&self, log: Log) {
        self.space.rig(log).await;
    }

    /// Verify every recorded COMM was consumed by the replay (port of `checkReplayData`).
    pub async fn check_replay_data(&self) -> Result<(), ReplayException> {
        self.space.check_replay_data().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use rchain_rspace::tuple_space::{ContResult, Result as RSpaceResult};
    use std::sync::Mutex;

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
            self.produced.lock().unwrap_or_else(|p| p.into_inner()).push((channel, data, persist));
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
    async fn inj_send_produces_through_bridge() {
        let mock = Arc::new(MockSpace {
            produced: Mutex::new(Vec::new()),
        });
        let cost = Arc::new(CostAccounting::from_initial(crate::accounting::Costs::unsafe_max()));
        let charging = ChargingRSpace::new(mock.clone(), cost.clone());
        let reducer = setup_reducer(charging, cost.clone(), SortedProc::default());

        let send = rchain_models::ast::Send {
            chan: Box::new(rchain_models::par_ops::from_expr(rchain_models::ast::Expr::GInt(1)).quote()),
            data: vec![rchain_models::par_ops::from_expr(rchain_models::ast::Expr::GInt(2)).quote()],
            persistent: false,
            locally_free: rchain_models::ast::AlwaysEqual(vec![]),
            connective_used: false,
        };
        let par = Par {
            sends: vec![send],
            ..Default::default()
        };
        let rand = Blake2b512Random::new_random(128);
        reducer
            .eval(&par, &Env::new(), &rand, cost.as_ref())
            .await
            .unwrap();

        let produced = mock.produced.lock().unwrap_or_else(|p| p.into_inner());
        assert_eq!(produced.len(), 1);
        assert_eq!(produced[0].1.pars, vec![SortedProc::new(rchain_models::par_ops::from_expr(
            rchain_models::ast::Expr::GInt(2)
        ))]);
    }
}
