//! The reporting replay runtime (port of `ReportingRuntime` in `ReportingCasper.scala`).
//!
//! A `ReplayRhoRuntime` analogue whose space is a [`ReportingRspace`], so produce/consume/COMM
//! events are recorded during replay. Exposes `get_report` to drain the recorded report.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, Mutex};

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::ast::Par;
use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_models::types::Closed;
use rchain_shared::store_manager::KeyValueStoreManager;
use rchain_rspace::checkpoint::{Checkpoint, SoftCheckpoint};
use rchain_rspace::errors::RSpaceError;
use rchain_rspace::i_replay_space::IReplaySpace;
use rchain_rspace::i_space::ISpace;
use rchain_rspace::internal::Datum;
use rchain_rspace::native_store::InMemNativeStore;
use rchain_rspace::reporting_rspace::{ReportingEvent, ReportingRspace};
use rchain_rspace::trace::Log;
use rchain_rspace::tuple_space::Tuplespace;
use rchain_rspace::util::ReplayException;

use crate::accounting::CostAccounting;
use crate::env::Env;
use crate::errors::RholangError;
use crate::evaluate_result::EvaluateResult;
use crate::runtime::{build_runtime_core, RhoReducer};
use crate::storage::RhoTuplespace;
use crate::system_processes::BlockData;

/// The concrete rholang reporting space (port of `RhoReportingRspace`).
pub type RhoReportingRspace =
    ReportingRspace<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>;

/// A single recorded reporting event (port of `RhoReportingEvent`).
pub type RhoReportingEvent =
    ReportingEvent<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>;

/// Build a reporting space from the store manager + the rholang matcher (port of
/// `ReportingRuntime.createReportingRSpace`).
pub async fn create_reporting_rspace(
    manager: &dyn KeyValueStoreManager,
) -> Result<Arc<RhoReportingRspace>, String> {
    rchain_rspace::factory::create_reporting_rspace(manager, Arc::new(crate::storage::RhoMatch))
        .await
        .map(Arc::new)
}

/// The reporting runtime (port of `ReportingRuntime`).
pub struct ReportingRuntime {
    reducer: Arc<RhoReducer>,
    space: Arc<RhoReportingRspace>,
    cost: Arc<CostAccounting>,
    block_data: Arc<Mutex<BlockData>>,
}

impl ReportingRuntime {
    pub async fn create(
        space: Arc<RhoReportingRspace>,
        mergeable_tag_name: SortedProc,
    ) -> std::io::Result<ReportingRuntime> {
        let tuplespace: RhoTuplespace = space.clone();
        let native_store = space.native_store();
        let core = build_runtime_core(&tuplespace, mergeable_tag_name, native_store).await?;
        Ok(ReportingRuntime {
            reducer: core.reducer,
            space,
            cost: core.cost,
            block_data: core.block_data,
        })
    }

    /// Drain the recorded report (port of `getReport`).
    pub fn get_report(&self) -> Vec<Vec<RhoReportingEvent>> {
        self.space.get_report()
    }

    /// The cost-accounting cell (exposed so replay can seed the per-deploy phlo budget).
    pub fn cost(&self) -> &CostAccounting {
        self.cost.as_ref()
    }

    /// The native system-contract store (shared with the wrapped reporting/replay/play spaces).
    pub fn native_store(&self) -> Arc<InMemNativeStore> {
        self.space.native_store()
    }

    pub fn set_block_data(&self, block_data: BlockData) {
        *self.block_data.lock().unwrap_or_else(|p| p.into_inner()) = block_data;
    }

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

    pub async fn evaluate(
        &self,
        term: &str,
        rand: &Blake2b512Random,
    ) -> Result<EvaluateResult, RholangError> {
        self.evaluate_with_env(term, &BTreeMap::new(), rand).await
    }

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

    pub async fn create_checkpoint(&self) -> Result<Checkpoint, String> {
        self.space.create_checkpoint().await
    }

    pub async fn reset(&self, root: Blake2b256Hash) -> Result<(), String> {
        self.space.reset(root).await
    }

    pub async fn create_soft_checkpoint(
        &self,
    ) -> SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation> {
        self.space.create_soft_checkpoint().await
    }

    pub async fn revert_to_soft_checkpoint(
        &self,
        checkpoint: SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>,
    ) {
        self.space.revert_to_soft_checkpoint(checkpoint).await;
    }

    pub async fn rig(&self, log: Log) {
        self.space.rig(log).await;
    }

    pub async fn check_replay_data(&self) -> Result<(), ReplayException> {
        self.space.check_replay_data().await
    }

    pub async fn get_data(
        &self,
        channel: &SortedProc,
    ) -> Result<Vec<Datum<ListParWithRandom>>, RSpaceError> {
        self.space.get_data(channel).await
    }

    pub async fn consume_result(
        &self,
        channels: &[SortedProc],
        patterns: &[BindPattern],
    ) -> Result<Option<(TaggedContinuation, Vec<ListParWithRandom>)>, RSpaceError> {
        let result = self
            .space
            .consume(
                channels,
                patterns,
                TaggedContinuation::Empty,
                false,
                BTreeSet::new(),
            )
            .await?;
        Ok(result.map(|(cont, data)| {
            (
                cont.continuation,
                data.into_iter().map(|d| d.matched_datum).collect(),
            )
        }))
    }
}
