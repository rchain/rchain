//! Replay orchestration over a `ReplayRhoRuntime` (port of
//! `casper/rholang/syntax/RuntimeReplaySyntax.scala`).
//!
//! Re-executes processed deploys and block-level system deploys against the recorded COMM trace,
//! verifying that the replayed status/cost match the play result and that every recorded COMM was
//! consumed (Law 11).

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use async_trait::async_trait;

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_crypto::public_key::PublicKey;
use rchain_models::ast::{Expr, Par, Var};
use rchain_models::casper::protocol::casper_message::{
    ProcessedDeploy, ProcessedSystemDeploy, SystemDeployData,
};
use rchain_models::par_ops::from_expr;
use rchain_models::types::count_free_vars;
use rchain_models::rholang::RhoType::RhoNumber;
use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_models::validator::Validator;
use rchain_shared::refined::NonNegI64;
use rchain_rholang::accounting::{Cost, CostAccounting};
use rchain_rholang::evaluate_result::EvaluateResult;
use rchain_rholang::errors::RholangError;
use rchain_rholang::native_state::NativeSystemState;
use rchain_rholang::reporting_runtime::ReportingRuntime;
use rchain_rholang::runtime::ReplayRhoRuntime;
use rchain_rholang::system_processes::BlockData;
use rchain_rspace::checkpoint::{Checkpoint, SoftCheckpoint};
use rchain_rspace::errors::RSpaceError;
use rchain_rspace::hashing::stable_hash_provider::hash_channel;
use rchain_rspace::internal::Datum;
use rchain_rspace::merger::event_log_index::NumberChannelsDiff;
use rchain_rspace::native_store::InMemNativeStore;
use rchain_rspace::trace::Log;
use rchain_rspace::util::ReplayException;

use crate::event_converter::to_rspace_event;
use crate::rholang::ReplayFailure;
use crate::system_deploy::{
    process_bool_result, NativeSystemDeployOp, SystemDeploy, SystemDeployUserError,
};

/// Random-seed split indices for the pre-charge / user-deploy / refund sequence (port of
/// `BlockRandomSeed`).
const PRE_CHARGE_SPLIT_INDEX: u8 = 0;
const USER_DEPLOY_SPLIT_INDEX: u8 = 1;
const REFUND_SPLIT_INDEX: u8 = 2;

/// The subset of a replay runtime needed to re-execute a block (implemented by both
/// [`ReplayRhoRuntime`] and [`ReportingRuntime`]).
#[async_trait]
pub trait ReplayRuntime {
    fn set_block_data(&self, block_data: BlockData);

    fn cost(&self) -> &CostAccounting;

    async fn reset(&self, root: Blake2b256Hash) -> Result<(), String>;

    async fn evaluate(&self, term: &str, rand: &Blake2b512Random) -> Result<EvaluateResult, RholangError>;

    async fn evaluate_with_env(
        &self,
        term: &str,
        env: &BTreeMap<String, Par>,
        rand: &Blake2b512Random,
    ) -> Result<EvaluateResult, RholangError>;

    async fn create_soft_checkpoint(
        &self,
    ) -> SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>;

    async fn revert_to_soft_checkpoint(
        &self,
        checkpoint: SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>,
    );

    async fn rig(&self, log: Log);

    async fn check_replay_data(&self) -> Result<(), ReplayException>;

    async fn get_data(&self, channel: &SortedProc) -> Result<Vec<Datum<ListParWithRandom>>, RSpaceError>;

    async fn consume_result(
        &self,
        channels: &[SortedProc],
        patterns: &[BindPattern],
    ) -> Result<Option<(TaggedContinuation, Vec<ListParWithRandom>)>, RSpaceError>;

    async fn create_checkpoint(&self) -> Result<Checkpoint, String>;

    fn native_store(&self) -> Arc<InMemNativeStore>;
}

/// Replay orchestration (port of `RuntimeReplayOps`).
pub struct RuntimeReplayOps<'a, R: ReplayRuntime + ?Sized> {
    runtime: &'a R,
}

impl<'a, R: ReplayRuntime + ?Sized> RuntimeReplayOps<'a, R> {
    pub fn new(runtime: &'a R) -> Self {
        RuntimeReplayOps { runtime }
    }

    /// Evaluate (and validate) deploys + system deploys, checkpointing to validate the final state
    /// hash (port of `replayComputeState`).
    pub async fn replay_compute_state(
        &self,
        start_hash: &Blake2b256Hash,
        rand: &Blake2b512Random,
        terms: &[ProcessedDeploy],
        system_deploys: &[ProcessedSystemDeploy],
        block_data: BlockData,
        with_cost_accounting: bool,
        bonds: &BTreeMap<Validator, NonNegI64>,
    ) -> Result<(Blake2b256Hash, Vec<NumberChannelsDiff>), ReplayFailure> {
        self.runtime.set_block_data(block_data);
        self.replay_deploys(start_hash, rand, terms, system_deploys, with_cost_accounting, bonds)
            .await
    }

    /// Reset to `start_hash`, replay each deploy then each system deploy, and checkpoint (port of
    /// `replayDeploys`).
    async fn replay_deploys(
        &self,
        start_hash: &Blake2b256Hash,
        rand: &Blake2b512Random,
        terms: &[ProcessedDeploy],
        system_deploys: &[ProcessedSystemDeploy],
        with_cost_accounting: bool,
        bonds: &BTreeMap<Validator, NonNegI64>,
    ) -> Result<(Blake2b256Hash, Vec<NumberChannelsDiff>), ReplayFailure> {
        self.runtime
            .reset(*start_hash)
            .await
            .map_err(ReplayFailure::internal_error)?;
        // Genesis replay (no cost accounting): re-install the native bonds so the replayed post-state
        // hash matches the play genesis hash.
        if !with_cost_accounting {
            NativeSystemState::new(self.runtime.native_store()).set_bonds(bonds);
        }

        let mut mergeable: Vec<NumberChannelsDiff> = Vec::new();
        for (i, term) in terms.iter().enumerate() {
            mergeable.push(
                self.replay_deploy_e(
                    term,
                    rand.split_byte(
                        u8::try_from(i)
                            .map_err(|_| ReplayFailure::internal_error("deploy count exceeds 255".to_string()))?,
                    ),
                    with_cost_accounting,
                )
                    .await?,
            );
        }
        for (i, sd) in system_deploys.iter().enumerate() {
            mergeable.push(
                self.replay_block_system_deploy(
                    sd,
                    rand.split_byte(
                        u8::try_from(terms.len() + i)
                            .map_err(|_| ReplayFailure::internal_error("deploy count exceeds 255".to_string()))?,
                    ),
                )
                    .await?,
            );
        }

        let checkpoint = self
            .runtime
            .create_checkpoint()
            .await
            .map_err(ReplayFailure::internal_error)?;
        Ok((checkpoint.root, mergeable))
    }

    /// Replay a single user deploy (port of `replayDeployE`).
    pub(crate) async fn replay_deploy_e(
        &self,
        processed_deploy: &ProcessedDeploy,
        rand: Blake2b512Random,
        with_cost_accounting: bool,
    ) -> Result<NumberChannelsDiff, ReplayFailure> {
        let mut mergeable: BTreeSet<Par> = BTreeSet::new();
        let expected_failure = processed_deploy.system_deploy_error.clone();

        // Load the deploy's recorded trace before re-executing (port of `rigWithCheck`).
        self.rig_deploy(processed_deploy).await;

        let succeeded = self
            .replay_deploy_evaluator(
                processed_deploy,
                rand,
                with_cost_accounting,
                expected_failure.as_deref(),
                &mut mergeable,
            )
            .await?;

        self.check_replay_data_with_fix(succeeded).await?;

        self.get_number_channels_data(&mergeable).await
    }

    /// The pre-charge / user-deploy / refund fold yielding whether the deploy succeeded (port of
    /// `replayDeployE`'s `evaluatorT`).
    #[allow(clippy::too_many_arguments)]
    async fn replay_deploy_evaluator(
        &self,
        processed_deploy: &ProcessedDeploy,
        rand: Blake2b512Random,
        with_cost_accounting: bool,
        expected_failure: Option<&str>,
        mergeable: &mut BTreeSet<Par>,
    ) -> Result<bool, ReplayFailure> {
        if !with_cost_accounting {
            let eval_result = self
                .replay_deploy_eval(processed_deploy, rand.split_byte(USER_DEPLOY_SPLIT_INDEX))
                .await?;
            if eval_result.succeeded() {
                mergeable.extend(eval_result.mergeable.iter().cloned());
            }
            return Ok(eval_result.succeeded());
        }

        // Pre-charge.
        let pre_charge = SystemDeploy::pre_charge(
            processed_deploy
                .deploy
                .data
                .total_phlo_charge()
                .ok_or_else(|| ReplayFailure::internal_error("phlo charge overflow"))?,
            &PublicKey::new(processed_deploy.deploy.deployer.clone()),
            rand.split_byte(PRE_CHARGE_SPLIT_INDEX),
        );
        let (_pre_result, pre_eval) = self
            .replay_system_deploy_internal(&pre_charge, expected_failure)
            .await?;
        self.runtime.create_soft_checkpoint().await;
        if pre_eval.succeeded() {
            mergeable.extend(pre_eval.mergeable.iter().cloned());
        }

        if expected_failure.is_some() {
            return Ok(true);
        }

        // User deploy (reverted on failure).
        let eval_result = self
            .replay_deploy_eval(processed_deploy, rand.split_byte(USER_DEPLOY_SPLIT_INDEX))
            .await?;
        if eval_result.succeeded() {
            mergeable.extend(eval_result.mergeable.iter().cloned());
            self.runtime.create_soft_checkpoint().await;
        }

        // Refund.
        let refund = SystemDeploy::refund(
            refund_amount(processed_deploy),
            rand.split_byte(REFUND_SPLIT_INDEX),
        );
        let (_refund_result, refund_eval) = self.replay_system_deploy_internal(&refund, None).await?;
        self.runtime.create_soft_checkpoint().await;
        if refund_eval.succeeded() {
            mergeable.extend(refund_eval.mergeable.iter().cloned());
        }

        Ok(eval_result.succeeded())
    }

    /// Replay the user deploy body and verify status/cost match the play result (port of
    /// `deployEvaluator`).
    async fn replay_deploy_eval(
        &self,
        processed_deploy: &ProcessedDeploy,
        rand: Blake2b512Random,
    ) -> Result<EvaluateResult, ReplayFailure> {
        // Soft transaction: revert the deploy's effects if it failed.
        let fallback = self.runtime.create_soft_checkpoint().await;
        // Enforce the same per-deploy phlo budget as the play path (see `process_deploy`), so a
        // replayed deploy that exceeds its recorded cost can't spuriously OOG against the shared
        // replay pool and fail the `recorded_cost != result.cost.value` check below.
        self.runtime.cost().set(Cost::new(
            processed_deploy.deploy.data.phlo_limit,
            "deploy-replay",
        ));
        let result = self
            .runtime
            .evaluate(&processed_deploy.deploy.data.term, &rand)
            .await
            .map_err(|e| ReplayFailure::internal_error(e.to_string()))?;
        if result.failed() {
            self.runtime.revert_to_soft_checkpoint(fallback).await;
        }

        // Verify deploy status matches.
        if processed_deploy.is_failed != result.failed() {
            return Err(ReplayFailure::replay_status_mismatch(
                processed_deploy.is_failed,
                result.failed(),
            ));
        }
        // Verify evaluation cost matches.
        let recorded_cost = i64::try_from(processed_deploy.cost.cost).map_err(|_| {
            ReplayFailure::replay_cost_mismatch(i64::MAX, result.cost.value)
        })?;
        if recorded_cost != result.cost.value {
            return Err(ReplayFailure::replay_cost_mismatch(recorded_cost, result.cost.value));
        }
        Ok(result)
    }

    /// Replay a block-level system deploy (port of `replayBlockSystemDeploy`).
    pub(crate) async fn replay_block_system_deploy(
        &self,
        processed: &ProcessedSystemDeploy,
        rand: Blake2b512Random,
    ) -> Result<NumberChannelsDiff, ReplayFailure> {
        let system_deploy_data = match processed {
            ProcessedSystemDeploy::Succeeded { system_deploy, .. } => system_deploy,
            ProcessedSystemDeploy::Failed { .. } => {
                return Err(ReplayFailure::internal_error("Expected system deploy"));
            }
        };
        let deploy = match system_deploy_data {
            SystemDeployData::Slash(validator) => SystemDeploy::slash(validator, rand),
            SystemDeployData::CloseBlock => SystemDeploy::close_block(rand),
            SystemDeployData::Empty => {
                return Err(ReplayFailure::internal_error("Expected system deploy"));
            }
        };

        self.rig_system_deploy(processed).await;

        let (_result, eval_res) = self.replay_system_deploy_internal(&deploy, None).await?;
        if eval_res.succeeded() {
            self.runtime.create_soft_checkpoint().await;
        }
        let data = self.get_number_channels_data(&eval_res.mergeable).await?;

        self.check_replay_data_with_fix(eval_res.succeeded()).await?;

        Ok(data)
    }

    /// Evaluate a system deploy and compare its play/replay status (port of
    /// `replaySystemDeployInternal`).
    async fn replay_system_deploy_internal(
        &self,
        system_deploy: &SystemDeploy,
        expected_failure_msg: Option<&str>,
    ) -> Result<(Result<(), SystemDeployUserError>, EvaluateResult), ReplayFailure> {
        let (result, eval_res) = self
            .eval_system_deploy(system_deploy)
            .await
            .map_err(ReplayFailure::internal_error)?;

        match (expected_failure_msg, &result) {
            // Replayed successful execution.
            (None, Ok(())) => {}
            // Replayed failed execution with a matching error.
            (Some(expected), Err(SystemDeployUserError(actual))) if expected == actual.as_str() => {}
            // Error messages differ.
            (Some(expected), Err(SystemDeployUserError(actual))) => {
                return Err(ReplayFailure::system_deploy_error_mismatch(
                    expected.to_string(),
                    actual.clone(),
                ));
            }
            // Error expected, replay successful.
            (Some(_), Ok(())) => {
                return Err(ReplayFailure::replay_status_mismatch(true, false));
            }
            // No error expected, replay failed.
            (None, Err(_)) => {
                return Err(ReplayFailure::replay_status_mismatch(false, true));
            }
        }
        Ok((result, eval_res))
    }

    /// Evaluate a system deploy on the replay runtime (port of `evalSystemDeploy`).
    async fn eval_system_deploy(
        &self,
        deploy: &SystemDeploy,
    ) -> Result<(Result<(), SystemDeployUserError>, EvaluateResult), String> {
        if let Some(op) = &deploy.op {
            return self.eval_native_system_deploy(op).await;
        }
        let eval_result = self
            .runtime
            .evaluate_with_env(deploy.source, &deploy.normalizer_env, &deploy.rand)
            .await
            .map_err(|e| e.to_string())?;
        if !eval_result.errors.is_empty() {
            return Err(format!("Unexpected system errors: {:?}", eval_result.errors));
        }
        let consumed = self.consume_system_result(deploy).await?;
        match consumed {
            Some((_, data)) => match data.as_slice() {
                [single] => match single.pars.as_slice() {
                    [p] => {
                        let result = process_bool_result(p.as_par());
                        Ok((result, eval_result))
                    }
                    _ => Err("Unexpected system-deploy result".to_string()),
                },
                _ => Err("Unexpected system-deploy result".to_string()),
            },
            None => Err("Unable to consume results of system deploy".to_string()),
        }
    }

    /// Evaluate a native system deploy during replay (deterministic: pure in the deploy + state).
    async fn eval_native_system_deploy(
        &self,
        op: &NativeSystemDeployOp,
    ) -> Result<(Result<(), SystemDeployUserError>, EvaluateResult), String> {
        let native = NativeSystemState::new(self.runtime.native_store());
        let result = match op {
            NativeSystemDeployOp::PreCharge { deployer, amount } => {
                native.pre_charge(deployer, *amount).await?
            }
            NativeSystemDeployOp::Refund { amount } => native.refund(*amount).await?,
            NativeSystemDeployOp::CloseBlock => native.close_block().await?,
            NativeSystemDeployOp::Slash { validator } => native.slash(validator).await?,
        };
        let eval_result = EvaluateResult {
            cost: Cost::new(0, "native-system-deploy"),
            errors: Vec::new(),
            mergeable: BTreeSet::new(),
        };
        Ok((result.map_err(SystemDeployUserError), eval_result))
    }

    async fn consume_system_result(
        &self,
        deploy: &SystemDeploy,
    ) -> Result<Option<(TaggedContinuation, Vec<ListParWithRandom>)>, String> {
        let patterns = vec![SortedProc::new(from_expr(Expr::EVar(Box::new(Var::FreeVar(0)))))];
        let pattern = BindPattern {
            free_count: count_free_vars(patterns[0].as_par()),
            patterns,
            remainder: None,
        };
        self.runtime
            .consume_result(&[SortedProc::new(deploy.return_channel.clone())], &[pattern])
            .await
            .map_err(|e| e.to_string())
    }

    /// Load the recorded trace of a processed deploy (port of `rig(ProcessedDeploy)`).
    async fn rig_deploy(&self, processed_deploy: &ProcessedDeploy) {
        let log = processed_deploy
            .deploy_log
            .iter()
            .map(to_rspace_event)
            .collect();
        self.runtime.rig(log).await;
    }

    /// Load the recorded trace of a processed system deploy (port of `rig(ProcessedSystemDeploy)`).
    async fn rig_system_deploy(&self, processed: &ProcessedSystemDeploy) {
        let event_list = match processed {
            ProcessedSystemDeploy::Succeeded { event_list, .. } => event_list,
            ProcessedSystemDeploy::Failed { event_list, .. } => event_list,
        };
        let log = event_list.iter().map(to_rspace_event).collect();
        self.runtime.rig(log).await;
    }

    /// Verify the replay trace, ignoring unused-COMM failures for failed deploys (port of
    /// `checkReplayDataWithFix`).
    async fn check_replay_data_with_fix(&self, eval_successful: bool) -> Result<(), ReplayFailure> {
        match self.runtime.check_replay_data().await {
            Ok(()) => Ok(()),
            Err(replay_exception) => {
                let failure = ReplayFailure::unused_comm_event(replay_exception.0);
                // TODO: temp fix for replay error mismatch (RCHAIN-3505).
                if eval_successful {
                    Err(failure)
                } else {
                    Ok(())
                }
            }
        }
    }

    /// Read the numeric values of the mergeable (number) channels (port of
    /// `getNumberChannelsData`).
    async fn get_number_channels_data(
        &self,
        channels: &BTreeSet<Par>,
    ) -> Result<NumberChannelsDiff, ReplayFailure> {
        let mut out = BTreeMap::new();
        for chan in channels {
            if let Some((hash, num)) = self.get_number_channel(chan).await? {
                out.insert(hash, num);
            }
        }
        Ok(out)
    }

    /// Read a single mergeable channel's number value (port of `getNumberChannel`).
    async fn get_number_channel(
        &self,
        chan: &Par,
    ) -> Result<Option<(Blake2b256Hash, i64)>, ReplayFailure> {
        let data = self
            .runtime
            .get_data(&SortedProc::new(chan.clone()))
            .await
            .map_err(|e| ReplayFailure::internal_error(e.to_string()))?;
        if data.is_empty() {
            return Ok(None);
        }
        if data.len() != 1 {
            return Err(ReplayFailure::internal_error(
                "Number channel must have singleton value.",
            ));
        }
        let datum = data
            .first()
            .ok_or_else(|| ReplayFailure::internal_error("Number channel must have singleton value."))?;
        let num = get_number_with_rnd(&datum.a).map_err(ReplayFailure::internal_error)?;
        let ch_hash = hash_channel(chan);
        Ok(Some((ch_hash, num)))
    }
}

#[async_trait]
impl ReplayRuntime for ReplayRhoRuntime {
    fn set_block_data(&self, block_data: BlockData) {
        ReplayRhoRuntime::set_block_data(self, block_data);
    }

    fn cost(&self) -> &CostAccounting {
        ReplayRhoRuntime::cost(self)
    }

    async fn reset(&self, root: Blake2b256Hash) -> Result<(), String> {
        ReplayRhoRuntime::reset(self, root).await
    }

    async fn evaluate(&self, term: &str, rand: &Blake2b512Random) -> Result<EvaluateResult, RholangError> {
        ReplayRhoRuntime::evaluate(self, term, rand).await
    }

    async fn evaluate_with_env(
        &self,
        term: &str,
        env: &BTreeMap<String, Par>,
        rand: &Blake2b512Random,
    ) -> Result<EvaluateResult, RholangError> {
        ReplayRhoRuntime::evaluate_with_env(self, term, env, rand).await
    }

    async fn create_soft_checkpoint(
        &self,
    ) -> SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation> {
        ReplayRhoRuntime::create_soft_checkpoint(self).await
    }

    async fn revert_to_soft_checkpoint(
        &self,
        checkpoint: SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>,
    ) {
        ReplayRhoRuntime::revert_to_soft_checkpoint(self, checkpoint).await
    }

    async fn rig(&self, log: Log) {
        ReplayRhoRuntime::rig(self, log).await
    }

    async fn check_replay_data(&self) -> Result<(), ReplayException> {
        ReplayRhoRuntime::check_replay_data(self).await
    }

    async fn get_data(&self, channel: &SortedProc) -> Result<Vec<Datum<ListParWithRandom>>, RSpaceError> {
        ReplayRhoRuntime::get_data(self, channel).await
    }

    async fn consume_result(
        &self,
        channels: &[SortedProc],
        patterns: &[BindPattern],
    ) -> Result<Option<(TaggedContinuation, Vec<ListParWithRandom>)>, RSpaceError> {
        ReplayRhoRuntime::consume_result(self, channels, patterns).await
    }

    async fn create_checkpoint(&self) -> Result<Checkpoint, String> {
        ReplayRhoRuntime::create_checkpoint(self).await
    }

    fn native_store(&self) -> Arc<InMemNativeStore> {
        ReplayRhoRuntime::native_store(self)
    }
}

#[async_trait]
impl ReplayRuntime for ReportingRuntime {
    fn set_block_data(&self, block_data: BlockData) {
        ReportingRuntime::set_block_data(self, block_data);
    }

    fn cost(&self) -> &CostAccounting {
        ReportingRuntime::cost(self)
    }

    async fn reset(&self, root: Blake2b256Hash) -> Result<(), String> {
        ReportingRuntime::reset(self, root).await
    }

    async fn evaluate(&self, term: &str, rand: &Blake2b512Random) -> Result<EvaluateResult, RholangError> {
        ReportingRuntime::evaluate(self, term, rand).await
    }

    async fn evaluate_with_env(
        &self,
        term: &str,
        env: &BTreeMap<String, Par>,
        rand: &Blake2b512Random,
    ) -> Result<EvaluateResult, RholangError> {
        ReportingRuntime::evaluate_with_env(self, term, env, rand).await
    }

    async fn create_soft_checkpoint(
        &self,
    ) -> SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation> {
        ReportingRuntime::create_soft_checkpoint(self).await
    }

    async fn revert_to_soft_checkpoint(
        &self,
        checkpoint: SoftCheckpoint<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>,
    ) {
        ReportingRuntime::revert_to_soft_checkpoint(self, checkpoint).await
    }

    async fn rig(&self, log: Log) {
        ReportingRuntime::rig(self, log).await
    }

    async fn check_replay_data(&self) -> Result<(), ReplayException> {
        ReportingRuntime::check_replay_data(self).await
    }

    async fn get_data(&self, channel: &SortedProc) -> Result<Vec<Datum<ListParWithRandom>>, RSpaceError> {
        ReportingRuntime::get_data(self, channel).await
    }

    async fn consume_result(
        &self,
        channels: &[SortedProc],
        patterns: &[BindPattern],
    ) -> Result<Option<(TaggedContinuation, Vec<ListParWithRandom>)>, RSpaceError> {
        ReportingRuntime::consume_result(self, channels, patterns).await
    }

    async fn create_checkpoint(&self) -> Result<Checkpoint, String> {
        ReportingRuntime::create_checkpoint(self).await
    }

    fn native_store(&self) -> Arc<InMemNativeStore> {
        ReportingRuntime::native_store(self)
    }
}

/// Extract the numeric value from a number-channel datum (port of `getNumberWithRnd`).
fn get_number_with_rnd(par_with_rnd: &ListParWithRandom) -> Result<i64, String> {
    match par_with_rnd.pars.as_slice() {
        [p] => RhoNumber::unapply(p.as_par())
            .ok_or_else(|| "Number channel should contain single Int term.".to_string()),
        _ => Err(format!(
            "Number channel should contain single Int term, found {} pars.",
            par_with_rnd.pars.len()
        )),
    }
}

/// The phlo refunded after a deploy (port of `ProcessedDeploy.refundAmount`).
fn refund_amount(processed_deploy: &ProcessedDeploy) -> i64 {
    // Compute in i128 and clamp to i64::MAX: the refund is non-negative by construction
    // (`max(0)`), so any overflow would otherwise wrap and corrupt cost accounting.
    let remaining = processed_deploy
        .deploy
        .data
        .phlo_limit
        .saturating_sub_unsigned(processed_deploy.cost.cost)
        .max(0) as i128;
    let product = remaining * (processed_deploy.deploy.data.phlo_price as i128);
    i64::try_from(product).unwrap_or(i64::MAX)
}
