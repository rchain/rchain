//! Runtime-manager types (port of `casper/rholang/types/ReplayFailure.scala`,
//! `casper/rholang/RuntimeDeployResult.scala`, and `casper/BlockExecutionTracker.scala`).

use rchain_models::casper::protocol::casper_message::{ProcessedDeploy, ProcessedSystemDeploy};
use rchain_rholang::evaluate_result::EvaluateResult;
use rchain_rspace::merger::event_log_index::NumberChannelsDiff;

/// A processed user deploy plus its mergeable channels and evaluation result (port of
/// `RuntimeDeployResult.UserDeployRuntimeResult`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UserDeployRuntimeResult {
    pub deploy: ProcessedDeploy,
    pub mergeable: NumberChannelsDiff,
    pub eval_result: EvaluateResult,
}

/// A processed system deploy plus its mergeable channels (port of
/// `RuntimeDeployResult.SystemDeployRuntimeResult`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SystemDeployRuntimeResult {
    pub deploy: ProcessedSystemDeploy,
    pub mergeable: NumberChannelsDiff,
}

/// Tracks deploy execution start/completion (port of `BlockExecutionTracker`).
pub trait BlockExecutionTracker {
    fn exec_started(&self, deploy_id: &[u8]);
    fn exec_complete(&self, deploy_id: &[u8], res: &EvaluateResult);
}

/// A no-op tracker (port of `RuntimeManager.noOpExecutionTracker`).
pub struct NoOpExecutionTracker;

impl BlockExecutionTracker for NoOpExecutionTracker {
    fn exec_started(&self, _deploy_id: &[u8]) {}
    fn exec_complete(&self, _deploy_id: &[u8], _res: &EvaluateResult) {}
}

/// A replay verification failure (port of `ReplayFailure`). `ReplayException` is carried as its
/// message `String`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReplayFailure {
    InternalError(String),
    ReplayStatusMismatch {
        initial_failed: bool,
        replay_failed: bool,
    },
    UnusedCommEvent(String),
    ReplayCostMismatch {
        initial_cost: i64,
        replay_cost: i64,
    },
    SystemDeployErrorMismatch {
        play_error: String,
        replay_error: String,
    },
}

impl ReplayFailure {
    pub fn internal_error(msg: impl Into<String>) -> ReplayFailure {
        ReplayFailure::InternalError(msg.into())
    }
    pub fn replay_status_mismatch(initial_failed: bool, replay_failed: bool) -> ReplayFailure {
        ReplayFailure::ReplayStatusMismatch {
            initial_failed,
            replay_failed,
        }
    }
    pub fn unused_comm_event(msg: impl Into<String>) -> ReplayFailure {
        ReplayFailure::UnusedCommEvent(msg.into())
    }
    pub fn replay_cost_mismatch(initial_cost: i64, replay_cost: i64) -> ReplayFailure {
        ReplayFailure::ReplayCostMismatch {
            initial_cost,
            replay_cost,
        }
    }
    pub fn system_deploy_error_mismatch(
        play_error: impl Into<String>,
        replay_error: impl Into<String>,
    ) -> ReplayFailure {
        ReplayFailure::SystemDeployErrorMismatch {
            play_error: play_error.into(),
            replay_error: replay_error.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constructors_build_variants() {
        assert_eq!(
            ReplayFailure::replay_status_mismatch(true, false),
            ReplayFailure::ReplayStatusMismatch {
                initial_failed: true,
                replay_failed: false
            }
        );
        assert_eq!(
            ReplayFailure::replay_cost_mismatch(1, 2),
            ReplayFailure::ReplayCostMismatch {
                initial_cost: 1,
                replay_cost: 2
            }
        );
        assert_eq!(
            ReplayFailure::unused_comm_event("boom"),
            ReplayFailure::UnusedCommEvent("boom".to_string())
        );
    }
}
