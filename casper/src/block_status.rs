//! Block validation status (port of `BlockStatus.scala`).

/// The outcome of validating a block (port of `BlockStatus`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BlockStatus {
    Valid,
    InvalidBlockNumber,
    InvalidRepeatDeploy,
    InvalidSequenceNumber,
    InvalidDeployShardId,
    JustificationRegression,
    NeglectedInvalidBlock,
    InvalidStateHash,
    InvalidBondsCache,
    InvalidRejectedDeploy,
    ContainsExpiredDeploy,
    ContainsFutureDeploy,
    ContainsLowCostDeploy,
}

impl BlockStatus {
    pub fn is_valid(&self) -> bool {
        matches!(self, BlockStatus::Valid)
    }
}
