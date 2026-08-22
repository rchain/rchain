//! Proposal result types (port of `blocks/proposer/ProposeResult.scala`).

use std::fmt;

use rchain_models::casper::protocol::casper_message::BlockMessage;

/// The outcome of a proposal (port of `ProposeStatus`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProposeStatus {
    ProposeSuccess,
    InternalDeployError,
    BugError,
    NotBonded,
    NotEnoughNewBlocks,
    TooFarAheadOfLastFinalized,
    NoNewDeploys,
}

impl fmt::Display for ProposeStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            ProposeStatus::ProposeSuccess => "Propose succeed: Valid",
            ProposeStatus::NoNewDeploys => "Proposal failed: NoNewDeploys",
            ProposeStatus::InternalDeployError => "Proposal failed: internal deploy error",
            ProposeStatus::NotBonded => "Proposal failed: ReadOnlyMode",
            ProposeStatus::NotEnoughNewBlocks => {
                "Proposal failed: Must wait for more blocks from other validators"
            }
            ProposeStatus::TooFarAheadOfLastFinalized => {
                "Proposal failed: too far ahead of the last finalized block"
            }
            ProposeStatus::BugError => "Proposal failed: BugError",
        };
        write!(f, "{s}")
    }
}

/// A proposal result (port of `ProposeResult`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProposeResult {
    pub propose_status: ProposeStatus,
}

/// The result of checking propose constraints (port of `CheckProposeConstraintsResult`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CheckProposeConstraintsResult {
    Success,
    NotBonded,
    NotEnoughNewBlocks,
    TooFarAheadOfLastFinalized,
}

/// The result of creating a block (port of `BlockCreatorResult`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlockCreatorResult {
    NoNewDeploys,
    Created(BlockMessage),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn propose_status_displays() {
        assert_eq!(ProposeStatus::NoNewDeploys.to_string(), "Proposal failed: NoNewDeploys");
        assert_eq!(ProposeStatus::ProposeSuccess.to_string(), "Propose succeed: Valid");
    }
}
