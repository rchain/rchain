//! RSpace errors.
//!
//! Hard error type for the rspace crate's declared partiality boundaries (lock poisoning, codec
//! decode, history sum-type invariants, storage commit, replay invariants). Mirrors the typed-fix
//! column of `spec/TYPE-SYSTEM.md` §3.2.

use std::fmt;

/// An rspace error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RSpaceError {
    /// A lock was poisoned (a panic occurred while it was held).
    LockPoisoned,
    /// A codec decode failed at the serialization boundary.
    Codec(&'static str),
    /// A history leaf had an unexpected variant for the requested key.
    UnexpectedLeaf(&'static str),
    /// A history commit failed.
    HistoryCommitFailed,
    /// `install` was attempted outside of startup.
    InstallNotAllowed,
    /// A cached key was unexpectedly missing.
    CachedKeyMissing,
    /// A radix action key had an empty prefix.
    EmptyPrefix,
    /// Replay data was expected to be empty at checkpoint.
    ReplayDataNotEmpty,
    /// A recomputed COMM event was not present in the recorded replay trace (a peer-supplied
    /// event log is self-inconsistent — the block is invalid, not a reason to panic).
    ReplayCommNotInTrace,
}

impl fmt::Display for RSpaceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RSpaceError::LockPoisoned => write!(f, "lock poisoned"),
            RSpaceError::Codec(what) => write!(f, "decode {what} failed"),
            RSpaceError::UnexpectedLeaf(what) => write!(f, "unexpected leaf while looking for {what}"),
            RSpaceError::HistoryCommitFailed => write!(f, "history commit failed"),
            RSpaceError::InstallNotAllowed => write!(f, "installing can be done only on startup"),
            RSpaceError::CachedKeyMissing => write!(f, "cached key must be present"),
            RSpaceError::EmptyPrefix => write!(f, "prefix must be non-empty"),
            RSpaceError::ReplayDataNotEmpty => write!(f, "replay data must be empty at checkpoint"),
            RSpaceError::ReplayCommNotInTrace => write!(f, "COMM event was not contained in the trace"),
        }
    }
}

impl std::error::Error for RSpaceError {}

/// Convenience result alias.
pub type Result<A> = std::result::Result<A, RSpaceError>;
