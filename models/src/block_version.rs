//! Block version constants.
//!
//! Mirrors `models/src/main/scala/coop/rchain/models/BlockVersion.scala`.

/// The current block version.
pub const CURRENT: i32 = 1;

/// All supported block versions.
pub const SUPPORTED: [i32; 1] = [CURRENT];
