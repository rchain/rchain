//! Non-blocking CSPRNG.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/util/SecureRandomUtil.scala`. The Scala tries
//! `NativePRNGNonBlocking` / `Windows-PRNG` / `SHA1PRNG`; the Rust port uses the OS CSPRNG directly.

pub use rand::rngs::OsRng;
