//! TLS configuration.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/TlsConf.scala`.

use std::path::PathBuf;

/// The TLS configuration (port of `TlsConf`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TlsConf {
    pub certificate_path: PathBuf,
    pub key_path: PathBuf,
    pub secure_random_non_blocking: bool,
    pub custom_certificate_location: bool,
    pub custom_key_location: bool,
}
