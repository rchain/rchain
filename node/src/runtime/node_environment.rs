//! Node environment initialization (port of `node/runtime/NodeEnvironment.scala`).

use std::path::Path;

use rchain_comm::peer_node::NodeIdentifier;
use rchain_comm::transport::generate_certificate_if_absent;
use rchain_comm::transport::tls_conf::TlsConf;

use crate::configuration::model::NodeConf;

/// A node-environment initialization error (port of `NodeEnvironment.InitializationException`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InitializationException(pub String);

impl std::fmt::Display for InitializationException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for InitializationException {}

fn data_dir_error(data_dir: &Path) -> String {
    format!(
        "The data dir must be a directory and have read and write permissions:\n{}",
        data_dir.display()
    )
}

/// Create the data dir if absent (port of `canCreateDataDir`).
pub fn can_create_data_dir(data_dir: &Path) -> Result<(), InitializationException> {
    if !data_dir.exists() {
        std::fs::create_dir(data_dir)
            .map_err(|_| InitializationException(data_dir_error(data_dir)))?;
    }
    Ok(())
}

/// Check the data dir is an accessible directory (port of `haveAccessToDataDir`).
pub fn have_access_to_data_dir(data_dir: &Path) -> Result<(), InitializationException> {
    if !data_dir.is_dir() {
        return Err(InitializationException(data_dir_error(data_dir)));
    }
    Ok(())
}

/// Check the TLS certificate file exists (port of `hasCertificate`).
pub fn has_certificate(tls: &TlsConf) -> Result<(), InitializationException> {
    if !tls.certificate_path.exists() {
        return Err(InitializationException(format!(
            "Certificate file {} not found",
            tls.certificate_path.display()
        )));
    }
    Ok(())
}

/// Check the TLS secret-key file exists (port of `hasKey`).
pub fn has_key(tls: &TlsConf) -> Result<(), InitializationException> {
    if !tls.key_path.exists() {
        return Err(InitializationException(format!(
            "Secret key file {} not found",
            tls.key_path.display()
        )));
    }
    Ok(())
}

/// Compute the node identifier from its key (port of `name`).
fn name(tls: &TlsConf) -> Result<NodeIdentifier, InitializationException> {
    generate_certificate_if_absent::node_address(tls)
        .map(NodeIdentifier::new)
        .map_err(|e| {
            InitializationException(format!("Failed to read the X.509 certificate: {e}"))
        })
}

/// Initialize the node environment and derive its identifier (port of `NodeEnvironment.create`).
pub fn create(conf: &NodeConf) -> Result<NodeIdentifier, InitializationException> {
    let data_dir = &conf.storage.data_dir;
    can_create_data_dir(data_dir)?;
    have_access_to_data_dir(data_dir)?;
    generate_certificate_if_absent::run(&conf.tls).map_err(InitializationException)?;
    has_certificate(&conf.tls)?;
    has_key(&conf.tls)?;
    name(&conf.tls)
}
