//! Shared harness for node-level integration tests.
//!
//! Assembles a real standalone node in-process (mirroring `main.rs`: `Configuration::build` →
//! `node_environment::create` → `setup_node_program` → `serve`) over an ephemeral data dir and
//! loopback ports, then drives its gRPC + HTTP surfaces.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use rchain_casper::validator_identity::ValidatorIdentity;
use rchain_comm::peer_node::NodeIdentifier;
use rchain_node::configuration::configuration::parse_defaults;
use rchain_node::configuration::hocon::node_conf_from_hocon;
use rchain_node::configuration::model::NodeConf;
use rchain_node::runtime::node_environment;
use rchain_node::runtime::node_runtime::{setup_node_program, NodeProgram};
use rchain_shared::base16;
use rchain_shared::log::StderrLog;

/// The default secp256k1 validator private key (hex), port of `ConstructDeploy.defaultSec`.
pub const VALIDATOR_PRIV_HEX: &str = "a68a6e6cca30f81bd24a719f3145d20e8424bd7b396309b0708a16c7d8000b76";

/// A multi-threaded tokio runtime with a large per-worker stack. The genesis blessed terms recurse
/// deeper than the default 2 MiB worker stack allows (matching the node binary's runtime).
pub fn test_runtime() -> tokio::runtime::Runtime {
    tokio::runtime::Builder::new_multi_thread()
        .thread_stack_size(32 * 1024 * 1024)
        .enable_all()
        .build()
        .expect("build test runtime")
}

/// Create a temporary directory for a test (caller removes it after dropping the node).
pub fn temp_dir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "rchain-it-{name}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// Allocate `n` free loopback TCP ports. The ports are released when the returned listeners drop.
pub fn free_ports(n: usize) -> Vec<u16> {
    let mut listeners = Vec::with_capacity(n);
    let mut ports = Vec::with_capacity(n);
    for _ in 0..n {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind ephemeral port");
        ports.push(listener.local_addr().expect("local addr").port());
        listeners.push(listener);
    }
    ports
}

/// Build a standalone `NodeConf` bound to loopback with the given `[http, admin-http, grpc-internal,
/// protocol]` ports. When `validator_hex` is set, a matching bonds file is written and wired into
/// genesis so the validator is bonded.
pub fn standalone_conf(dir: &Path, ports: &[u16], validator_hex: Option<&str>) -> NodeConf {
    assert!(ports.len() >= 4, "need [http, admin-http, grpc-internal, protocol] ports");
    let defaults = parse_defaults(dir.to_str().unwrap()).expect("parse defaults");
    let mut conf = node_conf_from_hocon(&defaults).expect("node conf from hocon");

    conf.storage.data_dir = dir.to_path_buf();
    conf.api_server.host = "127.0.0.1".to_string();
    conf.api_server.port_http = ports[0] as i32;
    conf.api_server.port_admin_http = ports[1] as i32;
    conf.api_server.port_grpc_internal = ports[2] as i32;
    conf.protocol_server.port = ports[3] as i32;

    conf.standalone = true;
    conf.protocol_server.no_upnp = true;
    conf.protocol_server.host = Some("127.0.0.1".to_string());

    if let Some(hex) = validator_hex {
        conf.casper.validator_private_key = Some(hex.to_string());
        let identity = ValidatorIdentity::from_hex(hex).expect("validator identity");
        let pub_hex = base16::encode(identity.public_key.bytes());
        let bonds = dir.join("bonds.txt");
        std::fs::write(&bonds, format!("{pub_hex} 100\n")).expect("write bonds file");
        conf.casper.genesis_block_data.bonds_file = bonds.to_string_lossy().into_owned();
    }

    // `create_genesis_block` calls `vault_parser::parse` (not `parse_if_exists`), so the wallets
    // file must exist — an empty one yields no initial vaults.
    let wallets = dir.join("wallets.txt");
    if !wallets.exists() {
        std::fs::write(&wallets, "").expect("write wallets file");
    }
    conf.casper.genesis_block_data.wallets_file = wallets.to_string_lossy().into_owned();

    conf
}

/// A running in-process node: the served-program task handle and the node identifier.
pub struct TestNode {
    pub handle: tokio::task::JoinHandle<Result<(), String>>,
    pub id: NodeIdentifier,
    pub grpc_port: u16,
    pub http_port: u16,
}

impl TestNode {
    /// Abort the server task (the node has no graceful-shutdown RPC).
    pub fn shutdown(self) {
        self.handle.abort();
    }
}

/// Initialize the environment, assemble the node, and start serving it.
pub async fn start(conf: &NodeConf, grpc_port: u16, http_port: u16) -> TestNode {
    let id = node_environment::create(conf).expect("node environment");
    let program: NodeProgram = setup_node_program(conf, &id, Arc::new(StderrLog))
        .await
        .expect("setup node program");
    let handle = tokio::spawn(program.serve());
    TestNode {
        handle,
        id,
        grpc_port,
        http_port,
    }
}
