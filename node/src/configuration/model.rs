//! Node configuration model (port of `configuration/model.scala`).

use std::path::PathBuf;
use std::time::Duration;

use rchain_casper::protocol::client::Name;
use rchain_casper::CasperConf;
use rchain_comm::peer_node::PeerNode;
use rchain_comm::transport::tls_conf::TlsConf;
use rchain_crypto::private_key::PrivateKey;
use rchain_crypto::public_key::PublicKey;

/// Root node configuration (port of the Scala `NodeConf` case class).
#[derive(Clone, Debug, PartialEq)]
pub struct NodeConf {
    pub standalone: bool,
    pub autopropose: bool,
    pub protocol_server: ProtocolServer,
    pub protocol_client: ProtocolClient,
    pub peers_discovery: PeersDiscovery,
    pub api_server: ApiServer,
    pub tls: TlsConf,
    pub storage: Storage,
    pub casper: CasperConf,
    pub metrics: Metrics,
    pub dev_mode: bool,
    pub dev: DevConf,
    pub default_data_dir: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProtocolServer {
    pub network_id: String,
    pub host: Option<String>,
    pub use_random_ports: bool,
    pub dynamic_ip: bool,
    pub no_upnp: bool,
    pub port: i32,
    pub grpc_max_recv_message_size: i64,
    pub grpc_max_recv_stream_message_size: i64,
    pub max_message_consumers: i32,
    pub disable_state_exporter: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProtocolClient {
    pub network_id: String,
    pub bootstrap: PeerNode,
    pub disable_lfs: bool,
    pub batch_max_connections: i32,
    pub network_timeout: Duration,
    pub grpc_max_recv_message_size: i64,
    pub grpc_stream_chunk_size: i64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PeersDiscovery {
    pub port: i32,
    pub lookup_interval: Duration,
    pub cleanup_interval: Duration,
    pub heartbeat_batch_size: i32,
    pub init_wait_loop_interval: Duration,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ApiServer {
    pub host: String,
    pub port_grpc_external: i32,
    pub port_grpc_internal: i32,
    pub grpc_max_recv_message_size: i64,
    pub port_http: i32,
    pub port_admin_http: i32,
    pub max_blocks_limit: i32,
    pub enable_reporting: bool,
    pub keep_alive_time: Duration,
    pub keep_alive_timeout: Duration,
    pub permit_keep_alive_time: Duration,
    pub max_connection_idle: Duration,
    pub max_connection_age: Duration,
    pub max_connection_age_grace: Duration,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Storage {
    pub data_dir: PathBuf,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Metrics {
    pub prometheus: bool,
    pub influxdb: bool,
    pub influxdb_udp: bool,
    pub zipkin: bool,
    pub sigar: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DevConf {
    pub deployer_private_key: Option<String>,
}

/// CLI subcommand outcome (port of the Scala `Command` ADT).
#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Eval {
        files: Vec<String>,
        print_unmatched_sends_only: bool,
    },
    Repl,
    Deploy {
        phlo_limit: i64,
        phlo_price: i64,
        valid_after_block: i64,
        private_key: Option<PrivateKey>,
        private_key_path: Option<PathBuf>,
        location: String,
        shard_id: String,
    },
    DeployStatus {
        id: Vec<u8>,
    },
    FindDeploy {
        id: Vec<u8>,
    },
    Propose {
        print_unmatched_sends: bool,
    },
    ShowBlock {
        hash: String,
    },
    ShowBlocks {
        depth: i32,
    },
    VisualizeDag {
        depth: i32,
        show_justification_lines: bool,
    },
    MachineVerifiableDag,
    Run,
    Keygen {
        path: PathBuf,
    },
    LastFinalizedBlock,
    IsFinalized {
        hash: String,
    },
    BondStatus {
        public_key: PublicKey,
    },
    Help,
    DataAtName {
        name: Name,
    },
    ContAtName {
        names: Vec<Name>,
    },
    Status,
}
