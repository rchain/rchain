//! Command-line options (port of `Options.scala`).

use std::path::PathBuf;
use std::time::Duration;

use clap::{Parser, Subcommand};
use rchain_comm::peer_node::PeerNode;

use super::super::hocon::parse_duration;

fn parse_duration_arg(s: &str) -> Result<Duration, String> {
    parse_duration(s).ok_or_else(|| {
        format!("'{s}': finite duration is expected, e.g. 20 seconds, 4 minutes, etc.")
    })
}

fn parse_peer_node_arg(s: &str) -> Result<PeerNode, String> {
    PeerNode::from_address(s).map_err(|_| "Can not parse the bootstrap address".to_string())
}

fn parse_base16(s: &str) -> Result<Vec<u8>, String> {
    rchain_shared::base16::decode(s)
        .ok_or_else(|| format!("Error parsing value. Invalid base16 encoding: {s}"))
}

/// The CLI surface (port of the Scala `Options` scallop config).
#[derive(Parser, Debug)]
#[command(name = "rchain", version, about = "RChain node | gRPC client", disable_help_flag = true)]
pub struct Options {
    /// Print help.
    #[arg(long = "help", action = clap::ArgAction::Help)]
    help: Option<bool>,
    /// Remote gRPC host for client calls.
    #[arg(short = 'h', long = "grpc-host", default_value = "localhost")]
    pub grpc_host: String,

    /// Remote gRPC port for client calls. Defaults to 40401 (external) for `deploy`, and 40402
    /// (internal) for `repl`/`propose`.
    #[arg(short = 'p', long = "grpc-port")]
    pub grpc_port: Option<i32>,

    /// Max inbound gRPC message size for client calls.
    #[arg(short = 's', long = "grpc-max-recv-message-size", default_value_t = 16 * 1024 * 1024)]
    pub grpc_max_recv_message_size: i32,

    /// Predefined set of defaults to use: default or docker.
    #[arg(long = "profile")]
    pub profile: Option<String>,

    #[command(subcommand)]
    pub subcommand: Commands,
}

#[derive(Subcommand, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Commands {
    /// Start RNode server.
    Run(Run),
    /// Generates a public/private key pair.
    Keygen {
        /// Folder to save keyfiles. Defaults to './'
        #[arg(default_value = "")]
        location: PathBuf,
    },
    /// View properties of the last finalized block.
    LastFinalizedBlock,
    /// Check if the given block has been finalized.
    IsFinalized {
        /// The hash value of the block to check.
        hash: String,
    },
    /// Starts a thin client REPL.
    Repl,
    /// Evaluate rholang in a file on an existing running node.
    Eval {
        #[arg(required = true)]
        file_names: Vec<String>,
        #[arg(long = "print-unmatched-sends-only")]
        print_unmatched_sends_only: bool,
    },
    /// Deploy a Rholang source file.
    Deploy {
        #[arg(long = "phlo-limit", required = true)]
        phlo_limit: i64,
        #[arg(long = "phlo-price", required = true)]
        phlo_price: i64,
        #[arg(long = "valid-after-block-number")]
        valid_after_block_number: Option<i64>,
        #[arg(long = "private-key", value_parser = parse_base16)]
        private_key: Option<Vec<u8>>,
        #[arg(long = "private-key-path")]
        private_key_path: Option<PathBuf>,
        #[arg(long = "shard-id", default_value = "")]
        shard_id: String,
        #[arg(required = true)]
        location: String,
    },
    /// Returns the status of the deploy with provided signature.
    DeployStatus {
        #[arg(long = "deploy-signature", required = true, value_parser = parse_base16)]
        deploy_signature: Vec<u8>,
    },
    /// View properties of a block.
    ShowBlock {
        /// The hash value of the block.
        hash: String,
    },
    /// View list of blocks.
    ShowBlocks {
        /// Lists blocks to the given depth.
        #[arg(long = "depth")]
        depth: Option<i32>,
    },
    /// DAG in DOT format.
    Vdag {
        #[arg(long = "depth")]
        depth: Option<i32>,
        #[arg(long = "showJustificationlines")]
        show_justification_lines: bool,
    },
    /// Machine Verifiable DAG.
    Mvdag,
    /// Listen for data at the specified name.
    ListenDataAtName {
        #[arg(short = 't', long = "type", required = true)]
        type_of_name: String,
        #[arg(short = 'c', long = "content", required = true)]
        content: Vec<String>,
    },
    /// Listen for continuation at the specified name.
    ListenContAtName {
        #[arg(short = 't', long = "type", required = true)]
        type_of_name: String,
        #[arg(short = 'c', long = "content", required = true)]
        content: Vec<String>,
    },
    /// Searches for a block containing the deploy with provided id.
    FindDeploy {
        #[arg(long = "deploy-id", required = true, value_parser = parse_base16)]
        deploy_id: Vec<u8>,
    },
    /// Force Casper to propose a block.
    Propose {
        #[arg(long = "print-unmatched-sends")]
        print_unmatched_sends: bool,
    },
    /// Check bond status for a validator public key.
    BondStatus {
        #[arg(value_parser = parse_base16)]
        validator_public_key: Vec<u8>,
    },
    /// Get RNode status information.
    Status,
}

/// The `run` subcommand options (port of `Options.run`).
#[derive(Parser, Debug)]
pub struct Run {
    /// Path to the configuration file for RNode server.
    #[arg(short = 'c', long = "config-file")]
    pub config_file: Option<PathBuf>,

    /// Number of threads allocated for main scheduler (hidden).
    #[arg(long = "thread-pool-size", hide = true)]
    pub thread_pool_size: Option<i32>,

    /// Start a stand-alone node.
    #[arg(short = 's', long = "standalone")]
    pub standalone: bool,

    /// Address of RNode to bootstrap from.
    #[arg(short = 'b', long = "bootstrap", value_parser = parse_peer_node_arg)]
    pub bootstrap: Option<PeerNode>,

    /// ID of the RChain network to connect to.
    #[arg(long = "network-id")]
    pub network_id: Option<String>,

    /// Make node automatically propose blocks.
    #[arg(long = "autopropose")]
    pub autopropose: bool,

    /// Disable UPnP.
    #[arg(long = "no-upnp")]
    pub no_upnp: bool,

    /// Host IP address changes dynamically.
    #[arg(long = "dynamic-ip")]
    pub dynamic_ip: bool,

    /// Number of validator identities to generate.
    #[arg(long = "autogen-shard-size")]
    pub autogen_shard_size: Option<i32>,

    /// Disable start from Last Finalized State.
    #[arg(long = "disable-lfs")]
    pub disable_lfs: bool,

    /// Address to bind RChain Protocol server.
    #[arg(long = "host")]
    pub host: Option<String>,

    /// Use random ports if configured ports are not free.
    #[arg(long = "use-random-ports")]
    pub use_random_ports: bool,

    /// Allow connections to peers with private network addresses (unused).
    #[arg(long = "allow-private-addresses")]
    pub allow_private_addresses: bool,

    /// Disable the node respond to export state requests.
    #[arg(long = "disable-state-exporter")]
    pub disable_state_exporter: bool,

    /// Default timeout for network calls.
    #[arg(long = "network-timeout", value_parser = parse_duration_arg)]
    pub network_timeout: Option<Duration>,

    /// Port used for node discovery (Kademlia).
    #[arg(long = "discovery-port")]
    pub discovery_port: Option<i32>,

    /// Peer discovery interval.
    #[arg(long = "discovery-lookup-interval", value_parser = parse_duration_arg)]
    pub discovery_lookup_interval: Option<Duration>,

    /// Peer discovery cleanup interval.
    #[arg(long = "discovery-cleanup-interval", value_parser = parse_duration_arg)]
    pub discovery_cleanup_interval: Option<Duration>,

    /// Check for first connection loop interval.
    #[arg(long = "discovery-init-wait-loop-interval", value_parser = parse_duration_arg)]
    pub discovery_init_wait_loop_interval: Option<Duration>,

    /// Peer discovery heartbeat batch size.
    #[arg(long = "discovery-heartbeat-batch-size")]
    pub discovery_heartbeat_batch_size: Option<i32>,

    /// gRPC port serving RChain Protocol messages.
    #[arg(short = 'p', long = "protocol-port")]
    pub protocol_port: Option<i32>,

    /// Maximum message size for gRPC transport server.
    #[arg(long = "protocol-grpc-max-recv-message-size")]
    pub protocol_grpc_max_recv_message_size: Option<i64>,

    /// Maximum size of messages received via transport streams.
    #[arg(long = "protocol-grpc-max-recv-stream-message-size")]
    pub protocol_grpc_max_recv_stream_message_size: Option<i64>,

    /// Chunk size for streaming packets between nodes.
    #[arg(long = "protocol-grpc-stream-chunk-size")]
    pub protocol_grpc_stream_chunk_size: Option<i32>,

    /// Number of connected peers picked for broadcasting/streaming.
    #[arg(long = "protocol-max-connections")]
    pub protocol_max_connections: Option<i32>,

    /// Number of incoming message consumers.
    #[arg(long = "protocol-max-message-consumers")]
    pub protocol_max_message_consumers: Option<i32>,

    /// Path to private key for TLS.
    #[arg(short = 'k', long = "tls-key-path")]
    pub tls_key_path: Option<PathBuf>,

    /// Path to X.509 certificate for TLS.
    #[arg(long = "tls-certificate-path")]
    pub tls_certificate_path: Option<PathBuf>,

    /// Use a non blocking secure random instance.
    #[arg(long = "tls-secure-random-non-blocking")]
    pub tls_secure_random_non_blocking: bool,

    /// Address to bind API servers.
    #[arg(long = "api-host")]
    pub api_host: Option<String>,

    /// Port for external gRPC API.
    #[arg(short = 'e', long = "api-port-grpc-external")]
    pub api_port_grpc_external: Option<i32>,

    /// Port for internal gRPC API.
    #[arg(short = 'i', long = "api-port-grpc-internal")]
    pub api_port_grpc_internal: Option<i32>,

    /// Maximum message size for gRPC API server.
    #[arg(long = "api-grpc-max-recv-message-size")]
    pub api_grpc_max_recv_message_size: Option<i32>,

    /// Port for HTTP services.
    #[arg(short = 'h', long = "api-port-http")]
    pub api_port_http: Option<i32>,

    /// Port for admin HTTP services.
    #[arg(short = 'a', long = "api-port-admin-http")]
    pub api_port_admin_http: Option<i32>,

    /// The max block numbers you can acquire from api.
    #[arg(long = "api-max-blocks-limit")]
    pub api_max_blocks_limit: Option<i32>,

    /// Enable reporting endpoints.
    #[arg(long = "api-enable-reporting")]
    pub api_enable_reporting: bool,

    /// Custom keepalive time.
    #[arg(long = "api-keep-alive-time", value_parser = parse_duration_arg)]
    pub api_keep_alive_time: Option<Duration>,

    /// Custom keepalive timeout.
    #[arg(long = "api-keep-alive-timeout", value_parser = parse_duration_arg)]
    pub api_keep_alive_timeout: Option<Duration>,

    /// Most aggressive keep-alive time clients are permitted.
    #[arg(long = "api-permit-keep-alive-time", value_parser = parse_duration_arg)]
    pub api_permit_keep_alive_time: Option<Duration>,

    /// Custom max connection idle time.
    #[arg(long = "api-max-connection-idle", value_parser = parse_duration_arg)]
    pub api_max_connection_idle: Option<Duration>,

    /// Custom max connection age.
    #[arg(long = "api-max-connection-age", value_parser = parse_duration_arg)]
    pub api_max_connection_age: Option<Duration>,

    /// Custom grace time for connection termination.
    #[arg(long = "api-max-connection-age-grace", value_parser = parse_duration_arg)]
    pub api_max_connection_age_grace: Option<Duration>,

    /// Path to data directory.
    #[arg(long = "data-dir")]
    pub data_dir: Option<PathBuf>,

    /// Name of the shard this node is connected to.
    #[arg(long = "shard-name")]
    pub shard_name: Option<String>,

    /// Base16 encoding of the public key for signing proposed blocks.
    #[arg(long = "validator-public-key")]
    pub validator_public_key: Option<String>,

    /// Base16 encoding of the private key for signing proposed blocks (hidden).
    #[arg(long = "validator-private-key", hide = true)]
    pub validator_private_key: Option<String>,

    /// Path to the base16 encoded private key for signing proposed blocks.
    #[arg(long = "validator-private-key-path")]
    pub validator_private_key_path: Option<PathBuf>,

    /// Interval for the casper loop.
    #[arg(long = "casper-loop-interval", value_parser = parse_duration_arg)]
    pub casper_loop_interval: Option<Duration>,

    /// Timeout for blocks requests.
    #[arg(long = "requested-blocks-timeout", value_parser = parse_duration_arg)]
    pub requested_blocks_timeout: Option<Duration>,

    /// Maximum number of block parents.
    #[arg(long = "max-number-of-parents")]
    pub max_number_of_parents: Option<i32>,

    /// Fork choice stale threshold.
    #[arg(long = "fork-choice-stale-threshold", value_parser = parse_duration_arg)]
    pub fork_choice_stale_threshold: Option<Duration>,

    /// Interval for checking if fork choice tip is stale.
    #[arg(long = "fork-choice-check-if-stale-interval", value_parser = parse_duration_arg)]
    pub fork_choice_check_if_stale_interval: Option<Duration>,

    /// Synchrony constraint threshold (fraction of stake).
    #[arg(long = "synchrony-constraint-threshold")]
    pub synchrony_constraint_threshold: Option<f64>,

    /// How far ahead of the last finalized block the node is allowed to propose.
    #[arg(long = "height-constraint-threshold")]
    pub height_constraint_threshold: Option<i64>,

    /// Bonds file (genesis).
    #[arg(long = "bonds-file")]
    pub bonds_file: Option<String>,

    /// Wallets file (genesis).
    #[arg(long = "wallets-file")]
    pub wallets_file: Option<String>,

    /// Minimum bond accepted by the PoS contract.
    #[arg(long = "bond-minimum")]
    pub bond_minimum: Option<i64>,

    /// Genesis block number for hard fork.
    #[arg(long = "genesis-block-number")]
    pub genesis_block_number: Option<i64>,

    /// Maximum bond accepted by the PoS contract.
    #[arg(long = "bond-maximum")]
    pub bond_maximum: Option<i64>,

    /// Length of the validation epoch in blocks.
    #[arg(long = "epoch-length")]
    pub epoch_length: Option<i32>,

    /// Length of the quarantine time in blocks.
    ///
    /// Reserved: parsed and validated, but not yet wired to the native PoS contract
    /// (see `spec/RUST-FIRST.md`).
    #[arg(long = "quarantine-length")]
    pub quarantine_length: Option<i32>,

    /// Number of active validators.
    #[arg(long = "number-of-active-validators")]
    pub number_of_active_validators: Option<i32>,

    /// Public key for transfers from the PoS vault.
    ///
    /// Reserved: parsed and validated, but not yet wired to the native PoS contract
    /// (see `spec/RUST-FIRST.md`).
    #[arg(long = "pos-vault-pub-key")]
    pub pos_vault_pub_key: Option<String>,

    /// Public key to manage system contract updates.
    #[arg(long = "system-contract-pub-key")]
    pub system_contract_pub_key: Option<String>,

    /// Enable the Prometheus metrics reporter.
    #[arg(long = "prometheus")]
    pub prometheus: bool,

    /// Enable the InfluxDB metrics reporter.
    #[arg(long = "influxdb")]
    pub influxdb: bool,

    /// Enable the InfluxDB UDP metrics reporter.
    #[arg(long = "influxdb-udp")]
    pub influxdb_udp: bool,

    /// Enable the Zipkin span reporter.
    #[arg(long = "zipkin")]
    pub zipkin: bool,

    /// Enable Sigar host system metrics.
    #[arg(long = "sigar")]
    pub sigar: bool,

    /// Enable all developer tools.
    #[arg(long = "dev-mode")]
    pub dev_mode: bool,

    /// Private key for dummy deploys.
    #[arg(long = "deployer-private-key")]
    pub deployer_private_key: Option<String>,

    /// MinPhloPrice.
    #[arg(long = "min-phlo-price")]
    pub min_phlo_price: Option<i64>,

    /// Space-separated list of public keys.
    #[arg(long = "pos-multi-sig-public-keys", num_args = 1..)]
    pub pos_multi_sig_public_keys: Option<Vec<String>>,

    /// How many confirmations are necessary to use multi-sig vault.
    ///
    /// Reserved: parsed and validated, but not yet wired to the native PoS contract
    /// (see `spec/RUST-FIRST.md`).
    #[arg(long = "pos-multi-sig-quorum")]
    pub pos_multi_sig_quorum: Option<i32>,
}
