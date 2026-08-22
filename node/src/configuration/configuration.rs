//! Configuration assembly (port of `Configuration.scala`).

use std::path::PathBuf;

use hocon::Hocon;
use linked_hash_map::LinkedHashMap;

use super::commandline::config_mapper;
use super::commandline::options::{Commands, Options};
use super::hocon::node_conf_from_hocon;
use super::model::NodeConf;

/// A named set of defaults (port of `Configuration.Profile`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Profile {
    pub name: String,
    pub data_dir: PathBuf,
    pub description: String,
}

fn default_profile() -> Profile {
    let home = std::env::var_os("HOME")
        .map(PathBuf::from)
        .unwrap_or_default();
    Profile {
        name: "default".to_string(),
        data_dir: home.join(".rnode"),
        description: "Defaults to $HOME/.rnode".to_string(),
    }
}

fn docker_profile() -> Profile {
    Profile {
        name: "docker".to_string(),
        data_dir: PathBuf::from("/var/lib/rnode"),
        description: "Defaults to /var/lib/rnode".to_string(),
    }
}

pub fn profiles() -> Vec<Profile> {
    vec![default_profile(), docker_profile()]
}

/// The config assembly (port of `Configuration` object).
pub struct Configuration;

impl Configuration {
    /// Build a `NodeConf` from CLI options (port of `Configuration.build`). The kamon config
    /// (the 4th return value in Scala) is deferred.
    pub fn build(options: &Options) -> Result<(NodeConf, Profile, Option<PathBuf>), String> {
        let profile = match &options.profile {
            Some(name) => profiles()
                .into_iter()
                .find(|p| &p.name == name)
                .unwrap_or_else(default_profile),
            None => default_profile(),
        };

        let run = match &options.subcommand {
            Commands::Run(run) => run,
            _ => return Err("`run` subcommand is required to build a node configuration".to_string()),
        };

        let data_dir = run
            .data_dir
            .clone()
            .unwrap_or_else(|| profile.data_dir.clone());

        let config_file_path = run
            .config_file
            .clone()
            .unwrap_or_else(|| data_dir.join("rnode.conf"));
        let config_file = if config_file_path.exists() {
            Some(config_file_path)
        } else {
            None
        };

        let options_config = config_mapper::from_options(options);
        let file_config = match &config_file {
            Some(path) => parse_file(path)?,
            None => Hocon::Hash(LinkedHashMap::new()),
        };
        let default_config = parse_defaults(&data_dir.to_string_lossy())?;

        let merged = merge(merge(options_config, file_config), default_config);
        let node_conf = node_conf_from_hocon(&merged)?;

        let quorum = node_conf.casper.genesis_block_data.pos_multi_sig_quorum;
        let keys_len = node_conf
            .casper
            .genesis_block_data
            .pos_multi_sig_public_keys
            .len();
        if quorum > keys_len as i32 {
            return Err(format!(
                "defaults.conf: The value 'pos-multi-sig-quorum' should be less or equal the length of 'pos-multi-sig-public-keys' (the actual values are '{quorum}' and '{keys_len}' respectively)"
            ));
        }

        Ok((check_dev_mode(node_conf), profile, config_file))
    }
}

/// If not in dev mode, strip the deployer private key (port of `Configuration.checkDevMode`).
pub fn check_dev_mode(node_conf: NodeConf) -> NodeConf {
    if node_conf.dev_mode {
        node_conf
    } else {
        if node_conf.dev.deployer_private_key.is_some() {
            println!("Node is not in dev mode, ignoring --deployer-private-key");
        }
        NodeConf {
            dev: super::model::DevConf {
                deployer_private_key: None,
            },
            ..node_conf
        }
    }
}

/// Parse the bundled `defaults.conf` with `default-data-dir` injected (port of the
/// `ConfigSource.resources("defaults.conf").withFallback(...)` default source).
pub fn parse_defaults(data_dir: &str) -> Result<Hocon, String> {
    let defaults = include_str!("defaults.conf");
    let combined = format!("default-data-dir = \"{data_dir}\"\n{defaults}");
    hocon::HoconLoader::new()
        .load_str(&combined)
        .map_err(|e| e.to_string())?
        .hocon()
        .map_err(|e| e.to_string())
}

/// Parse a user HOCON config file.
pub fn parse_file(path: &PathBuf) -> Result<Hocon, String> {
    hocon::HoconLoader::new()
        .load_file(path)
        .map_err(|e| e.to_string())?
        .hocon()
        .map_err(|e| e.to_string())
}

/// Deep merge with fallback semantics (port of typesafe-config `withFallback`): the higher-priority
/// value wins; nested objects merge recursively.
pub fn merge(high: Hocon, low: Hocon) -> Hocon {
    match (high, low) {
        (Hocon::Hash(mut high_map), Hocon::Hash(low_map)) => {
            for (key, low_value) in low_map {
                match high_map.get(&key) {
                    Some(high_value) => {
                        let merged = merge(high_value.clone(), low_value);
                        high_map.insert(key, merged);
                    }
                    None => {
                        high_map.insert(key, low_value);
                    }
                }
            }
            Hocon::Hash(high_map)
        }
        (high, _) => high,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::configuration::model::{
        ApiServer, DevConf, Metrics, PeersDiscovery, ProtocolClient, ProtocolServer, Storage,
    };
    use rchain_casper::{CasperConf, GenesisBlockData};
    use rchain_comm::peer_node::PeerNode;
    use rchain_comm::transport::tls_conf::TlsConf;
    use std::time::Duration;

    fn default_pos_multi_sig_public_keys() -> Vec<String> {
        vec![
            "04db91a53a2b72fcdcb201031772da86edad1e4979eb6742928d27731b1771e0bc40c9e9c9fa6554bdec041a87cee423d6f2e09e9dfb408b78e85a4aa611aad20c".to_string(),
            "042a736b30fffcc7d5a58bb9416f7e46180818c82b15542d0a7819d1a437aa7f4b6940c50db73a67bfc5f5ec5b5fa555d24ef8339b03edaa09c096de4ded6eae14".to_string(),
            "047f0f0f5bbe1d6d1a8dac4d88a3957851940f39a57cd89d55fe25b536ab67e6d76fd3f365c83e5bfe11fe7117e549b1ae3dd39bfc867d1c725a4177692c4e7754".to_string(),
        ]
    }

    fn default_pos_vault_pub_key() -> String {
        "0432946f7f91f8f767d7c3d43674faf83586dffbd1b8f9278a5c72820dc20308836299f47575ff27f4a736b72e63d91c3cd853641861f64e08ee5f9204fc708df6".to_string()
    }

    fn default_system_contract_pub_key() -> String {
        "04e2eb6b06058d10b30856043c29076e2d2d7c374d2beedded6ecb8d1df585dfa583bd7949085ac6b0761497b0cfd056eb3d0db97efb3940b14c00fff4e53c85bf".to_string()
    }

    fn secs(s: u64) -> Duration {
        Duration::from_secs(s)
    }

    fn default_expected() -> NodeConf {
        let bootstrap = PeerNode::from_address(
            "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404",
        )
        .unwrap();
        NodeConf {
            standalone: false,
            autopropose: false,
            dev_mode: false,
            protocol_server: ProtocolServer {
                network_id: "testnet".to_string(),
                host: None,
                use_random_ports: false,
                dynamic_ip: false,
                no_upnp: false,
                port: 40400,
                grpc_max_recv_message_size: 262144,
                grpc_max_recv_stream_message_size: 268435456,
                max_message_consumers: 400,
                disable_state_exporter: false,
            },
            protocol_client: ProtocolClient {
                network_id: "testnet".to_string(),
                bootstrap,
                disable_lfs: false,
                batch_max_connections: 20,
                network_timeout: secs(5),
                grpc_max_recv_message_size: 262144,
                grpc_stream_chunk_size: 262144,
            },
            peers_discovery: PeersDiscovery {
                port: 40404,
                lookup_interval: secs(20),
                cleanup_interval: secs(20 * 60),
                heartbeat_batch_size: 100,
                init_wait_loop_interval: secs(1),
            },
            api_server: ApiServer {
                host: "0.0.0.0".to_string(),
                port_grpc_external: 40401,
                port_grpc_internal: 40402,
                grpc_max_recv_message_size: 16777216,
                port_http: 40403,
                port_admin_http: 40405,
                max_blocks_limit: 50,
                enable_reporting: false,
                keep_alive_time: secs(2 * 60 * 60),
                keep_alive_timeout: secs(20),
                permit_keep_alive_time: secs(5 * 60),
                max_connection_idle: secs(60 * 60),
                max_connection_age: secs(60 * 60),
                max_connection_age_grace: secs(60 * 60),
            },
            storage: Storage {
                data_dir: PathBuf::from("/var/lib/rnode"),
            },
            tls: TlsConf {
                certificate_path: PathBuf::from("/var/lib/rnode/node.certificate.pem"),
                key_path: PathBuf::from("/var/lib/rnode/node.key.pem"),
                secure_random_non_blocking: false,
                custom_certificate_location: false,
                custom_key_location: false,
            },
            casper: CasperConf {
                validator_public_key: None,
                validator_private_key: None,
                validator_private_key_path: None,
                shard_name: "root".to_string(),
                casper_loop_interval: secs(30),
                requested_blocks_timeout: secs(240),
                max_number_of_parents: 2147483647,
                fork_choice_stale_threshold: secs(10 * 60),
                fork_choice_check_if_stale_interval: secs(11 * 60),
                synchrony_constraint_threshold: 0.67,
                height_constraint_threshold: 1000,
                genesis_block_data: GenesisBlockData {
                    genesis_data_dir: PathBuf::from("/var/lib/rnode/genesis"),
                    bonds_file: "/var/lib/rnode/genesis/bonds.txt".to_string(),
                    wallets_file: "/var/lib/rnode/genesis/wallets.txt".to_string(),
                    bond_minimum: 1,
                    bond_maximum: 9223372036854775807,
                    epoch_length: 10000,
                    quarantine_length: 50000,
                    genesis_block_number: 0,
                    number_of_active_validators: 100,
                    pos_multi_sig_public_keys: default_pos_multi_sig_public_keys(),
                    pos_multi_sig_quorum: 2,
                    pos_vault_pub_key: String::new(),
                    system_contract_pub_key: String::new(),
                },
                autogen_shard_size: 5,
                min_phlo_price: 1,
            },
            metrics: Metrics {
                prometheus: false,
                influxdb: false,
                influxdb_udp: false,
                zipkin: false,
                sigar: false,
            },
            dev: DevConf {
                deployer_private_key: None,
            },
            default_data_dir: "/var/lib/rnode".to_string(),
        }
    }

    #[test]
    fn parse_default_config() {
        let default_config = parse_defaults("/var/lib/rnode").unwrap();
        let config = node_conf_from_hocon(&default_config).unwrap();
        assert_eq!(config, default_expected());
    }

    #[test]
    fn cli_options_override_defaults() {
        use clap::Parser as _;
        use crate::configuration::commandline::options::Options;

        let args = "\
            run --standalone --dev-mode \
            --host localhost \
            --bootstrap rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404 \
            --network-id testnet --no-upnp --dynamic-ip --autogen-shard-size 111111 --use-random-ports \
            --network-timeout 111111seconds --discovery-port 111111 --discovery-lookup-interval 111111seconds \
            --discovery-cleanup-interval 111111seconds --discovery-heartbeat-batch-size 111111 \
            --discovery-init-wait-loop-interval 111111seconds --protocol-port 111111 \
            --protocol-grpc-max-recv-message-size 111111 --protocol-grpc-max-recv-stream-message-size 111111 \
            --protocol-grpc-stream-chunk-size 111111 --protocol-max-connections 111111 \
            --protocol-max-message-consumers 111111 --disable-state-exporter \
            --tls-certificate-path /var/lib/rnode/node.certificate.pem --tls-key-path /var/lib/rnode/node.key.pem \
            --tls-secure-random-non-blocking --api-host localhost --api-port-grpc-external 111111 \
            --api-port-grpc-internal 111111 --api-port-http 111111 --api-port-admin-http 111111 \
            --api-grpc-max-recv-message-size 111111 --api-max-blocks-limit 111111 --api-enable-reporting \
            --api-keep-alive-time 111111seconds --api-keep-alive-timeout 111111seconds \
            --api-permit-keep-alive-time 111111seconds --api-max-connection-idle 111111seconds \
            --api-max-connection-age 111111seconds --api-max-connection-age-grace 111111seconds \
            --data-dir /var/lib/rnode --shard-name root --validator-public-key 111111 \
            --validator-private-key 111111 --validator-private-key-path /var/lib/rnode/pem.key \
            --casper-loop-interval 111111seconds --requested-blocks-timeout 111111seconds \
            --max-number-of-parents 111111 --fork-choice-stale-threshold 111111seconds \
            --fork-choice-check-if-stale-interval 111111seconds --synchrony-constraint-threshold 111111 \
            --height-constraint-threshold 111111 --bonds-file /var/lib/rnode/genesis/bonds1.txt \
            --wallets-file /var/lib/rnode/genesis/wallets1.txt --bond-minimum 111111 --bond-maximum 111111 \
            --epoch-length 111111 --quarantine-length 111111 --genesis-block-number 222 \
            --number-of-active-validators 111111 --pos-vault-pub-key 0432946f7f91f8f767d7c3d43674faf83586dffbd1b8f9278a5c72820dc20308836299f47575ff27f4a736b72e63d91c3cd853641861f64e08ee5f9204fc708df6 \
            --system-contract-pub-key 04e2eb6b06058d10b30856043c29076e2d2d7c374d2beedded6ecb8d1df585dfa583bd7949085ac6b0761497b0cfd056eb3d0db97efb3940b14c00fff4e53c85bf \
            --disable-lfs --prometheus --influxdb --influxdb-udp --zipkin --sigar";

        let options = Options::parse_from(std::iter::once("rchain").chain(args.split_whitespace()));
        let options_config = config_mapper::from_options(&options);
        let default_config = parse_defaults("/var/lib/rnode").unwrap();
        let merged = merge(options_config, default_config);
        let config = node_conf_from_hocon(&merged).unwrap();

        let bootstrap = PeerNode::from_address(
            "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404",
        )
        .unwrap();

        let expected = NodeConf {
            standalone: true,
            autopropose: false,
            dev_mode: true,
            protocol_server: ProtocolServer {
                network_id: "testnet".to_string(),
                host: Some("localhost".to_string()),
                use_random_ports: true,
                dynamic_ip: true,
                no_upnp: true,
                port: 111111,
                grpc_max_recv_message_size: 111111,
                grpc_max_recv_stream_message_size: 111111,
                max_message_consumers: 111111,
                disable_state_exporter: true,
            },
            protocol_client: ProtocolClient {
                network_id: "testnet".to_string(),
                bootstrap,
                disable_lfs: true,
                batch_max_connections: 111111,
                network_timeout: secs(111111),
                grpc_max_recv_message_size: 111111,
                grpc_stream_chunk_size: 111111,
            },
            peers_discovery: PeersDiscovery {
                port: 111111,
                lookup_interval: secs(111111),
                cleanup_interval: secs(111111),
                heartbeat_batch_size: 111111,
                init_wait_loop_interval: secs(111111),
            },
            api_server: ApiServer {
                host: "localhost".to_string(),
                port_grpc_external: 111111,
                port_grpc_internal: 111111,
                grpc_max_recv_message_size: 111111,
                port_http: 111111,
                port_admin_http: 111111,
                max_blocks_limit: 111111,
                enable_reporting: true,
                keep_alive_time: secs(111111),
                keep_alive_timeout: secs(111111),
                permit_keep_alive_time: secs(111111),
                max_connection_idle: secs(111111),
                max_connection_age: secs(111111),
                max_connection_age_grace: secs(111111),
            },
            storage: Storage {
                data_dir: PathBuf::from("/var/lib/rnode"),
            },
            tls: TlsConf {
                certificate_path: PathBuf::from("/var/lib/rnode/node.certificate.pem"),
                key_path: PathBuf::from("/var/lib/rnode/node.key.pem"),
                secure_random_non_blocking: true,
                custom_certificate_location: false,
                custom_key_location: false,
            },
            casper: CasperConf {
                validator_public_key: Some("111111".to_string()),
                validator_private_key: Some("111111".to_string()),
                validator_private_key_path: Some(PathBuf::from("/var/lib/rnode/pem.key")),
                shard_name: "root".to_string(),
                casper_loop_interval: secs(111111),
                requested_blocks_timeout: secs(111111),
                max_number_of_parents: 111111,
                fork_choice_stale_threshold: secs(111111),
                fork_choice_check_if_stale_interval: secs(111111),
                synchrony_constraint_threshold: 111111.0,
                height_constraint_threshold: 111111,
                genesis_block_data: GenesisBlockData {
                    genesis_data_dir: PathBuf::from("/var/lib/rnode/genesis"),
                    bonds_file: "/var/lib/rnode/genesis/bonds1.txt".to_string(),
                    wallets_file: "/var/lib/rnode/genesis/wallets1.txt".to_string(),
                    bond_minimum: 111111,
                    bond_maximum: 111111,
                    epoch_length: 111111,
                    quarantine_length: 111111,
                    genesis_block_number: 222,
                    number_of_active_validators: 111111,
                    pos_multi_sig_public_keys: default_pos_multi_sig_public_keys(),
                    pos_multi_sig_quorum: 2,
                    pos_vault_pub_key: default_pos_vault_pub_key(),
                    system_contract_pub_key: default_system_contract_pub_key(),
                },
                autogen_shard_size: 111111,
                min_phlo_price: 1,
            },
            metrics: Metrics {
                prometheus: true,
                influxdb: true,
                influxdb_udp: true,
                zipkin: true,
                sigar: true,
            },
            dev: DevConf {
                deployer_private_key: None,
            },
            default_data_dir: "/var/lib/rnode".to_string(),
        };

        assert_eq!(config, expected);
    }
}
