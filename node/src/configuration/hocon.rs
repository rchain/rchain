//! HOCON → config conversion (port of the pureconfig readers in `Configuration.scala`).
//!
//! The `hocon` crate parses HOCON text into a `Hocon` tree (including `${...}` substitution), but
//! keeps sizes/durations as strings, so those are parsed here — mirroring the Scala custom
//! `myIntReader` (size-in-bytes `Long`) and the duration/`PeerNode` readers.

use std::path::PathBuf;
use std::time::Duration;

use hocon::Hocon;
use rchain_casper::{CasperConf, GenesisBlockData};
use rchain_comm::peer_node::PeerNode;
use rchain_comm::transport::tls_conf::TlsConf;

use super::model::{
    ApiServer, DevConf, Metrics, NodeConf, PeersDiscovery, ProtocolClient, ProtocolServer, Storage,
};

/// Parse a size-in-bytes string (`256K`, `16M`, `256M`) into a byte count (port of
/// typesafe-config `getBytes`). Accepts bare integers and `K`/`M`/`G`/`T`/`P` units (binary,
/// powers of 1024) with optional `B`/`i` suffixes.
pub fn parse_size(s: &str) -> Option<i64> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    if let Ok(n) = s.parse::<i64>() {
        return Some(n);
    }
    let mut split = 0;
    for (i, c) in s.char_indices() {
        if c.is_ascii_digit() || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E' {
            split = i + c.len_utf8();
        } else {
            break;
        }
    }
    let (num_s, unit) = s.split_at(split);
    let n: f64 = num_s.trim().parse().ok()?;
    let unit = unit.trim();
    let mult: f64 = match unit {
        "" | "B" | "b" => 1.0,
        "k" | "K" | "KiB" | "Ki" => 1024.0,
        "kB" => 1000.0,
        "m" | "M" | "MiB" | "Mi" => 1024.0_f64.powi(2),
        "MB" => 1000.0_f64.powi(2),
        "g" | "G" | "GiB" | "Gi" => 1024.0_f64.powi(3),
        "GB" => 1000.0_f64.powi(3),
        "t" | "T" | "TiB" | "Ti" => 1024.0_f64.powi(4),
        "TB" => 1000.0_f64.powi(4),
        "p" | "P" | "PiB" | "Pi" => 1024.0_f64.powi(5),
        "PB" => 1000.0_f64.powi(5),
        _ => return None,
    };
    Some((n * mult) as i64)
}

/// Parse a duration string (`20 seconds`, `5 minutes`, `111111seconds`) into a `Duration` (port of
/// `scala.concurrent.duration.Duration`).
pub fn parse_duration(s: &str) -> Option<Duration> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    let mut split = 0;
    for (i, c) in s.char_indices() {
        if c.is_ascii_digit() || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E' {
            split = i + c.len_utf8();
        } else {
            break;
        }
    }
    let (num_s, unit) = s.split_at(split);
    let n: f64 = num_s.trim().parse().ok()?;
    let unit = unit.trim().to_lowercase();
    let nanos: f64 = match unit.as_str() {
        "ns" | "nano" | "nanos" | "nanosecond" | "nanoseconds" => 1.0,
        "us" | "µs" | "micro" | "micros" | "microsecond" | "microseconds" => 1_000.0,
        "ms" | "milli" | "millis" | "millisecond" | "milliseconds" => 1_000_000.0,
        "s" | "sec" | "secs" | "second" | "seconds" => 1_000_000_000.0,
        "m" | "min" | "mins" | "minute" | "minutes" => 60_000_000_000.0,
        "h" | "hour" | "hours" => 3_600_000_000_000.0,
        "d" | "day" | "days" => 86_400_000_000_000.0,
        _ => return None,
    };
    Some(Duration::from_nanos((n * nanos) as u64))
}

// --- primitive getters -------------------------------------------------------

fn err<T>(expected: &str, got: &Hocon) -> Result<T, String> {
    Err(format!("expected {expected}, got {got:?}"))
}

fn get<'a>(h: &'a Hocon, key: &str) -> Result<&'a Hocon, String> {
    match h {
        Hocon::Hash(map) => map
            .get(key)
            .ok_or_else(|| format!("missing config key `{key}`")),
        _ => err("object", h),
    }
}

fn get_opt<'a>(h: &'a Hocon, key: &str) -> Option<&'a Hocon> {
    match h {
        Hocon::Hash(map) => map.get(key),
        _ => None,
    }
}

fn to_string(h: &Hocon) -> Result<String, String> {
    match h {
        Hocon::String(s) => Ok(s.clone()),
        _ => err("string", h),
    }
}

fn to_bool(h: &Hocon) -> Result<bool, String> {
    match h {
        Hocon::Boolean(b) => Ok(*b),
        _ => err("boolean", h),
    }
}

fn to_i32(h: &Hocon) -> Result<i32, String> {
    match h {
        Hocon::Integer(n) => i32::try_from(*n).map_err(|_| format!("integer out of range: {n}")),
        _ => err("integer", h),
    }
}

/// `Long` reader — accepts a plain number or a size-in-bytes string (Scala `myIntReader`).
fn to_i64(h: &Hocon) -> Result<i64, String> {
    match h {
        Hocon::Integer(n) => Ok(*n),
        Hocon::Real(x) => Ok(*x as i64),
        Hocon::String(s) => parse_size(s).ok_or_else(|| format!("invalid size value `{s}`")),
        _ => err("integer or size string", h),
    }
}

fn to_f64(h: &Hocon) -> Result<f64, String> {
    match h {
        Hocon::Real(x) => Ok(*x),
        Hocon::Integer(n) => Ok(*n as f64),
        _ => err("number", h),
    }
}

fn to_duration(h: &Hocon) -> Result<Duration, String> {
    match h {
        Hocon::Integer(nanos) => {
            let nanos =
                u64::try_from(*nanos).map_err(|_| format!("negative duration: {nanos}"))?;
            Ok(Duration::from_nanos(nanos))
        }
        Hocon::String(s) => parse_duration(s).ok_or_else(|| format!("invalid duration `{s}`")),
        _ => err("duration", h),
    }
}

fn to_path(h: &Hocon) -> Result<PathBuf, String> {
    to_string(h).map(PathBuf::from)
}

fn to_peer_node(h: &Hocon) -> Result<PeerNode, String> {
    let s = to_string(h)?;
    PeerNode::from_address(&s).map_err(|e| format!("invalid peer node address `{s}`: {e:?}"))
}

fn to_string_list(h: &Hocon) -> Result<Vec<String>, String> {
    match h {
        Hocon::Array(items) => items.iter().map(to_string).collect(),
        Hocon::String(s) => Ok(s.split(' ').map(|p| p.to_string()).collect()),
        _ => err("array or space-separated string", h),
    }
}

fn to_optional_string(h: &Hocon, key: &str) -> Result<Option<String>, String> {
    get_opt(h, key).map(to_string).transpose()
}

fn to_optional_path(h: &Hocon, key: &str) -> Result<Option<PathBuf>, String> {
    get_opt(h, key).map(to_path).transpose()
}

// --- struct converters -------------------------------------------------------

fn protocol_server_from_hocon(h: &Hocon) -> Result<ProtocolServer, String> {
    Ok(ProtocolServer {
        network_id: to_string(get(h, "network-id")?)?,
        host: to_optional_string(h, "host")?,
        use_random_ports: to_bool(get(h, "use-random-ports")?)?,
        dynamic_ip: to_bool(get(h, "dynamic-ip")?)?,
        no_upnp: to_bool(get(h, "no-upnp")?)?,
        port: to_i32(get(h, "port")?)?,
        grpc_max_recv_message_size: to_i64(get(h, "grpc-max-recv-message-size")?)?,
        grpc_max_recv_stream_message_size: to_i64(get(h, "grpc-max-recv-stream-message-size")?)?,
        max_message_consumers: to_i32(get(h, "max-message-consumers")?)?,
        disable_state_exporter: to_bool(get(h, "disable-state-exporter")?)?,
    })
}

fn protocol_client_from_hocon(h: &Hocon) -> Result<ProtocolClient, String> {
    Ok(ProtocolClient {
        network_id: to_string(get(h, "network-id")?)?,
        bootstrap: to_peer_node(get(h, "bootstrap")?)?,
        disable_lfs: to_bool(get(h, "disable-lfs")?)?,
        batch_max_connections: to_i32(get(h, "batch-max-connections")?)?,
        network_timeout: to_duration(get(h, "network-timeout")?)?,
        grpc_max_recv_message_size: to_i64(get(h, "grpc-max-recv-message-size")?)?,
        grpc_stream_chunk_size: to_i64(get(h, "grpc-stream-chunk-size")?)?,
    })
}

fn peers_discovery_from_hocon(h: &Hocon) -> Result<PeersDiscovery, String> {
    Ok(PeersDiscovery {
        port: to_i32(get(h, "port")?)?,
        lookup_interval: to_duration(get(h, "lookup-interval")?)?,
        cleanup_interval: to_duration(get(h, "cleanup-interval")?)?,
        heartbeat_batch_size: to_i32(get(h, "heartbeat-batch-size")?)?,
        init_wait_loop_interval: to_duration(get(h, "init-wait-loop-interval")?)?,
    })
}

fn api_server_from_hocon(h: &Hocon) -> Result<ApiServer, String> {
    Ok(ApiServer {
        host: to_string(get(h, "host")?)?,
        port_grpc_external: to_i32(get(h, "port-grpc-external")?)?,
        port_grpc_internal: to_i32(get(h, "port-grpc-internal")?)?,
        grpc_max_recv_message_size: to_i64(get(h, "grpc-max-recv-message-size")?)?,
        port_http: to_i32(get(h, "port-http")?)?,
        port_admin_http: to_i32(get(h, "port-admin-http")?)?,
        max_blocks_limit: to_i32(get(h, "max-blocks-limit")?)?,
        enable_reporting: to_bool(get(h, "enable-reporting")?)?,
        keep_alive_time: to_duration(get(h, "keep-alive-time")?)?,
        keep_alive_timeout: to_duration(get(h, "keep-alive-timeout")?)?,
        permit_keep_alive_time: to_duration(get(h, "permit-keep-alive-time")?)?,
        max_connection_idle: to_duration(get(h, "max-connection-idle")?)?,
        max_connection_age: to_duration(get(h, "max-connection-age")?)?,
        max_connection_age_grace: to_duration(get(h, "max-connection-age-grace")?)?,
    })
}

fn storage_from_hocon(h: &Hocon) -> Result<Storage, String> {
    Ok(Storage {
        data_dir: to_path(get(h, "data-dir")?)?,
    })
}

fn metrics_from_hocon(h: &Hocon) -> Result<Metrics, String> {
    Ok(Metrics {
        prometheus: to_bool(get(h, "prometheus")?)?,
        influxdb: to_bool(get(h, "influxdb")?)?,
        influxdb_udp: to_bool(get(h, "influxdb-udp")?)?,
        zipkin: to_bool(get(h, "zipkin")?)?,
        sigar: to_bool(get(h, "sigar")?)?,
    })
}

fn dev_conf_from_hocon(h: &Hocon) -> Result<DevConf, String> {
    Ok(DevConf {
        deployer_private_key: to_optional_string(h, "deployer-private-key")?,
    })
}

fn tls_conf_from_hocon(h: &Hocon) -> Result<TlsConf, String> {
    Ok(TlsConf {
        certificate_path: to_path(get(h, "certificate-path")?)?,
        key_path: to_path(get(h, "key-path")?)?,
        secure_random_non_blocking: to_bool(get(h, "secure-random-non-blocking")?)?,
        custom_certificate_location: to_bool(get(h, "custom-certificate-location")?)?,
        custom_key_location: to_bool(get(h, "custom-key-location")?)?,
    })
}

fn genesis_block_data_from_hocon(h: &Hocon) -> Result<GenesisBlockData, String> {
    Ok(GenesisBlockData {
        genesis_data_dir: to_path(get(h, "genesis-data-dir")?)?,
        bonds_file: to_string(get(h, "bonds-file")?)?,
        wallets_file: to_string(get(h, "wallets-file")?)?,
        bond_minimum: to_i64(get(h, "bond-minimum")?)?,
        bond_maximum: to_i64(get(h, "bond-maximum")?)?,
        epoch_length: to_i32(get(h, "epoch-length")?)?,
        quarantine_length: to_i32(get(h, "quarantine-length")?)?,
        genesis_block_number: to_i64(get(h, "genesis-block-number")?)?,
        number_of_active_validators: to_i32(get(h, "number-of-active-validators")?)?,
        pos_multi_sig_public_keys: to_string_list(get(h, "pos-multi-sig-public-keys")?)?,
        pos_multi_sig_quorum: to_i32(get(h, "pos-multi-sig-quorum")?)?,
        pos_vault_pub_key: to_string(get(h, "pos-vault-pub-key")?)?,
        system_contract_pub_key: to_string(get(h, "system-contract-pub-key")?)?,
    })
}

fn casper_conf_from_hocon(h: &Hocon) -> Result<CasperConf, String> {
    Ok(CasperConf {
        validator_public_key: to_optional_string(h, "validator-public-key")?,
        validator_private_key: to_optional_string(h, "validator-private-key")?,
        validator_private_key_path: to_optional_path(h, "validator-private-key-path")?,
        shard_name: to_string(get(h, "shard-name")?)?,
        casper_loop_interval: to_duration(get(h, "casper-loop-interval")?)?,
        requested_blocks_timeout: to_duration(get(h, "requested-blocks-timeout")?)?,
        max_number_of_parents: to_i32(get(h, "max-number-of-parents")?)?,
        fork_choice_stale_threshold: to_duration(get(h, "fork-choice-stale-threshold")?)?,
        fork_choice_check_if_stale_interval: to_duration(
            get(h, "fork-choice-check-if-stale-interval")?,
        )?,
        synchrony_constraint_threshold: to_f64(get(h, "synchrony-constraint-threshold")?)?,
        height_constraint_threshold: to_i64(get(h, "height-constraint-threshold")?)?,
        genesis_block_data: genesis_block_data_from_hocon(get(h, "genesis-block-data")?)?,
        autogen_shard_size: to_i32(get(h, "autogen-shard-size")?)?,
        min_phlo_price: to_i64(get(h, "min-phlo-price")?)?,
    })
}

/// Build a `NodeConf` from a merged `Hocon` tree (port of `mergedConf.load[NodeConf]`).
pub fn node_conf_from_hocon(h: &Hocon) -> Result<NodeConf, String> {
    Ok(NodeConf {
        standalone: to_bool(get(h, "standalone")?)?,
        autopropose: to_bool(get(h, "autopropose")?)?,
        protocol_server: protocol_server_from_hocon(get(h, "protocol-server")?)?,
        protocol_client: protocol_client_from_hocon(get(h, "protocol-client")?)?,
        peers_discovery: peers_discovery_from_hocon(get(h, "peers-discovery")?)?,
        api_server: api_server_from_hocon(get(h, "api-server")?)?,
        tls: tls_conf_from_hocon(get(h, "tls")?)?,
        storage: storage_from_hocon(get(h, "storage")?)?,
        casper: casper_conf_from_hocon(get(h, "casper")?)?,
        metrics: metrics_from_hocon(get(h, "metrics")?)?,
        dev_mode: to_bool(get(h, "dev-mode")?)?,
        dev: dev_conf_from_hocon(get(h, "dev")?)?,
        default_data_dir: to_string(get(h, "default-data-dir")?)?,
    })
}
