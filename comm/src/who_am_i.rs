//! External-IP discovery (port of `comm/WhoAmI.scala`).
//!
//! The cats-effect `F[_]` and `Log[F]` are simplified to synchronous calls: logging is a
//! `FnMut(String)` callback and the HTTP fetch is a minimal HTTP/1.0 GET over `TcpStream` (the two
//! services are plain `http://`). `retrieveExternalAddress` delegates to `UPnP.assurePortForwarding`.

use std::io::{Read, Write};
use std::net::{IpAddr, TcpStream};

use rchain_shared::refined::Port;

use crate::peer_node::{NodeIdentifier, PeerNode};

const AMAZON: &str = "http://checkip.amazonaws.com";
const WHAT_IS_MY_IP: &str = "http://bot.whatismyipaddress.com";

/// Fetch a local peer node, guessing the external IP when `host` is absent (port of
/// `fetchLocalPeerNode`).
pub fn fetch_local_peer_node(
    host: Option<String>,
    protocol_port: Port,
    discovery_port: Port,
    no_upnp: bool,
    id: NodeIdentifier,
    log: &mut dyn FnMut(String),
) -> PeerNode {
    let external = retrieve_external_address(
        no_upnp,
        &[
            i32::from(u16::from(protocol_port)),
            i32::from(u16::from(discovery_port)),
        ],
        log,
    );
    let host = fetch_host(host, external.as_deref(), log);
    PeerNode::from(id, host, protocol_port, discovery_port)
}

/// Re-check the local peer node, returning a new one if the external IP changed (port of
/// `checkLocalPeerNode`).
pub fn check_local_peer_node(
    protocol_port: Port,
    discovery_port: Port,
    peer_node: PeerNode,
    log: &mut dyn FnMut(String),
) -> Option<PeerNode> {
    let (_, a) = check_all(None, &check_from_real);
    if a == peer_node.endpoint.host {
        None
    } else {
        log(format!("external IP address has changed to {a}"));
        Some(PeerNode::from(peer_node.id, a, protocol_port, discovery_port))
    }
}

fn fetch_host(host: Option<String>, external: Option<&str>, log: &mut dyn FnMut(String)) -> String {
    match host {
        Some(h) => h,
        None => who_am_i(external, log),
    }
}

/// Open ports via UPnP and return the gateway external IP (port of `retrieveExternalAddress`).
fn retrieve_external_address(
    no_upnp: bool,
    ports: &[i32],
    log: &mut dyn FnMut(String),
) -> Option<String> {
    if no_upnp {
        None
    } else {
        let devices = crate::upnp::discover();
        crate::upnp::assure_port_forwarding(ports, devices, log)
    }
}

fn who_am_i(external: Option<&str>, log: &mut dyn FnMut(String)) -> String {
    log("flag --host was not provided, guessing your external IP address".to_string());
    let (s, a) = check_all(external, &check_from_real);
    log(format!("guessed {a} from source: {s}"));
    a
}

fn check_all(external: Option<&str>, fetch: &dyn Fn(&str) -> Option<String>) -> (String, String) {
    let r1 = check("AmazonAWS service", AMAZON, fetch);
    let r2 = check_next(r1, || check("WhatIsMyIP service", WHAT_IS_MY_IP, fetch));
    let r3 = check_next(r2, || upnp_ip_check(external));
    let r4 = check_next(r3, || ("failed to guess".to_string(), Some("localhost".to_string())));
    let (s, a) = r4;
    (s, a.unwrap_or_default())
}

fn check(
    source: &str,
    from: &str,
    fetch: &dyn Fn(&str) -> Option<String>,
) -> (String, Option<String>) {
    (source.to_string(), fetch(from))
}

fn check_next(
    prev: (String, Option<String>),
    next: impl FnOnce() -> (String, Option<String>),
) -> (String, Option<String>) {
    if prev.1.is_some() {
        prev
    } else {
        next()
    }
}

fn upnp_ip_check(external: Option<&str>) -> (String, Option<String>) {
    ("UPnP".to_string(), external.map(canonical_ip))
}

/// `InetAddress.getByName(_).getHostAddress` for a literal IP (identity, canonicalized).
fn canonical_ip(ip: &str) -> String {
    ip.parse::<IpAddr>()
        .map(|a| a.to_string())
        .unwrap_or_else(|_| ip.to_string())
}

/// Minimal HTTP/1.0 GET returning the first body line parsed as an IP (port of `checkFrom`).
fn check_from_real(from: &str) -> Option<String> {
    let rest = from
        .strip_prefix("http://")
        .or_else(|| from.strip_prefix("https://"))?;
    let (authority, path) = rest.split_once('/').unwrap_or((rest, ""));
    let (host, port) = match authority.split_once(':') {
        Some((h, p)) => (h, p.parse::<u16>().ok()?),
        None => (authority, 80),
    };
    let mut stream = TcpStream::connect((host, port)).ok()?;
    let request = format!("GET /{path} HTTP/1.0\r\nHost: {host}\r\nConnection: close\r\n\r\n");
    stream.write_all(request.as_bytes()).ok()?;
    let mut response = Vec::new();
    stream.read_to_end(&mut response).ok()?;
    let response = String::from_utf8_lossy(&response);
    let body = response.split("\r\n\r\n").nth(1)?;
    let ip = body.lines().next()?.trim();
    ip.parse::<IpAddr>().ok().map(|a| a.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_next_uses_prev_when_present() {
        let prev = ("a".to_string(), Some("1".to_string()));
        let next = check_next(prev.clone(), || ("b".to_string(), Some("2".to_string())));
        assert_eq!(next, prev);
    }

    #[test]
    fn check_next_uses_next_when_absent() {
        let next = check_next(("a".to_string(), None), || {
            ("b".to_string(), Some("2".to_string()))
        });
        assert_eq!(next, ("b".to_string(), Some("2".to_string())));
    }

    #[test]
    fn upnp_ip_check_canonicalizes() {
        assert_eq!(
            upnp_ip_check(Some("8.8.8.8")),
            ("UPnP".to_string(), Some("8.8.8.8".to_string()))
        );
        assert_eq!(upnp_ip_check(None), ("UPnP".to_string(), None));
    }

    #[test]
    fn check_all_uses_first_successful_source() {
        let fetch = |from: &str| {
            if from.contains("whatismyipaddress") {
                Some("1.2.3.4".to_string())
            } else {
                None
            }
        };
        let (source, ip) = check_all(None, &fetch);
        assert_eq!(source, "WhatIsMyIP service");
        assert_eq!(ip, "1.2.3.4");
    }

    #[test]
    fn check_all_falls_back_to_localhost() {
        let fetch = |_from: &str| None;
        let (source, ip) = check_all(None, &fetch);
        assert_eq!(source, "failed to guess");
        assert_eq!(ip, "localhost");
    }

    #[test]
    fn fetch_local_peer_node_uses_provided_host() {
        let mut logs = Vec::new();
        let peer = fetch_local_peer_node(
            Some("example.com".to_string()),
            Port::new(40400),
            Port::new(40404),
            true,
            NodeIdentifier::new(vec![1, 2, 3]),
            &mut |msg| logs.push(msg),
        );
        assert_eq!(peer.endpoint.host, "example.com");
        assert_eq!(peer.endpoint.tcp_port, Port::new(40400));
        assert_eq!(peer.endpoint.udp_port, Port::new(40404));
        assert!(logs.is_empty());
    }
}
