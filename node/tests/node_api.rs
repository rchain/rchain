//! Node-level integration tests: a real in-process standalone node driven over HTTP.
//!
//! Mirrors the Scala `integration-tests` suite's single-node scenarios (`test_genesis_ceremony`
//! slice, `test_web_api`).

mod common;

use std::time::Duration;

use serde_json::Value;

use common::{free_ports, standalone_conf, start, temp_dir, VALIDATOR_PRIV_HEX};

async fn poll_blocks(client: &reqwest::Client, url: &str) -> Vec<Value> {
    let deadline = tokio::time::Instant::now() + Duration::from_secs(30);
    loop {
        match client.get(url).send().await {
            Ok(resp) if resp.status().is_success() => match resp.json::<Value>().await {
                Ok(Value::Array(a)) if !a.is_empty() => return a,
                Ok(_) => {}
                Err(_) => {}
            },
            _ => {}
        }
        assert!(
            tokio::time::Instant::now() < deadline,
            "blocks never appeared: {url}"
        );
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
}

/// Scala `test_web_api` (read-only surface): the HTTP server comes up and serves the
/// genesis-independent endpoints (`/version`, `/status`, `/api/status`) without a validator or a
/// genesis block.
#[tokio::test]
async fn http_surface_without_genesis() {
    let dir = temp_dir("http-surface");
    let ports = free_ports(4);
    let http_port = ports[0];
    let conf = standalone_conf(&dir, &ports, None);
    let node = start(&conf, ports[2], http_port).await;
    let base = format!("http://127.0.0.1:{http_port}");
    let client = reqwest::Client::new();

    let deadline = tokio::time::Instant::now() + Duration::from_secs(15);
    let version = loop {
        if let Ok(resp) = client.get(format!("{base}/version")).send().await {
            if resp.status().is_success() {
                break resp.text().await.unwrap();
            }
        }
        assert!(tokio::time::Instant::now() < deadline, "HTTP server did not come up");
        tokio::time::sleep(Duration::from_millis(100)).await;
    };
    assert!(version.contains("RChain Node"), "version = {version}");

    // `/api/status` returns a JSON status document.
    let status = client
        .get(format!("{base}/api/status"))
        .send()
        .await
        .unwrap()
        .json::<Value>()
        .await
        .unwrap();
    assert!(status.get("networkId").is_some(), "api/status = {status}");

    node.shutdown();
    let _ = std::fs::remove_dir_all(&dir);
}

/// Scala `test_genesis_ceremony` (single-node slice) + `test_web_api`: a standalone validator node
/// boots, creates the genesis block, and exposes it over the HTTP API.
#[test]
fn genesis_boot_exposes_block_over_http() {
    common::test_runtime().block_on(async {
    let dir = temp_dir("genesis-boot");
    let ports = free_ports(4);
    let http_port = ports[0];
    let conf = standalone_conf(&dir, &ports, Some(VALIDATOR_PRIV_HEX));
    let node = start(&conf, ports[2], http_port).await;
    let base = format!("http://127.0.0.1:{http_port}");
    let client = reqwest::Client::new();

    // The HTTP server comes up before genesis is created; poll until `/version` responds.
    let deadline = tokio::time::Instant::now() + Duration::from_secs(15);
    let version = loop {
        if let Ok(resp) = client.get(format!("{base}/version")).send().await {
            if resp.status().is_success() {
                break resp.text().await.unwrap();
            }
        }
        assert!(tokio::time::Instant::now() < deadline, "HTTP server did not come up");
        tokio::time::sleep(Duration::from_millis(100)).await;
    };
    assert!(version.contains("RChain Node"), "version = {version}");

    // `GET /api/blocks` returns the genesis block once the genesis ceremony completes.
    let blocks = poll_blocks(&client, &format!("{base}/api/blocks")).await;

    let genesis = &blocks[0];
    // The genesis block has no justifications.
    assert_eq!(genesis["justifications"].as_array().unwrap().len(), 0);
    // ... and carries the single bonded validator with stake 100.
    let bonds = genesis["bonds"].as_array().unwrap();
    assert_eq!(bonds.len(), 1);
    assert_eq!(bonds[0]["stake"], 100);

    node.shutdown();
    let _ = std::fs::remove_dir_all(&dir);
    });
}
