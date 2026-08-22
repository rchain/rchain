//! End-to-end consensus-pipeline integration tests (genesis → block → replay).

mod common;

use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::casper::protocol::casper_message::{DeployData, ProcessedDeploy, SignedDeployData};
use rchain_rholang::system_processes::BlockData;

use common::build_runtime_manager;

fn fixed_rand() -> Blake2b512Random {
    Blake2b512Random::from_init(&[0u8; 32])
}

/// A minimal signed deploy with the given term (signature verification is deferred to the
/// deploy-acceptance path, so the sig/deployer fields are left empty here).
fn deploy(term: &str) -> SignedDeployData {
    deploy_with_limit(term, 90_000)
}

fn deploy_with_limit(term: &str, limit: i64) -> SignedDeployData {
    SignedDeployData {
        data: DeployData {
            term: term.to_string(),
            timestamp: 0,
            phlo_price: 1,
            phlo_limit: limit,
            valid_after_block_number: 0,
            shard_id: "root".to_string(),
        },
        deployer: vec![0u8; 32],
        sig: Vec::new(),
        sig_algorithm: "secp256k1".to_string(),
    }
}

#[tokio::test]
async fn genesis_deploy_replay_recomputes_state() {
    let rm = build_runtime_manager().await;
    let rand = fixed_rand();
    let (pre, post, results) = rm
        .compute_genesis(
            &[deploy(r#"@"chan"!(42)"#)],
            &rand,
            BlockData::empty(),
            &std::collections::BTreeMap::new(),
        )
        .await
        .expect("compute_genesis");
    assert_eq!(results.len(), 1);
    assert!(results[0].eval_result.succeeded(), "deploy should succeed");

    // Law 11: replay recomputes the same post-state hash from the recorded log.
    let processed: Vec<ProcessedDeploy> = results.iter().map(|r| r.deploy.clone()).collect();
    let (replay_post, _) = rm
        .replay_compute_state(
            &pre,
            &processed,
            &[],
            &rand,
            BlockData::empty(),
            false,
            &std::collections::BTreeMap::new(),
        )
        .await
        .expect("replay_compute_state");
    assert_eq!(post, replay_post, "replay must reproduce the play post-state");
}

#[tokio::test]
async fn empty_state_hash_fixed_matches_runtime() {
    let rm = build_runtime_manager().await;
    let hash = rm.runtime().empty_state_hash().await.expect("empty state hash");
    assert_eq!(
        hash,
        rchain_casper::interpreter_util::empty_state_hash_fixed(),
        "the hard-coded genesis pre-state hash must match the computed empty state"
    );
}

#[tokio::test]
async fn deploy_exceeding_phlo_limit_fails_and_next_runs() {
    let rm = build_runtime_manager().await;
    let rand = fixed_rand();
    let starving = deploy_with_limit(r#"@"chan"!(42)"#, 1);
    let normal = deploy(r#"@"chan2"!(43)"#);

    let (_, _, results) = rm
        .compute_genesis(
            &[starving, normal],
            &rand,
            BlockData::empty(),
            &std::collections::BTreeMap::new(),
        )
        .await
        .expect("compute_genesis");

    assert!(results[0].deploy.is_failed, "phlo-exhausted deploy must be failed");
    assert!(
        results[0]
            .eval_result
            .errors
            .iter()
            .any(|e| matches!(e, rchain_rholang::errors::RholangError::OutOfPhlogistonsError)),
        "failure must be an OutOfPhlogistonsError"
    );

    // The next deploy still runs: the per-deploy phlo `set` resets the balance.
    assert!(!results[1].deploy.is_failed, "subsequent deploy must succeed");
}

#[tokio::test]
async fn replay_matches_play_for_persistent_and_peek() {
    let rm = build_runtime_manager().await;
    let rand = fixed_rand();
    // A non-trivial deploy: persistent send + peek receive (Law 11 replay must reproduce the play
    // post-state, not just a single trivial send).
    let term = r#"new c in { c!!(42) | for (@x <<- c) { @"out"!(x) } }"#;
    let (pre, post, results) = rm
        .compute_genesis(
            &[deploy(term)],
            &rand,
            BlockData::empty(),
            &std::collections::BTreeMap::new(),
        )
        .await
        .expect("compute_genesis");
    assert!(results[0].eval_result.succeeded(), "deploy should succeed");

    let processed: Vec<ProcessedDeploy> = results.iter().map(|r| r.deploy.clone()).collect();
    let (replay_post, _) = rm
        .replay_compute_state(
            &pre,
            &processed,
            &[],
            &rand,
            BlockData::empty(),
            false,
            &std::collections::BTreeMap::new(),
        )
        .await
        .expect("replay_compute_state");
    assert_eq!(post, replay_post, "replay must reproduce the play post-state");
}
