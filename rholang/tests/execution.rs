//! End-to-end execution-pipeline integration tests (parse → normalize → reduce → rspace → hash).
//!
//! These assemble a real `RhoRuntime` + `ReplayRhoRuntime` over an in-memory store and drive a
//! deploy through the full stack, pinning post-state hashes against committed golden vectors and
//! asserting the Scala-independent `replay == play` determinism invariant (Law 11).

mod common;

use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::ast::Expr;
use rchain_models::par_ops::from_expr;
use rchain_models::sorted::SortedProc;
use rchain_models::types::Closed;
use rchain_rholang::env::Env;
use rchain_rholang::registry::registry_bootstrap_ast;
use rchain_rspace::history::history::empty_root_hash_value;

use common::{build_runtime_pair, load_golden};

/// A fixed, deterministic random seed so post-state hashes are reproducible.
fn fixed_rand() -> Blake2b512Random {
    Blake2b512Random::from_init(&[0u8; 32])
}

fn chan(name: &str) -> SortedProc {
    SortedProc::new(from_expr(Expr::GString(name.to_string())))
}

/// Assert `hash` matches the committed golden vector for `case`.
fn assert_state_hash(case: &str, hash: &[u8]) {
    let want = load_golden(case, "execution").unwrap_or_else(|| panic!("missing golden case {case}"));
    assert_eq!(rchain_shared::base16::encode(hash), want, "golden mismatch for {case}");
}

#[tokio::test]
async fn execute_deploy_produces_datum() {
    let (rt, _replay) = build_runtime_pair().await;
    let res = rt.evaluate(r#"@"chan"!(42)"#, &fixed_rand()).await.expect("evaluate");
    assert!(res.succeeded(), "unexpected errors: {:?}", res.errors);
    let data = rt.get_data_par(&chan("chan")).await.expect("get_data_par");
    assert_eq!(data, vec![from_expr(Expr::GInt(42))]);
}

#[tokio::test]
async fn execute_deploy_state_hash_is_deterministic() {
    let (a, _) = build_runtime_pair().await;
    let (b, _) = build_runtime_pair().await;
    a.evaluate(r#"@"chan"!(42)"#, &fixed_rand()).await.unwrap();
    b.evaluate(r#"@"chan"!(42)"#, &fixed_rand()).await.unwrap();
    let ha = a.create_checkpoint().await.unwrap().root;
    let hb = b.create_checkpoint().await.unwrap().root;
    assert_eq!(ha, hb);
    assert_state_hash("exec_deploy_42", ha.as_bytes());
}

#[tokio::test]
async fn replay_matches_play() {
    let (rt, rrt) = build_runtime_pair().await;
    let rand = fixed_rand();
    rt.evaluate(r#"@"chan"!(42)"#, &rand).await.unwrap();
    let cp = rt.create_checkpoint().await.unwrap();

    // Replay from the empty pre-state against the recorded play log.
    rrt.reset(empty_root_hash_value()).await.unwrap();
    rrt.rig(cp.log.clone()).await;
    rrt.evaluate(r#"@"chan"!(42)"#, &rand).await.unwrap();
    rrt.check_replay_data().await.expect("replay data consistent");

    let replay_cp = rrt.create_checkpoint().await.unwrap();
    assert_eq!(cp.root, replay_cp.root);
    assert_state_hash("replay_deploy_42", replay_cp.root.as_bytes());
}

#[tokio::test]
async fn empty_state_bootstrap_is_deterministic() {
    let (rt, _replay) = build_runtime_pair().await;
    rt.reset(empty_root_hash_value()).await.unwrap();
    let bootstrap = Closed::new(registry_bootstrap_ast()).expect("registry bootstrap is closed");
    rt.inj(&bootstrap, &Env::new(), &fixed_rand())
        .await
        .unwrap();
    let root = rt.create_checkpoint().await.unwrap().root;
    assert_state_hash("empty_state", root.as_bytes());
}

#[tokio::test]
async fn failing_deploy_is_captured_not_propagated() {
    let (rt, _replay) = build_runtime_pair().await;
    // `1 + "a"` is a well-formed term whose reduction is a type error (Int + String).
    let res = rt
        .evaluate(r#"@"chan"!(1 + "a")"#, &fixed_rand())
        .await
        .expect("evaluate returns Ok");
    assert!(res.failed(), "expected a captured failure");
    assert!(!res.errors.is_empty());
    // The post-state is still checkpointable.
    rt.create_checkpoint().await.expect("checkpoint after failed deploy");
}

#[tokio::test]
async fn peek_and_persistent_work() {
    let (rt, _) = build_runtime_pair().await;
    let rand = fixed_rand();

    // Peek (`<<-`): read without consuming.
    rt.evaluate(r#"new c in { c!(42) | for (@x <<- c) { @"peek"!(x) } }"#, &rand)
        .await
        .unwrap();
    assert_eq!(
        rt.get_data_par(&chan("peek")).await.unwrap(),
        vec![from_expr(Expr::GInt(42))]
    );

    // Persistent send (`!!`): datum stays across two consumes.
    rt.evaluate(r#"new c in { c!!(42) | for (@x <- c) { @"p1"!(x) } | for (@y <- c) { @"p2"!(y) } }"#, &rand)
        .await
        .unwrap();
    assert_eq!(rt.get_data_par(&chan("p1")).await.unwrap(), vec![from_expr(Expr::GInt(42))]);
    assert_eq!(rt.get_data_par(&chan("p2")).await.unwrap(), vec![from_expr(Expr::GInt(42))]);
}

#[tokio::test]
async fn list_channel_matches() {
    let (rt, _) = build_runtime_pair().await;
    let rand = fixed_rand();

    // The blessed `MakeNode` shape: a contract binds `@node` (a PROC var) and sends on the
    // list-as-channel `@[node, *storeToken]`; a receive on `@["key", *storeToken]` must see it.
    let r = rt
        .evaluate(r#"new storeToken, Make in { contract Make(@initVal, @node) = { @[node, *storeToken]!(initVal) } | Make!(7, "key") | for (@x <- @["key", *storeToken]) { @"listch"!(x) } }"#, &rand)
        .await
        .unwrap();
    assert!(r.succeeded(), "list-as-channel term errors: {:?}", r.errors);
    assert_eq!(
        rt.get_data_par(&chan("listch")).await.unwrap(),
        vec![from_expr(Expr::GInt(7))]
    );
}
