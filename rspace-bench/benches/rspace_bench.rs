//! Criterion benchmarks for the RSpace / rholang reduce path (port of `legacy/rspace-bench`).
//!
//! Ports `EvalBench` (the `mvcepp.rho` reduce path), `WideBench` (`wide-setup.rho` + `wide.rho`),
//! `KeyBench` (`Blake2b256Hash` key encoding), and raw `RSpaceBench`/`ReplayRSpaceBench`
//! (produce/consume on the play/replay spaces). The Scala `AddressBookExample` toy type has no Rust
//! equivalent; the raw benches use the real `Par`/`BindPattern`/`ListParWithRandom`/
//! `TaggedContinuation` types instead.

use std::collections::BTreeSet;
use std::sync::Arc;

use criterion::{criterion_group, criterion_main, Criterion};

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::ast::{Expr, Par, Var};
use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_rholang::runtime::{ReplayRhoRuntime, RhoRuntime};
use rchain_rholang::storage::RhoMatch;
use rchain_rspace::factory::create_history_repository;
use rchain_rspace::hot_store::InMemHotStore;
use rchain_rspace::rspace::RSpace;
use rchain_rspace::tuple_space::Tuplespace;
use rchain_shared::store_manager::InMemoryStoreManager;

const MVCEPP: &str = include_str!("resources/mvcepp.rho");
const WIDE: &str = include_str!("resources/wide.rho");
const WIDE_SETUP: &str = include_str!("resources/wide-setup.rho");

/// Assemble a play + replay runtime pair over a fresh in-memory store (mirror of
/// `rholang/tests/common/mod.rs::build_runtime_pair`).
async fn build_runtime_pair() -> (RhoRuntime, ReplayRhoRuntime) {
    let manager = InMemoryStoreManager::default();
    let history =
        create_history_repository::<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>(
            &manager, "rspace",
        )
        .await
        .expect("history repository");
    let reader = history.get_history_reader(history.root()).await;
    let hot = Arc::new(InMemHotStore::new(reader.base()));
    let (play, replay) = RSpace::create_with_replay(history.clone(), hot, Arc::new(RhoMatch));
    let rho = RhoRuntime::create(play, history.clone(), SortedProc::default())
        .await
        .expect("rho runtime");
    let replay = ReplayRhoRuntime::create(Arc::new(replay), history, SortedProc::default())
        .await
        .expect("replay runtime");
    (rho, replay)
}

fn eval_bench(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().expect("tokio runtime");
    let (runtime, _replay) = rt.block_on(build_runtime_pair());
    let rand = Blake2b512Random::default_random();

    c.bench_function("eval/mvcepp", |b| {
        b.iter(|| {
            rt.block_on(async {
                runtime.evaluate(MVCEPP, &rand).await.expect("mvcepp reduce");
            })
        })
    });
}

fn wide_bench(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().expect("tokio runtime");
    let rand = Blake2b512Random::default_random();

    c.bench_function("eval/wide", |b| {
        b.iter(|| {
            rt.block_on(async {
                let (runtime, _replay) = build_runtime_pair().await;
                runtime.evaluate(WIDE_SETUP, &rand).await.expect("wide setup");
                runtime.evaluate(WIDE, &rand).await.expect("wide reduce");
            })
        })
    });
}

fn key_bench(c: &mut Criterion) {
    let hashes: Vec<Blake2b256Hash> =
        (0..1001).map(|i| Blake2b256Hash::create(&[i as u8; 32])).collect();

    c.bench_function("key/prepare_codec", |b| {
        b.iter(|| {
            for h in &hashes {
                let _ = h.to_byte_array();
            }
        })
    });

    c.bench_function("key/prepare_raw", |b| {
        b.iter(|| {
            for h in &hashes {
                let _ = h.as_bytes();
            }
        })
    });
}

fn channel(s: &str) -> SortedProc {
    SortedProc::new(Par {
        exprs: vec![Expr::GString(s.to_string())],
        ..Default::default()
    })
}

fn wildcard() -> Par {
    Par {
        exprs: vec![Expr::EVar(Box::new(Var::Wildcard))],
        ..Default::default()
    }
}

fn datum() -> ListParWithRandom {
    ListParWithRandom {
        pars: vec![SortedProc::default()],
        random_state: Blake2b512Random::default_random(),
    }
}

fn pattern() -> BindPattern {
    BindPattern {
        patterns: vec![SortedProc::new(wildcard())],
        remainder: None,
        free_count: 0,
    }
}

fn rspace_bench(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().expect("tokio runtime");
    let (runtime, _replay) = rt.block_on(build_runtime_pair());
    let space = runtime.space().clone();
    let ch = channel("friends#1");

    c.bench_function("rspace/consume", |b| {
        b.iter(|| {
            rt.block_on(async {
                space
                    .consume(
                        &[ch.clone()],
                        &[pattern()],
                        TaggedContinuation::Empty,
                        true,
                        BTreeSet::new(),
                    )
                    .await
                    .expect("consume");
            })
        })
    });

    c.bench_function("rspace/produce", |b| {
        b.iter(|| {
            rt.block_on(async {
                space
                    .produce(ch.clone(), datum(), false)
                    .await
                    .expect("produce");
            })
        })
    });
}

fn replay_rspace_bench(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().expect("tokio runtime");
    let (_runtime, replay) = rt.block_on(build_runtime_pair());
    let space = replay.space().clone();
    let ch = channel("consume");

    c.bench_function("replay/consume", |b| {
        b.iter(|| {
            rt.block_on(async {
                space
                    .consume(
                        &[ch.clone()],
                        &[pattern()],
                        TaggedContinuation::Empty,
                        true,
                        BTreeSet::new(),
                    )
                    .await
                    .expect("replay consume");
            })
        })
    });

    c.bench_function("replay/produce", |b| {
        b.iter(|| {
            rt.block_on(async {
                space
                    .produce(ch.clone(), datum(), false)
                    .await
                    .expect("replay produce");
            })
        })
    });
}

criterion_group!(eval, eval_bench);
criterion_group!(wide, wide_bench);
criterion_group!(key, key_bench);
criterion_group!(rspace, rspace_bench);
criterion_group!(replay, replay_rspace_bench);
criterion_main!(eval, wide, key, rspace, replay);
