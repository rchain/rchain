//! Shared harness for the rholang execution-pipeline integration tests.

use std::sync::Arc;

use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_rholang::runtime::{RhoRuntime, ReplayRhoRuntime};
use rchain_rholang::storage::RhoMatch;
use rchain_rspace::factory::create_history_repository;
use rchain_rspace::hot_store::InMemHotStore;
use rchain_rspace::rspace::RSpace;
use rchain_shared::store_manager::InMemoryStoreManager;

/// Assemble a play + replay runtime pair over a fresh in-memory store.
pub async fn build_runtime_pair() -> (RhoRuntime, ReplayRhoRuntime) {
    let manager = InMemoryStoreManager::default();
    let history =
        create_history_repository::<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation>(
            &manager,
            "rspace",
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

/// Look up a committed golden hex vector for `case` in `testdata/differential/<target>.tsv`.
pub fn load_golden(case: &str, target: &str) -> Option<String> {
    let path = format!(
        "{}/testdata/differential/{target}.tsv",
        env!("CARGO_MANIFEST_DIR")
    );
    let contents = std::fs::read_to_string(path).ok()?;
    contents.lines().find_map(|line| {
        let (id, hex) = line.split_once('\t')?;
        (id == case).then(|| hex.to_string())
    })
}
