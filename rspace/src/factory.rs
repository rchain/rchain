//! Store → RSpace construction (port of `RSpace.createHistoryRepo`/`createWithReplay` and
//! `ReportingRspace.create`).

use std::sync::Arc;

use rchain_shared::serialize::Serialize;
use rchain_shared::store_manager::{database, KeyValueStoreManager};
use rchain_shared::typed_store::BytesCodec;

use crate::history::codecs::Blake2b256HashCodec;
use crate::history::cold_store::PersistedDataCodec;
use crate::history::history_repository::HistoryRepository;
use crate::history::instances::radix_history::RadixHistory;
use crate::history::root_repository::RootRepository;
use crate::history::roots_store::RootsStore;
use crate::hot_store::InMemHotStore;
use crate::match_::Match;
use crate::replay_rspace::ReplayRSpace;
use crate::reporting_rspace::ReportingRspace;
use crate::rspace::RSpace;

/// Create a history repository over the three named stores (port of
/// `HistoryRepositoryInstances.lmdbRepository`). `prefix` separates store sets — `"rspace"` for the
/// node's block state, `"eval"` for the REPL's isolated evaluation state.
pub async fn create_history_repository<C, P, A, K>(
    manager: &dyn KeyValueStoreManager,
    prefix: &str,
) -> Result<Arc<HistoryRepository<C, P, A, K>>, String> {
    let history_store = database(
        manager,
        &format!("{prefix}-history"),
        Arc::new(Blake2b256HashCodec),
        Arc::new(BytesCodec),
    )
    .await?;
    let roots_store = manager.store(&format!("{prefix}-roots")).await?;
    let cold_store = database(
        manager,
        &format!("{prefix}-cold"),
        Arc::new(Blake2b256HashCodec),
        Arc::new(PersistedDataCodec),
    )
    .await?;

    let roots_repo = Arc::new(RootRepository::new(RootsStore::new(roots_store)));
    let root = roots_repo.current_root().await?;
    let history = RadixHistory::new(root, Arc::new(history_store)).await;

    Ok(Arc::new(HistoryRepository::new(
        history,
        roots_repo,
        Arc::new(cold_store),
    )))
}

/// Create a play space from the store + matcher (port of `RSpace.createWithReplay`, play half).
pub async fn create_rspace<C, P, A, K>(
    manager: &dyn KeyValueStoreManager,
    matcher: Arc<dyn Match<P, A>>,
) -> Result<Arc<RSpace<C, P, A, K>>, String>
where
    C: Ord + Clone + Serialize<C> + Send + Sync + 'static,
    P: Clone + Serialize<P> + Send + Sync + 'static,
    A: Clone + Serialize<A> + Send + Sync + 'static,
    K: Clone + Serialize<K> + Send + Sync + 'static,
{
    let history_repo = create_history_repository::<C, P, A, K>(manager, "rspace").await?;
    let reader = history_repo.get_history_reader(history_repo.root()).await;
    let hot_store = Arc::new(InMemHotStore::new(reader.base()));
    let (play, _replay) = RSpace::create_with_replay(history_repo, hot_store, matcher);
    Ok(play)
}

/// Create a replay space from the store + matcher (port of `RSpace.createWithReplay`).
pub async fn create_replay_rspace<C, P, A, K>(
    manager: &dyn KeyValueStoreManager,
    matcher: Arc<dyn Match<P, A>>,
) -> Result<ReplayRSpace<C, P, A, K>, String>
where
    C: Ord + Clone + Serialize<C> + Send + Sync + 'static,
    P: Clone + Serialize<P> + Send + Sync + 'static,
    A: Clone + Serialize<A> + Send + Sync + 'static,
    K: Clone + Serialize<K> + Send + Sync + 'static,
{
    let history_repo = create_history_repository::<C, P, A, K>(manager, "rspace").await?;
    let reader = history_repo.get_history_reader(history_repo.root()).await;
    let hot_store = Arc::new(InMemHotStore::new(reader.base()));
    let (_play, replay) = RSpace::create_with_replay(history_repo, hot_store, matcher);
    Ok(replay)
}

/// Create a reporting space from the store + matcher (port of `ReportingRspace.create`).
pub async fn create_reporting_rspace<C, P, A, K>(
    manager: &dyn KeyValueStoreManager,
    matcher: Arc<dyn Match<P, A>>,
) -> Result<ReportingRspace<C, P, A, K>, String>
where
    C: Ord + Clone + Serialize<C> + Send + Sync + 'static,
    P: Clone + Serialize<P> + Send + Sync + 'static,
    A: Clone + Serialize<A> + Send + Sync + 'static,
    K: Clone + Serialize<K> + Send + Sync + 'static,
{
    let replay = create_replay_rspace(manager, matcher).await?;
    Ok(ReportingRspace::new(Arc::new(replay)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::store_manager::InMemoryStoreManager;

    struct StrMatch;
    impl Match<String, String> for StrMatch {
        fn get(&self, _p: &String, a: &String) -> Option<String> {
            Some(a.clone())
        }
    }

    #[tokio::test]
    async fn creates_reporting_rspace_from_in_memory_store() {
        let manager = InMemoryStoreManager::default();
        let space = create_reporting_rspace::<String, String, String, String>(
            &manager,
            Arc::new(StrMatch),
        )
        .await
        .unwrap();
        assert!(space.get_report().is_empty());
    }
}
