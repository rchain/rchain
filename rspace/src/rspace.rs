//! The "play" tuple space: consume/produce/install + checkpoint.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/{RSpaceOps,RSpace}.scala`.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, RwLock};

use async_trait::async_trait;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::serialize::Serialize;

use crate::checkpoint::{Checkpoint, SoftCheckpoint};
use crate::concurrent::two_step_lock::TwoStepLock;
use crate::hashing::stable_hash_provider::hash_channel;
use crate::history::history_repository::HistoryRepository;
use crate::hot_store::{HotStore, InMemHotStore};
use crate::i_space::ISpace;
use crate::internal::{ConsumeCandidate, Datum, Install, ProduceCandidate, Row, WaitingContinuation};
use crate::match_::Match;
use crate::native_store::{InMemNativeStore, NativeStoreState};
use crate::replay_rspace::ReplayRSpace;
use crate::space_matcher::{extract_data_candidates, extract_first_match};
use crate::trace::event::{Comm, Consume, Event, Produce};
use crate::trace::Log;
use crate::tuple_space::{ContResult, Result, Tuplespace};

type MaybeActionResult<C, P, A, K> =
    std::result::Result<Option<(ContResult<C, P, K>, Vec<Result<C, A>>)>, crate::errors::RSpaceError>;
type MaybeProduceCandidate<C, P, A, K> =
    std::result::Result<Option<ProduceCandidate<C, P, A, K>>, crate::errors::RSpaceError>;

/// The play tuple space (port of `RSpace` / `RSpaceOps`).
pub struct RSpace<C, P, A, K> {
    history_repository: RwLock<Arc<HistoryRepository<C, P, A, K>>>,
    store: RwLock<Arc<dyn HotStore<C, P, A, K>>>,
    event_log: RwLock<Log>,
    produce_counter: RwLock<BTreeMap<Produce, usize>>,
    installs: RwLock<BTreeMap<Vec<C>, Install<P, K>>>,
    lock_f: Arc<TwoStepLock<Blake2b256Hash>>,
    matcher: Arc<dyn Match<P, A>>,
    native_store: Arc<InMemNativeStore>,
}

impl<C, P, A, K> RSpace<C, P, A, K>
where
    C: Ord + Clone + Serialize<C> + Send + Sync + 'static,
    P: Clone + Serialize<P> + Send + Sync + 'static,
    A: Clone + Serialize<A> + Send + Sync + 'static,
    K: Clone + Serialize<K> + Send + Sync + 'static,
{
    pub fn new(
        history_repository: Arc<HistoryRepository<C, P, A, K>>,
        store: Arc<dyn HotStore<C, P, A, K>>,
        matcher: Arc<dyn Match<P, A>>,
    ) -> Self {
        RSpace {
            history_repository: RwLock::new(history_repository),
            store: RwLock::new(store),
            event_log: RwLock::new(Vec::new()),
            produce_counter: RwLock::new(BTreeMap::new()),
            installs: RwLock::new(BTreeMap::new()),
            lock_f: Arc::new(TwoStepLock::new()),
            matcher,
            native_store: Arc::new(InMemNativeStore::empty()),
        }
    }

    /// Create both the play and replay spaces over the same store (port of
    /// `RSpace.createWithReplay`).
    pub fn create_with_replay(
        history_repository: Arc<HistoryRepository<C, P, A, K>>,
        store: Arc<dyn HotStore<C, P, A, K>>,
        matcher: Arc<dyn Match<P, A>>,
    ) -> (Arc<RSpace<C, P, A, K>>, ReplayRSpace<C, P, A, K>) {
        let play = Arc::new(RSpace::new(history_repository, store, matcher));
        let replay = ReplayRSpace::new(play.clone());
        (play, replay)
    }

    pub(crate) fn produce_counters(&self, produce_refs: &[Produce]) -> BTreeMap<Produce, usize> {
        let counter = crate::lock::rlock(&self.produce_counter);
        produce_refs
            .iter()
            .map(|p| (p.clone(), *counter.get(p).unwrap_or(&0)))
            .collect()
    }

    pub(crate) fn produce_counter_value(&self, source: &Produce) -> usize {
        crate::lock::rlock(&self.produce_counter)
            .get(source)
            .copied()
            .unwrap_or(0)
    }

    pub(crate) fn increment_produce_counter(&self, produce_ref: &Produce) {
        let mut counter = crate::lock::wlock(&self.produce_counter);
        *counter.entry(produce_ref.clone()).or_insert(0) += 1;
    }

    pub(crate) fn matcher(&self) -> Arc<dyn Match<P, A>> {
        self.matcher.clone()
    }

    /// The native system-contract store (shared with the reducer's system processes).
    pub fn native_store(&self) -> Arc<InMemNativeStore> {
        self.native_store.clone()
    }

    pub(crate) fn current_store(&self) -> Arc<dyn HotStore<C, P, A, K>> {
        crate::lock::rlock(&self.store).clone()
    }

    fn current_history(&self) -> Arc<HistoryRepository<C, P, A, K>> {
        crate::lock::rlock(&self.history_repository)
            .clone()
    }

    fn log_comm(&self, comm: Comm) -> Comm {
        crate::lock::wlock(&self.event_log)
            .insert(0, Event::Comm(comm.clone()));
        comm
    }

    fn log_consume(&self, consume_ref: Consume) -> Consume {
        crate::lock::wlock(&self.event_log)
            .insert(0, Event::Consume(consume_ref.clone()));
        consume_ref
    }

    fn log_produce(&self, produce_ref: Produce, persist: bool) -> Produce {
        crate::lock::wlock(&self.event_log)
            .insert(0, Event::Produce(produce_ref.clone()));
        if !persist {
            let mut counter = crate::lock::wlock(&self.produce_counter);
            *counter.entry(produce_ref.clone()).or_insert(0) += 1;
        }
        produce_ref
    }

    async fn fetch_channel_to_index_data(
        &self,
        channels: &[C],
    ) -> std::result::Result<BTreeMap<C, Vec<(Datum<A>, i64)>>, crate::errors::RSpaceError> {
        let store = self.current_store();
        let mut map = BTreeMap::new();
        for c in channels {
            let data = store.get_data(c).await?;
            let indexed: Vec<(Datum<A>, i64)> =
                data.into_iter().enumerate().map(|(i, d)| (d, i as i64)).collect();
            map.insert(c.clone(), indexed);
        }
        Ok(map)
    }

    pub(crate) async fn store_waiting_continuation(
        &self,
        channels: &[C],
        wc: WaitingContinuation<P, K>,
    ) -> MaybeActionResult<C, P, A, K> {
        let store = self.current_store();
        store.put_continuation(channels, wc).await?;
        for channel in channels {
            store.put_join(channel, channels).await?;
        }
        Ok(None)
    }

    pub(crate) async fn store_data(
        &self,
        channel: &C,
        data: A,
        persist: bool,
        produce_ref: Produce,
    ) -> MaybeActionResult<C, P, A, K> {
        let store = self.current_store();
        store
            .put_datum(
                channel,
                Datum {
                    a: data,
                    persist,
                    source: produce_ref,
                },
            )
            .await?;
        Ok(None)
    }

    pub(crate) async fn store_persistent_data(
        &self,
        data_candidates: &[ConsumeCandidate<C, A>],
    ) -> std::result::Result<(), crate::errors::RSpaceError> {
        let mut sorted = data_candidates.to_vec();
        sorted.sort_by(|a, b| b.datum_index.cmp(&a.datum_index));
        let store = self.current_store();
        for candidate in sorted {
            if !candidate.datum.persist {
                store
                    .remove_datum(&candidate.channel, candidate.datum_index)
                    .await?;
            }
        }
        Ok(())
    }

    pub(crate) async fn remove_matched_datum_and_join(
        &self,
        channels: &[C],
        data_candidates: &[ConsumeCandidate<C, A>],
    ) -> std::result::Result<(), crate::errors::RSpaceError> {
        let mut sorted = data_candidates.to_vec();
        sorted.sort_by(|a, b| b.datum_index.cmp(&a.datum_index));
        let store = self.current_store();
        for candidate in sorted {
            if candidate.datum_index >= 0 && !candidate.datum.persist {
                store
                    .remove_datum(&candidate.channel, candidate.datum_index)
                    .await?;
            }
            store.remove_join(&candidate.channel, channels).await?;
        }
        Ok(())
    }

    pub(crate) fn wrap_result(
        &self,
        channels: &[C],
        wk: &WaitingContinuation<P, K>,
        data_candidates: &[ConsumeCandidate<C, A>],
    ) -> MaybeActionResult<C, P, A, K> {
        Ok(Some((
            ContResult {
                continuation: wk.continuation.clone(),
                persistent: wk.persist,
                channels: channels.to_vec(),
                patterns: wk.patterns.clone(),
                peek: !wk.peeks.is_empty(),
            },
            data_candidates
                .iter()
                .map(|dc| Result {
                    channel: dc.channel.clone(),
                    matched_datum: dc.datum.a.clone(),
                    removed_datum: dc.removed_datum.clone(),
                    persistent: dc.datum.persist,
                })
                .collect(),
        )))
    }

    async fn extract_produce_candidate(
        &self,
        grouped_channels: &[Vec<C>],
        bat_channel: &C,
        data: Datum<A>,
    ) -> MaybeProduceCandidate<C, P, A, K> {
        let store = self.current_store();
        for channels in grouped_channels {
            let conts = store.get_continuations(channels).await?;
            let match_candidates: Vec<(WaitingContinuation<P, K>, usize)> =
                conts.into_iter().enumerate().map(|(i, wc)| (wc, i)).collect();
            let mut channel_to_indexed_data = BTreeMap::new();
            for c in channels {
                let data_list = store.get_data(c).await?;
                let mut indexed: Vec<(Datum<A>, i64)> =
                    data_list.into_iter().enumerate().map(|(i, d)| (d, i as i64)).collect();
                if c == bat_channel {
                    indexed.insert(0, (data.clone(), -1));
                }
                channel_to_indexed_data.insert(c.clone(), indexed);
            }
            if let Some(pc) = extract_first_match(
                channels,
                &match_candidates,
                &channel_to_indexed_data,
                self.matcher.as_ref(),
            ) {
                return Ok(Some(pc));
            }
        }
        Ok(None)
    }

    async fn process_match_found(
        &self,
        pc: ProduceCandidate<C, P, A, K>,
    ) -> MaybeActionResult<C, P, A, K> {
        let ProduceCandidate {
            channels,
            continuation: wk,
            continuation_index,
            data_candidates,
        } = pc;
        let consume_ref = wk.source.clone();
        let comm = Comm::apply(&data_candidates, consume_ref, wk.peeks.clone(), |ps| {
            self.produce_counters(ps)
        });
        self.log_comm(comm);
        if !wk.persist {
            let store = self.current_store();
            store
                .remove_continuation(&channels, continuation_index)
                .await?;
        }
        self.remove_matched_datum_and_join(&channels, &data_candidates)
            .await?;
        self.wrap_result(&channels, &wk, &data_candidates)
    }

    async fn locked_consume(
        &self,
        channels: &[C],
        patterns: &[P],
        continuation: K,
        persist: bool,
        peeks: BTreeSet<usize>,
        consume_ref: Consume,
    ) -> MaybeActionResult<C, P, A, K> {
        self.log_consume(consume_ref.clone());
        let channel_to_indexed_data = self.fetch_channel_to_index_data(channels).await?;
        let options = extract_data_candidates(
            &channels
                .iter()
                .cloned()
                .zip(patterns.iter().cloned())
                .collect::<Vec<_>>(),
            &channel_to_indexed_data,
            self.matcher.as_ref(),
        );
        let wk = WaitingContinuation {
            patterns: patterns.to_vec(),
            continuation,
            persist,
            peeks: peeks.clone(),
            source: consume_ref.clone(),
        };
        match options.into_iter().collect::<Option<Vec<_>>>() {
            None => self.store_waiting_continuation(channels, wk).await,
            Some(data_candidates) => {
                let comm = Comm::apply(&data_candidates, consume_ref, peeks, |ps| {
                    self.produce_counters(ps)
                });
                self.log_comm(comm);
                self.store_persistent_data(&data_candidates).await?;
                self.wrap_result(channels, &wk, &data_candidates)
            }
        }
    }

    async fn locked_produce(
        &self,
        channel: C,
        data: A,
        persist: bool,
        produce_ref: Produce,
    ) -> MaybeActionResult<C, P, A, K> {
        let grouped_channels = self.current_store().get_joins(&channel).await?;
        self.log_produce(produce_ref.clone(), persist);
        let datum = Datum {
            a: data.clone(),
            persist,
            source: produce_ref.clone(),
        };
        let extracted = self
            .extract_produce_candidate(&grouped_channels, &channel, datum)
            .await?;
        match extracted {
            None => self.store_data(&channel, data, persist, produce_ref).await,
            Some(pc) => self.process_match_found(pc).await,
        }
    }

    async fn locked_install(
        &self,
        channels: &[C],
        patterns: &[P],
        continuation: K,
    ) -> std::result::Result<Option<(K, Vec<A>)>, crate::errors::RSpaceError> {
        let store = self.current_store();
        let mut channel_to_indexed_data = BTreeMap::new();
        for c in channels {
            let data = store.get_data(c).await?;
            let indexed: Vec<(Datum<A>, i64)> =
                data.into_iter().enumerate().map(|(i, d)| (d, i as i64)).collect();
            channel_to_indexed_data.insert(c.clone(), indexed);
        }
        let options = extract_data_candidates(
            &channels
                .iter()
                .cloned()
                .zip(patterns.iter().cloned())
                .collect::<Vec<_>>(),
            &channel_to_indexed_data,
            self.matcher.as_ref(),
        );
        match options.into_iter().collect::<Option<Vec<_>>>() {
            None => {
                let consume_ref = Consume::apply(channels, patterns, &continuation, true);
                let wc = WaitingContinuation {
                    patterns: patterns.to_vec(),
                    continuation: continuation.clone(),
                    persist: true,
                    peeks: BTreeSet::new(),
                    source: consume_ref,
                };
                crate::lock::wlock(&self.installs).insert(
                    channels.to_vec(),
                    Install {
                        patterns: patterns.to_vec(),
                        continuation: continuation.clone(),
                    },
                );
                store.install_continuation(channels, wc).await;
                for channel in channels {
                    store.install_join(channel, channels).await;
                }
                Ok(None)
            }
            Some(_) => Err(crate::errors::RSpaceError::InstallNotAllowed),
        }
    }
}

#[async_trait]
impl<C, P, A, K> Tuplespace<C, P, A, K> for RSpace<C, P, A, K>
where
    C: Ord + Clone + Serialize<C> + Send + Sync + 'static,
    P: Clone + Serialize<P> + Send + Sync + 'static,
    A: Clone + Serialize<A> + Send + Sync + 'static,
    K: Clone + Serialize<K> + Send + Sync + 'static,
{
    async fn consume(
        &self,
        channels: &[C],
        patterns: &[P],
        continuation: K,
        persist: bool,
        peeks: BTreeSet<usize>,
    ) -> MaybeActionResult<C, P, A, K> {
        assert!(!channels.is_empty(), "channels can't be empty");
        assert_eq!(
            channels.len(),
            patterns.len(),
            "channels.length must equal patterns.length"
        );
        let consume_ref = Consume::apply(channels, patterns, &continuation, persist);
        let hashes: Vec<Blake2b256Hash> = channels.iter().map(hash_channel).collect();
        let thunk = self.locked_consume(channels, patterns, continuation, persist, peeks, consume_ref);
        self.lock_f
            .acquire(&hashes, Box::pin(async { Ok(hashes.clone()) }), thunk)
            .await?
    }

    async fn produce(&self, channel: C, data: A, persist: bool) -> MaybeActionResult<C, P, A, K> {
        let produce_ref = Produce::apply(&channel, &data, persist);
        let thunk = self.locked_produce(channel.clone(), data, persist, produce_ref);
        let phase_two = {
            let store = self.current_store();
            let channel = channel.clone();
            Box::pin(async move {
                Ok(store
                    .get_joins(&channel)
                    .await?
                    .into_iter()
                    .flatten()
                    .map(|c| hash_channel(&c))
                    .collect())
            })
        };
        let hash = hash_channel(&channel);
        self.lock_f.acquire(&[hash], phase_two, thunk).await?
    }

    async fn install(
        &self,
        channels: &[C],
        patterns: &[P],
        continuation: K,
    ) -> std::result::Result<Option<(K, Vec<A>)>, crate::errors::RSpaceError> {
        assert_eq!(
            channels.len(),
            patterns.len(),
            "channels.length must equal patterns.length"
        );
        let hashes: Vec<Blake2b256Hash> = channels.iter().map(hash_channel).collect();
        let thunk = self.locked_install(channels, patterns, continuation);
        self.lock_f
            .acquire(&hashes, Box::pin(async { Ok(hashes.clone()) }), thunk)
            .await?
    }
}

#[async_trait]
impl<C, P, A, K> ISpace<C, P, A, K> for RSpace<C, P, A, K>
where
    C: Ord + Clone + Serialize<C> + Send + Sync + 'static,
    P: Clone + Serialize<P> + Send + Sync + 'static,
    A: Clone + Serialize<A> + Send + Sync + 'static,
    K: Clone + Serialize<K> + Send + Sync + 'static,
{
    async fn create_checkpoint(&self) -> std::result::Result<Checkpoint, String> {
        let changes = self.current_store().changes().await;
        let native_changes = self.native_store.drain_changes();
        let next_history = {
            let history = self.current_history();
            history.checkpoint_with_native(&changes, &native_changes).await?
        };
        *crate::lock::wlock(&self.history_repository) = next_history.clone();
        let log = std::mem::take(&mut *crate::lock::wlock(&self.event_log));
        *crate::lock::wlock(&self.produce_counter) = BTreeMap::new();
        let history_reader = next_history.get_history_reader(next_history.root()).await;
        let base = history_reader.base();
        *crate::lock::wlock(&self.store) = Arc::new(InMemHotStore::new(base));
        let native_reader = next_history.get_native_reader(next_history.root()).await;
        self.native_store.set_reader(native_reader);
        Ok(Checkpoint {
            root: next_history.root(),
            log,
        })
    }

    async fn reset(&self, root: Blake2b256Hash) -> std::result::Result<(), String> {
        let next_history = {
            let history = self.current_history();
            history.reset(root).await?
        };
        *crate::lock::wlock(&self.history_repository) = next_history.clone();
        *crate::lock::wlock(&self.event_log) = Vec::new();
        *crate::lock::wlock(&self.produce_counter) = BTreeMap::new();
        let history_reader = next_history.get_history_reader(root).await;
        let base = history_reader.base();
        *crate::lock::wlock(&self.store) = Arc::new(InMemHotStore::new(base));
        self.native_store.revert(NativeStoreState::default());
        let native_reader = next_history.get_native_reader(root).await;
        self.native_store.set_reader(native_reader);
        Ok(())
    }

    async fn get_data(&self, channel: &C) -> std::result::Result<Vec<Datum<A>>, crate::errors::RSpaceError> {
        self.current_store().get_data(channel).await
    }

    async fn get_waiting_continuations(
        &self,
        channels: &[C],
    ) -> std::result::Result<Vec<WaitingContinuation<P, K>>, crate::errors::RSpaceError> {
        self.current_store().get_continuations(channels).await
    }

    async fn get_joins(&self, channel: &C) -> std::result::Result<Vec<Vec<C>>, crate::errors::RSpaceError> {
        self.current_store().get_joins(channel).await
    }

    async fn clear(&self) -> std::result::Result<(), String> {
        let empty = crate::history::radix_tree::empty_root_hash();
        self.reset(empty).await
    }

    async fn to_map(&self) -> BTreeMap<Vec<C>, Row<P, A, K>> {
        self.current_store().to_map().await
    }

    async fn create_soft_checkpoint(&self) -> SoftCheckpoint<C, P, A, K> {
        let snapshot = self.current_store().snapshot().await;
        let log = std::mem::take(&mut *crate::lock::wlock(&self.event_log));
        let produce_counter = std::mem::take(&mut *crate::lock::wlock(&self.produce_counter));
        SoftCheckpoint {
            cache_snapshot: snapshot,
            log,
            produce_counter,
            native_snapshot: self.native_store.snapshot(),
        }
    }

    async fn revert_to_soft_checkpoint(&self, checkpoint: SoftCheckpoint<C, P, A, K>) {
        let history = self.current_history();
        let history_reader = history.get_history_reader(history.root()).await;
        let base = history_reader.base();
        *crate::lock::wlock(&self.store) =
            Arc::new(InMemHotStore::from_state(checkpoint.cache_snapshot, base));
        *crate::lock::wlock(&self.event_log) = checkpoint.log;
        *crate::lock::wlock(&self.produce_counter) = checkpoint.produce_counter;
        self.native_store.revert(checkpoint.native_snapshot);
    }
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

    async fn space() -> Arc<RSpace<String, String, String, String>> {
        let manager = InMemoryStoreManager::default();
        crate::factory::create_rspace::<String, String, String, String>(&manager, Arc::new(StrMatch))
            .await
            .unwrap()
    }

    #[tokio::test]
    async fn checkpoint_reset_returns_persisted_data() {
        let s = space().await;
        s.produce("c".to_string(), "data".to_string(), false).await.unwrap();
        let cp = s.create_checkpoint().await.unwrap();
        s.reset(cp.root).await.unwrap();
        let data = s.get_data(&"c".to_string()).await.unwrap();
        assert_eq!(data.len(), 1);
        assert_eq!(data[0].a, "data");
    }

    #[tokio::test]
    async fn soft_checkpoint_revert_rolls_back_produces() {
        let s = space().await;
        let soft = s.create_soft_checkpoint().await;
        s.produce("c".to_string(), "extra".to_string(), false).await.unwrap();
        s.revert_to_soft_checkpoint(soft).await;
        assert!(
            s.get_data(&"c".to_string()).await.unwrap().is_empty(),
            "reverted produce must be gone"
        );
    }
}
