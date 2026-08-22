//! The in-memory hot store overlay over a history snapshot.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/HotStore.scala`. Reads from the history store
//! are memoized per key in a `HistoryStoreCache` (the Scala `Deferred`-backed cache), so concurrent
//! readers of the same key share a single back-fill and readers of different keys do not serialize.

use std::collections::BTreeMap;
use std::sync::Arc;

use async_trait::async_trait;
use tokio::sync::{Mutex, OnceCell};

use crate::errors::RSpaceError;
use crate::history::history_reader::HistoryReaderBase;
use crate::hot_store_action::HotStoreAction;
use crate::internal::{Datum, Row, WaitingContinuation};

/// The hot-store overlay state (port of `HotStoreState`).
#[derive(Clone, Debug)]
pub struct HotStoreState<C, P, A, K> {
    pub continuations: BTreeMap<Vec<C>, Vec<WaitingContinuation<P, K>>>,
    pub installed_continuations: BTreeMap<Vec<C>, WaitingContinuation<P, K>>,
    pub data: BTreeMap<C, Vec<Datum<A>>>,
    pub joins: BTreeMap<C, Vec<Vec<C>>>,
    pub installed_joins: BTreeMap<C, Vec<Vec<C>>>,
}

impl<C, P, A, K> Default for HotStoreState<C, P, A, K> {
    fn default() -> Self {
        HotStoreState {
            continuations: BTreeMap::new(),
            installed_continuations: BTreeMap::new(),
            data: BTreeMap::new(),
            joins: BTreeMap::new(),
            installed_joins: BTreeMap::new(),
        }
    }
}

fn remove_index<E: Clone>(col: &[E], index: usize) -> Vec<E> {
    let mut out = col.to_vec();
    out.remove(index);
    out
}

/// Memoized history-store reads (port of `HistoryStoreCache`).
struct HistoryStoreCache<C, P, A, K> {
    continuations: BTreeMap<Vec<C>, Arc<OnceCell<Vec<WaitingContinuation<P, K>>>>>,
    datums: BTreeMap<C, Arc<OnceCell<Vec<Datum<A>>>>>,
    joins: BTreeMap<C, Arc<OnceCell<Vec<Vec<C>>>>>,
}

impl<C, P, A, K> Default for HistoryStoreCache<C, P, A, K> {
    fn default() -> Self {
        HistoryStoreCache {
            continuations: BTreeMap::new(),
            datums: BTreeMap::new(),
            joins: BTreeMap::new(),
        }
    }
}

/// The hot store interface (port of `HotStore[F]`).
#[async_trait]
pub trait HotStore<C, P, A, K>: Send + Sync {
    async fn get_continuations(
        &self,
        channels: &[C],
    ) -> Result<Vec<WaitingContinuation<P, K>>, RSpaceError>;
    async fn put_continuation(
        &self,
        channels: &[C],
        wc: WaitingContinuation<P, K>,
    ) -> Result<(), RSpaceError>;
    async fn install_continuation(&self, channels: &[C], wc: WaitingContinuation<P, K>);
    async fn remove_continuation(
        &self,
        channels: &[C],
        index: usize,
    ) -> Result<(), RSpaceError>;

    async fn get_data(&self, channel: &C) -> Result<Vec<Datum<A>>, RSpaceError>;
    async fn put_datum(&self, channel: &C, datum: Datum<A>) -> Result<(), RSpaceError>;
    async fn remove_datum(&self, channel: &C, index: i64) -> Result<(), RSpaceError>;

    async fn get_joins(&self, channel: &C) -> Result<Vec<Vec<C>>, RSpaceError>;
    async fn put_join(&self, channel: &C, join: &[C]) -> Result<(), RSpaceError>;
    async fn install_join(&self, channel: &C, join: &[C]);
    async fn remove_join(&self, channel: &C, join: &[C]) -> Result<(), RSpaceError>;

    async fn changes(&self) -> Vec<HotStoreAction<C, P, A, K>>;
    async fn to_map(&self) -> BTreeMap<Vec<C>, Row<P, A, K>>;
    async fn snapshot(&self) -> HotStoreState<C, P, A, K>;
}

/// The in-memory hot store (port of `InMemHotStore`).
pub struct InMemHotStore<C, P, A, K> {
    state: Mutex<HotStoreState<C, P, A, K>>,
    cache: Mutex<HistoryStoreCache<C, P, A, K>>,
    reader_base: Arc<dyn HistoryReaderBase<C, P, A, K>>,
}

impl<C, P, A, K> InMemHotStore<C, P, A, K>
where
    C: Ord + Clone + Send + Sync + 'static,
    P: Clone + Send + Sync + 'static,
    A: Clone + Send + Sync + 'static,
    K: Clone + Send + Sync + 'static,
{
    pub fn new(reader_base: Arc<dyn HistoryReaderBase<C, P, A, K>>) -> Self {
        InMemHotStore {
            state: Mutex::new(HotStoreState::default()),
            cache: Mutex::new(HistoryStoreCache::default()),
            reader_base,
        }
    }

    pub fn from_state(
        state: HotStoreState<C, P, A, K>,
        reader_base: Arc<dyn HistoryReaderBase<C, P, A, K>>,
    ) -> Self {
        InMemHotStore {
            state: Mutex::new(state),
            cache: Mutex::new(HistoryStoreCache::default()),
            reader_base,
        }
    }

    async fn get_cont_from_history_store(
        &self,
        channels: &[C],
    ) -> Result<Vec<WaitingContinuation<P, K>>, RSpaceError> {
        let cell = {
            let mut cache = self.cache.lock().await;
            cache
                .continuations
                .entry(channels.to_vec())
                .or_insert_with(|| Arc::new(OnceCell::new()))
                .clone()
        };
        let reader_base = self.reader_base.clone();
        let channels = channels.to_vec();
        let result = cell
            .get_or_try_init(|| async move { reader_base.get_continuations(&channels).await })
            .await?;
        Ok(result.clone())
    }

    async fn get_data_from_history_store(
        &self,
        channel: &C,
    ) -> Result<Vec<Datum<A>>, RSpaceError> {
        let cell = {
            let mut cache = self.cache.lock().await;
            cache
                .datums
                .entry(channel.clone())
                .or_insert_with(|| Arc::new(OnceCell::new()))
                .clone()
        };
        let reader_base = self.reader_base.clone();
        let channel = channel.clone();
        let result = cell
            .get_or_try_init(|| async move { reader_base.get_data(&channel).await })
            .await?;
        Ok(result.clone())
    }

    async fn get_joins_from_history_store(
        &self,
        channel: &C,
    ) -> Result<Vec<Vec<C>>, RSpaceError> {
        let cell = {
            let mut cache = self.cache.lock().await;
            cache
                .joins
                .entry(channel.clone())
                .or_insert_with(|| Arc::new(OnceCell::new()))
                .clone()
        };
        let reader_base = self.reader_base.clone();
        let channel = channel.clone();
        let result = cell
            .get_or_try_init(|| async move { reader_base.get_joins(&channel).await })
            .await?;
        Ok(result.clone())
    }
}

#[async_trait]
impl<C, P, A, K> HotStore<C, P, A, K> for InMemHotStore<C, P, A, K>
where
    C: Ord + Clone + Send + Sync + 'static,
    P: Clone + Send + Sync + 'static,
    A: Clone + Send + Sync + 'static,
    K: Clone + Send + Sync + 'static,
{
    async fn get_continuations(
        &self,
        channels: &[C],
    ) -> Result<Vec<WaitingContinuation<P, K>>, RSpaceError> {
        let from_history = self.get_cont_from_history_store(channels).await?;
        let mut state = self.state.lock().await;
        Ok(match state.continuations.get(channels) {
            Some(conts) => {
                let mut out = Vec::new();
                if let Some(installed) = state.installed_continuations.get(channels) {
                    out.push(installed.clone());
                }
                out.extend(conts.clone());
                out
            }
            None => {
                state
                    .continuations
                    .insert(channels.to_vec(), from_history.clone());
                let mut out = Vec::new();
                if let Some(installed) = state.installed_continuations.get(channels) {
                    out.push(installed.clone());
                }
                out.extend(from_history);
                out
            }
        })
    }

    async fn put_continuation(
        &self,
        channels: &[C],
        wc: WaitingContinuation<P, K>,
    ) -> Result<(), RSpaceError> {
        let from_history = self.get_cont_from_history_store(channels).await?;
        let mut state = self.state.lock().await;
        let cur = state
            .continuations
            .entry(channels.to_vec())
            .or_insert(from_history);
        cur.insert(0, wc);
        Ok(())
    }

    async fn install_continuation(&self, channels: &[C], wc: WaitingContinuation<P, K>) {
        let mut state = self.state.lock().await;
        state
            .installed_continuations
            .insert(channels.to_vec(), wc);
    }

    async fn remove_continuation(
        &self,
        channels: &[C],
        index: usize,
    ) -> Result<(), RSpaceError> {
        let from_history = self.get_cont_from_history_store(channels).await?;
        let mut state = self.state.lock().await;
        let is_installed = state.installed_continuations.contains_key(channels);
        if is_installed && index == 0 {
            // Attempted to remove the installed continuation — skip.
            return Ok(());
        }
        let removed_index = if is_installed { index - 1 } else { index };
        let cur = state
            .continuations
            .entry(channels.to_vec())
            .or_insert(from_history);
        if removed_index < cur.len() {
            *cur = remove_index(cur, removed_index);
        }
        Ok(())
    }

    async fn get_data(&self, channel: &C) -> Result<Vec<Datum<A>>, RSpaceError> {
        let from_history = self.get_data_from_history_store(channel).await?;
        let mut state = self.state.lock().await;
        Ok(match state.data.get(channel) {
            Some(data) => data.clone(),
            None => {
                state.data.insert(channel.clone(), from_history.clone());
                from_history
            }
        })
    }

    async fn put_datum(&self, channel: &C, datum: Datum<A>) -> Result<(), RSpaceError> {
        let from_history = self.get_data_from_history_store(channel).await?;
        let mut state = self.state.lock().await;
        let cur = state.data.entry(channel.clone()).or_insert(from_history);
        cur.insert(0, datum);
        Ok(())
    }

    async fn remove_datum(&self, channel: &C, index: i64) -> Result<(), RSpaceError> {
        let from_history = self.get_data_from_history_store(channel).await?;
        let mut state = self.state.lock().await;
        let cur = state.data.entry(channel.clone()).or_insert(from_history);
        if index >= 0 && (index as usize) < cur.len() {
            *cur = remove_index(cur, index as usize);
        }
        Ok(())
    }

    async fn get_joins(&self, channel: &C) -> Result<Vec<Vec<C>>, RSpaceError> {
        let from_history = self.get_joins_from_history_store(channel).await?;
        let mut state = self.state.lock().await;
        Ok(match state.joins.get(channel) {
            Some(joins) => {
                let mut out = state.installed_joins.get(channel).cloned().unwrap_or_default();
                out.extend(joins.clone());
                out
            }
            None => {
                state.joins.insert(channel.clone(), from_history.clone());
                let mut out = state.installed_joins.get(channel).cloned().unwrap_or_default();
                out.extend(from_history);
                out
            }
        })
    }

    async fn put_join(&self, channel: &C, join: &[C]) -> Result<(), RSpaceError> {
        let from_history = self.get_joins_from_history_store(channel).await?;
        let mut state = self.state.lock().await;
        let cur = state.joins.entry(channel.clone()).or_insert(from_history);
        if !cur.contains(&join.to_vec()) {
            cur.insert(0, join.to_vec());
        }
        Ok(())
    }

    async fn install_join(&self, channel: &C, join: &[C]) {
        let mut state = self.state.lock().await;
        let cur = state.installed_joins.entry(channel.clone()).or_default();
        if !cur.contains(&join.to_vec()) {
            cur.insert(0, join.to_vec());
        }
    }

    async fn remove_join(&self, channel: &C, join: &[C]) -> Result<(), RSpaceError> {
        let from_history = self.get_joins_from_history_store(channel).await?;
        let mut state = self.state.lock().await;
        let cur = state.joins.entry(channel.clone()).or_insert(from_history);
        if let Some(index) = cur.iter().position(|j| j == join) {
            *cur = remove_index(cur, index);
        }
        Ok(())
    }

    async fn changes(&self) -> Vec<HotStoreAction<C, P, A, K>> {
        let state = self.state.lock().await;
        let mut out = Vec::new();
        for (k, v) in &state.continuations {
            if v.is_empty() {
                out.push(HotStoreAction::DeleteContinuations(k.clone()));
            } else {
                out.push(HotStoreAction::InsertContinuations(k.clone(), v.clone()));
            }
        }
        for (k, v) in &state.data {
            if v.is_empty() {
                out.push(HotStoreAction::DeleteData(k.clone()));
            } else {
                out.push(HotStoreAction::InsertData(k.clone(), v.clone()));
            }
        }
        for (k, v) in &state.joins {
            if v.is_empty() {
                out.push(HotStoreAction::DeleteJoins(k.clone()));
            } else {
                out.push(HotStoreAction::InsertJoins(k.clone(), v.clone()));
            }
        }
        out
    }

    async fn to_map(&self) -> BTreeMap<Vec<C>, Row<P, A, K>> {
        let state = self.state.lock().await;
        let mut out: BTreeMap<Vec<C>, Row<P, A, K>> = BTreeMap::new();
        for (k, v) in &state.data {
            out.entry(vec![k.clone()]).or_default().data = v.clone();
        }
        for (k, v) in &state.continuations {
            out.entry(k.clone()).or_default().wks.extend(v.clone());
        }
        for (k, v) in &state.installed_continuations {
            out.entry(k.clone()).or_default().wks.insert(0, v.clone());
        }
        out.retain(|_, row| !(row.data.is_empty() && row.wks.is_empty()));
        out
    }

    async fn snapshot(&self) -> HotStoreState<C, P, A, K> {
        self.state.lock().await.clone()
    }
}
