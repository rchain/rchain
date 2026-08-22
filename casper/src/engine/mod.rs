//! Node engine (port of the `casper/engine/` state machines).
//!
//! The pure `LfsState`/`LfsTupleSpaceState` requester states, the effectful
//! `LfsBlockRequester`/`LfsTupleSpaceRequester`/`NodeSyncing`/`NodeRunning` machines, and
//! `NodeLaunch`'s genesis-from-config helpers + `apply` mode-dispatch are ported.

pub mod lfs_block_requester;
pub mod lfs_tuple_space_requester;
pub mod node_launch;
pub mod node_running;
pub mod node_syncing;

use std::collections::{BTreeMap, BTreeSet};

/// A casper-message handling status (port of `NodeRunning.CasperMessageStatus`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CasperMessageStatus {
    BlockIsInDag,
    BlockIsInCasperBuffer,
    BlockIsReceived,
    BlockIsWaitingForCasper,
    BlockIsInProcessing,
    DoNotIgnore,
}

/// Whether to ignore a casper message + the status (port of `IgnoreCasperMessageStatus`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IgnoreCasperMessageStatus {
    pub do_ignore: bool,
    pub status: CasperMessageStatus,
}

/// A request status (port of `LfsBlockRequester.ReqStatus`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReqStatus {
    Init,
    Requested,
    Received,
}

/// Flags describing a received block (port of `LfsBlockRequester.ReceiveInfo`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReceiveInfo {
    pub requested: bool,
    pub latest: bool,
    pub last_latest: bool,
}

/// Request-state machine for the Last Finalized State block requester (port of
/// `LfsBlockRequester.ST`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LfsState<Key: Ord + Clone> {
    pub d: BTreeMap<Key, ReqStatus>,
    pub latest: BTreeSet<Key>,
    pub lower_bound: i64,
    pub height_map: BTreeMap<i64, BTreeSet<Key>>,
    pub finished: BTreeSet<Key>,
    pub extra_heights: i32,
}

impl<Key: Ord + Clone> LfsState<Key> {
    /// Create a request state with initial keys (port of `ST.apply`).
    pub fn new(
        initial: BTreeSet<Key>,
        latest: BTreeSet<Key>,
        lower_bound: i64,
        extra_heights: i32,
    ) -> Self {
        LfsState {
            d: initial.into_iter().map(|k| (k, ReqStatus::Init)).collect(),
            latest,
            lower_bound,
            height_map: BTreeMap::new(),
            finished: BTreeSet::new(),
            extra_heights,
        }
    }

    /// Add new keys in `Init` status, skipping existing/finished keys (port of `add`).
    pub fn add(&self, keys: &BTreeSet<Key>) -> LfsState<Key> {
        let mut st = self.clone();
        for k in keys {
            if !st.finished.contains(k) && !st.d.contains_key(k) {
                st.d.insert(k.clone(), ReqStatus::Init);
            }
        }
        st
    }

    /// Get the next keys to request (port of `getNext`).
    pub fn get_next(&self, resend: bool) -> (LfsState<Key>, BTreeSet<Key>) {
        let mut requests = self.d.clone();
        if !self.latest.is_empty() {
            for k in &self.latest {
                requests.entry(k.clone()).or_insert(ReqStatus::Init);
            }
        }

        let mut new_requests: BTreeMap<Key, ReqStatus> = BTreeMap::new();
        for (key, status) in &requests {
            let check_for_request =
                *status == ReqStatus::Init || (resend && *status == ReqStatus::Requested);
            let select = if self.latest.is_empty() {
                check_for_request
            } else {
                check_for_request && self.latest.contains(key)
            };
            if select {
                new_requests.insert(key.clone(), ReqStatus::Requested);
            }
        }

        let mut st = self.clone();
        for (k, s) in &new_requests {
            st.d.insert(k.clone(), s.clone());
        }
        let keys: BTreeSet<Key> = new_requests.into_keys().collect();
        (st, keys)
    }

    /// Mark `k` received, returning the updated state + receive flags (port of `received`).
    pub fn received(&self, k: &Key, height: i64) -> (LfsState<Key>, ReceiveInfo) {
        let is_req = self.d.get(k) == Some(&ReqStatus::Requested);
        if !is_req {
            return (
                self.clone(),
                ReceiveInfo {
                    requested: false,
                    latest: false,
                    last_latest: false,
                },
            );
        }

        let mut new_latest = self.latest.clone();
        new_latest.remove(k);
        let is_latest = self.latest != new_latest;
        let is_last_latest = is_latest && new_latest.is_empty();

        let mut height_map = self.height_map.clone();
        height_map.entry(height).or_default().insert(k.clone());

        let lower_bound_1 = if is_latest {
            (height - 1).min(self.lower_bound)
        } else {
            self.lower_bound
        };
        let lower_bound = if is_last_latest {
            (lower_bound_1 - self.extra_heights as i64).max(0)
        } else {
            lower_bound_1
        };

        let mut d = self.d.clone();
        d.insert(k.clone(), ReqStatus::Received);

        let st = LfsState {
            d,
            latest: new_latest,
            lower_bound,
            height_map,
            finished: self.finished.clone(),
            extra_heights: self.extra_heights,
        };
        (
            st,
            ReceiveInfo {
                requested: is_req,
                latest: is_latest,
                last_latest: is_last_latest,
            },
        )
    }

    /// Mark `k` finished if it was received (port of `done`).
    pub fn done(&self, k: &Key) -> LfsState<Key> {
        if self.d.get(k) == Some(&ReqStatus::Received) {
            let mut st = self.clone();
            st.d.remove(k);
            st.finished.insert(k.clone());
            st
        } else {
            self.clone()
        }
    }

    /// Whether all keys are finished (port of `isFinished`).
    pub fn is_finished(&self) -> bool {
        self.latest.is_empty() && self.d.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn set(items: &[i32]) -> BTreeSet<i32> {
        items.iter().copied().collect()
    }

    #[test]
    fn add_skips_finished_and_existing() {
        let st = LfsState::new(set(&[1]), set(&[]), 0, 0);
        let st = st.add(&set(&[1, 2]));
        assert_eq!(st.d.len(), 2);
        assert_eq!(st.d[&1], ReqStatus::Init);
        assert_eq!(st.d[&2], ReqStatus::Init);
    }

    #[test]
    fn get_next_requests_init_keys() {
        let st = LfsState::new(set(&[1, 2]), set(&[]), 0, 0);
        let (st, req) = st.get_next(false);
        assert_eq!(req, set(&[1, 2]));
        // All requested now.
        assert!(st.d.values().all(|s| *s == ReqStatus::Requested));
        // Second call without resend requests nothing.
        let (_, req2) = st.get_next(false);
        assert!(req2.is_empty());
        // With resend, requested keys are re-requested.
        let (_, req3) = st.get_next(true);
        assert_eq!(req3, set(&[1, 2]));
    }

    #[test]
    fn received_tracks_latest_and_lower_bound() {
        let st = LfsState::new(set(&[]), set(&[1, 2]), 10, 0);
        let (st, _req) = st.get_next(false);
        let (st, info) = st.received(&1, 5);
        assert!(info.latest);
        assert!(!info.last_latest);
        assert_eq!(st.lower_bound, 4);

        let (st, info) = st.received(&2, 6);
        assert!(info.latest);
        assert!(info.last_latest);
        assert_eq!(st.lower_bound, 4);
        // Not finished until received keys are marked done.
        assert!(!st.is_finished());

        let st = st.done(&1).done(&2);
        assert!(st.is_finished());
    }

    #[test]
    fn done_marks_finished() {
        let st = LfsState::new(set(&[1]), set(&[]), 0, 0);
        let (st, _) = st.get_next(false);
        let (st, _) = st.received(&1, 0);
        let st = st.done(&1);
        assert!(st.d.is_empty());
        assert!(st.finished.contains(&1));
        assert!(st.is_finished());
    }
}
