//! Last Finalized State tuple-space requester (port of `engine/LfsTupleSpaceRequester.scala`).
//!
//! Downloads the rholang state (history + data items) for the last finalized state in chunks, via
//! the pure `LfsTupleSpaceState` state machine and the effectful `stream` orchestration.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;
use std::time::Duration;

use rchain_comm::rp::rp_conf::RPConf;
use rchain_comm::transport::transport_layer::TransportLayer;
use rchain_comm::transport::transport_layer_syntax;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_models::casper::protocol::casper_message::{
    FinalizedFringe, StoreItemsMessage, StoreItemsMessageRequest,
};
use rchain_models::casper::protocol::packet_type_tag::ToPacket;
use rchain_rspace::state::{validate_state_items, RSpaceImporter, StateValidationError};
use rchain_shared::log::{Log, LogSource};

use crate::protocol::casper_message_protocol::StoreItemsMessageRequestSerde;

/// A rspace state path: nested `(hash, index)` levels, with `index` as the pointer-block index
/// (port of `StatePartPath`).
pub type StatePartPath = Vec<(Blake2b256Hash, Option<u8>)>;

/// Number of nodes in an LFS sync data-transfer chunk (port of `pageSize`).
pub const PAGE_SIZE: i32 = 750;

/// Request status (port of `LfsTupleSpaceRequester.ReqStatus`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReqStatus {
    Init,
    Requested,
    Received,
    Done,
}

/// The tuple-space requester state machine (port of `LfsTupleSpaceRequester.ST`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LfsTupleSpaceState<Key: Ord + Clone> {
    d: BTreeMap<Key, ReqStatus>,
}

impl<Key: Ord + Clone> LfsTupleSpaceState<Key> {
    /// Create the state with the initial keys in `Init` status (port of `ST.apply`).
    pub fn new(initial: Vec<Key>) -> Self {
        LfsTupleSpaceState {
            d: initial.into_iter().map(|k| (k, ReqStatus::Init)).collect(),
        }
    }

    /// Add new keys in `Init` status, skipping existing keys (port of `add`).
    pub fn add(&self, keys: &BTreeSet<Key>) -> Self {
        let mut d = self.d.clone();
        for k in keys {
            d.entry(k.clone()).or_insert(ReqStatus::Init);
        }
        LfsTupleSpaceState { d }
    }

    /// Get the next keys to request, marking them `Requested` (port of `getNext`).
    pub fn get_next(&self, resend: bool) -> (Self, Vec<Key>) {
        let requested: Vec<Key> = self
            .d
            .iter()
            .filter(|(_, v)| **v == ReqStatus::Init || (resend && **v == ReqStatus::Requested))
            .map(|(k, _)| k.clone())
            .collect();
        let mut d = self.d.clone();
        for k in &requested {
            d.insert(k.clone(), ReqStatus::Requested);
        }
        (LfsTupleSpaceState { d }, requested)
    }

    /// Mark `k` received if it was requested, returning whether it was requested (port of
    /// `received`).
    pub fn received(&self, k: Key) -> (Self, bool) {
        let is_requested = self.d.get(&k) == Some(&ReqStatus::Requested);
        let mut d = self.d.clone();
        if is_requested {
            d.insert(k, ReqStatus::Received);
        }
        (LfsTupleSpaceState { d }, is_requested)
    }

    /// Mark `k` done if it was received (port of `done`).
    pub fn done(&self, k: Key) -> Self {
        let is_received = self.d.get(&k) == Some(&ReqStatus::Received);
        let mut d = self.d.clone();
        if is_received {
            d.insert(k, ReqStatus::Done);
        }
        LfsTupleSpaceState { d }
    }

    /// Whether all keys are done (port of `isFinished`).
    pub fn is_finished(&self) -> bool {
        self.d.values().all(|v| *v == ReqStatus::Done)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn keys(items: &[i32]) -> BTreeSet<i32> {
        items.iter().copied().collect()
    }

    fn as_set(items: &[i32]) -> BTreeSet<i32> {
        items.iter().copied().collect()
    }

    #[test]
    fn get_next_returns_empty_when_called_again() {
        let st = LfsTupleSpaceState::new(vec![10]);
        let (st1, ids1) = st.get_next(false);
        assert_eq!(as_set(&ids1), keys(&[10]));

        let (st2, ids2) = st1.get_next(false);
        assert!(ids2.is_empty());
        assert_eq!(st1, st2);
    }

    #[test]
    fn get_next_returns_new_items_after_add() {
        let st = LfsTupleSpaceState::new(vec![10]);
        let st2 = st.add(&keys(&[9, 8]));
        let (_, ids2) = st2.get_next(false);
        assert_eq!(as_set(&ids2), keys(&[10, 9, 8]));
    }

    #[test]
    fn get_next_returns_requested_on_resend() {
        let st = LfsTupleSpaceState::new(vec![10]);
        let (st1, ids1) = st.get_next(false);
        assert_eq!(as_set(&ids1), keys(&[10]));

        let (_, ids2) = st1.get_next(true);
        assert_eq!(as_set(&ids2), keys(&[10]));
    }

    #[test]
    fn received_true_for_requested_false_for_unknown() {
        let st = LfsTupleSpaceState::new(vec![10]);
        let (st1, _) = st.get_next(false);

        let (_, is_received) = st1.received(10);
        assert!(is_received);

        let (_, is_received) = st1.received(100);
        assert!(!is_received);
    }

    #[test]
    fn done_makes_state_finished() {
        let st = LfsTupleSpaceState::new(vec![10]);

        let st1 = st.done(10);
        assert!(!st1.is_finished());

        let (st2, _) = st1.get_next(false);
        let (st3, _) = st2.received(10);
        let st4 = st3.done(10);
        assert!(st4.is_finished());
    }

    #[test]
    fn start_to_finish_receives_one_item() {
        let st = LfsTupleSpaceState::new(vec![10]);

        let (st1, ids1) = st.get_next(false);
        assert_eq!(as_set(&ids1), keys(&[10]));

        let (st2, ids2) = st1.get_next(false);
        assert!(ids2.is_empty());
        assert!(!st2.is_finished());

        let (st3, is_received) = st2.received(10);
        assert!(is_received);

        let st4 = st3.done(10);
        assert!(st4.is_finished());
    }
}

// -------------------------------------------------------------------------------------------------
// Effectful stream (port of `LfsTupleSpaceRequester.stream`)
// -------------------------------------------------------------------------------------------------

/// Take the next set of state-part paths to request and send a `StoreItemsMessageRequest` for each
/// (port of `requestStream`'s broadcast step).
async fn request_next(
    st: &Arc<tokio::sync::Mutex<LfsTupleSpaceState<StatePartPath>>>,
    transport: &dyn TransportLayer,
    conf: &RPConf,
    log: &dyn Log,
    source: LogSource,
    resend: bool,
) {
    let is_end = { st.lock().await.is_finished() };
    let ids = {
        let mut guard = st.lock().await;
        let (new_state, ids) = guard.get_next(resend);
        *guard = new_state;
        ids
    };
    if !is_end && !ids.is_empty() {
        for id in &ids {
            log.info(source, "Sending StoreItemsRequest to bootstrap");
            let req = StoreItemsMessageRequest {
                start_path: id.clone(),
                skip: 0,
                take: PAGE_SIZE,
            };
            transport_layer_syntax::send_to_bootstrap(
                transport,
                conf,
                StoreItemsMessageRequestSerde.mk_packet(&req),
            )
            .await;
        }
    }
}

/// Process an incoming `StoreItemsMessage`: validate it, import its history/data items, and mark the
/// chunk done (port of `responseStream`).
async fn process_store_items<I: RSpaceImporter>(
    st: &Arc<tokio::sync::Mutex<LfsTupleSpaceState<StatePartPath>>>,
    importer: &mut I,
    log: &dyn Log,
    source: LogSource,
    request_tx: &tokio::sync::mpsc::Sender<bool>,
    msg: &StoreItemsMessage,
) -> Result<(), StateValidationError> {
    let start_path = msg.start_path.clone();
    let is_received = {
        let mut guard = st.lock().await;
        let (new_state, is_received) = guard.received(start_path.clone());
        *guard = new_state;
        is_received
    };

    if is_received {
        // Add the last path for requesting and trigger the request queue.
        {
            let last_path: BTreeSet<StatePartPath> = [msg.last_path.clone()].into_iter().collect();
            let mut guard = st.lock().await;
            *guard = guard.add(&last_path);
        }
        let _ = request_tx.send(false).await;

        // Validate received state items against the trie.
        validate_state_items(
            &msg.history_items,
            &msg.data_items,
            &start_path,
            PAGE_SIZE,
            0,
            &|h| importer.get_history_item(*h),
        )
        .map_err(|e| {
            log.error(source, &format!("Invalid state items received: {e:?}"));
            e
        })?;

        // Import history and data items.
        importer.set_history_items(&msg.history_items, |v: &Vec<u8>| v.clone());
        importer.set_data_items(&msg.data_items, |v: &Vec<u8>| v.clone());

        // Mark the chunk done and trigger the request queue.
        {
            let mut guard = st.lock().await;
            *guard = guard.done(start_path.clone());
        }
        let _ = request_tx.send(false).await;
    }
    Ok(())
}

/// Request the tuple space (history + data items) for the last finalized state (port of
/// `LfsTupleSpaceRequester.stream`). Returns the final requester state once all chunks are received.
pub async fn request_tuple_space<I: RSpaceImporter>(
    fringe: &FinalizedFringe,
    tuple_space_rx: &mut tokio::sync::mpsc::Receiver<StoreItemsMessage>,
    request_timeout: Duration,
    transport: &dyn TransportLayer,
    conf: &RPConf,
    importer: &mut I,
    log: &dyn Log,
) -> Result<LfsTupleSpaceState<StatePartPath>, StateValidationError> {
    let source = LogSource::new("casper.engine.LfsTupleSpaceRequester");

    let state_hash = Blake2b256Hash::from_byte_array(fringe.state_hash.as_bytes());
    let start_request: StatePartPath = vec![(state_hash, None)];
    importer.set_root(state_hash);

    let st = Arc::new(tokio::sync::Mutex::new(LfsTupleSpaceState::new(vec![
        start_request,
    ])));
    let (request_tx, mut request_rx) = tokio::sync::mpsc::channel::<bool>(2);
    let _ = request_tx.send(false).await;

    let error: Arc<tokio::sync::Mutex<Option<StateValidationError>>> =
        Arc::new(tokio::sync::Mutex::new(None));

    // Request loop: pull request triggers (or resend on idle timeout), terminating when finished.
    let request_loop = async {
        loop {
            let resend = tokio::select! {
                r = request_rx.recv() => match r {
                    Some(r) => r,
                    None => return,
                },
                _ = tokio::time::sleep(request_timeout) => {
                    log.warn(
                        source,
                        &format!(
                            "No tuple space state responses for {request_timeout:?}. Resending requests."
                        ),
                    );
                    true
                }
            };
            request_next(&st, transport, conf, log, source, resend).await;
            if error.lock().await.is_some() {
                return;
            }
            if st.lock().await.is_finished() {
                return;
            }
        }
    };

    // Response loop: handle incoming state chunks in parallel with the request loop.
    let response_loop = async {
        loop {
            match tuple_space_rx.recv().await {
                Some(msg) => {
                    if let Err(e) =
                        process_store_items(&st, &mut *importer, log, source, &request_tx, &msg).await
                    {
                        *error.lock().await = Some(e);
                        return;
                    }
                }
                None => return,
            }
        }
    };

    tokio::pin!(request_loop);
    tokio::pin!(response_loop);
    tokio::select! {
        _ = &mut request_loop => {},
        _ = &mut response_loop => {},
    }

    if let Some(e) = error.lock().await.clone() {
        return Err(e);
    }
    let guard = st.lock().await;
    Ok(guard.clone())
}
