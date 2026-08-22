//! Proposer instance (port of `node/instances/ProposerInstance.scala`).
//!
//! Drains propose requests, serializing actual proposal through a semaphore; concurrent attempts
//! resolve to `ProposerResult::Empty`. The `trigger` re-enqueue optimization is deferred.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use futures_util::StreamExt;
use tokio::sync::{mpsc, oneshot, Semaphore};
use tokio_stream::wrappers::ReceiverStream;

use rchain_casper::blocks::proposer::propose_result::{ProposeResult, ProposeStatus};
use rchain_casper::blocks::proposer::proposer::{Proposer, ProposerResult};
use rchain_casper::state::ProposerState;
use rchain_models::casper::protocol::casper_message::BlockMessage;

/// Create the proposer stream (port of `ProposerInstance.create`).
pub fn create(
    propose_requests_rx: mpsc::Receiver<(bool, oneshot::Sender<ProposerResult>)>,
    propose_requests_tx: mpsc::Sender<(bool, oneshot::Sender<ProposerResult>)>,
    proposer: Proposer,
    state: Arc<tokio::sync::Mutex<ProposerState>>,
) -> impl tokio_stream::Stream<Item = (ProposeResult, Option<BlockMessage>)> + Send + 'static {
    let input = ReceiverStream::new(propose_requests_rx);
    let lock = Arc::new(Semaphore::new(1));
    let trigger = Arc::new(AtomicBool::new(false));
    let proposer = Arc::new(proposer);

    input
        .map(move |(is_async, propose_id_def)| {
            let lock = lock.clone();
            let trigger = trigger.clone();
            let state = state.clone();
            let tx = propose_requests_tx.clone();
            let proposer = proposer.clone();
            async move {
                let permit = match lock.clone().try_acquire_owned() {
                    Ok(p) => p,
                    Err(_) => {
                        let _ = propose_id_def.send(ProposerResult::Empty);
                        trigger.store(true, Ordering::SeqCst);
                        return None;
                    }
                };

                let (r_tx, r_rx) = oneshot::channel();
                {
                    state.lock().await.curr_propose_result = Some(r_rx);
                }
                let r = proposer.propose(is_async, propose_id_def).await;
                let r = match r {
                    Ok(r) => r,
                    Err(_) => (
                        ProposeResult {
                            propose_status: ProposeStatus::BugError,
                        },
                        None,
                    ),
                };
                let _ = r_tx.send(r.clone());
                {
                    let mut s = state.lock().await;
                    s.latest_propose_result = Some(r.clone());
                    s.curr_propose_result = None;
                }
                drop(permit);

                // Re-enqueue a follow-up propose if a request arrived while this one was running.
                if trigger.swap(false, Ordering::SeqCst) {
                    let (d_tx, d_rx) = oneshot::channel();
                    let _ = tx.send((false, d_tx)).await;
                    // Keep the receiver alive until the re-queued propose completes it.
                    std::mem::forget(d_rx);
                }
                Some(r)
            }
        })
        .buffer_unordered(100)
        .filter_map(|r| async move { r })
}
