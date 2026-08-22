//! Last Finalized State block requester (port of `engine/LfsBlockRequester.scala`).
//!
//! Downloads the blocks needed to reconstruct the last finalized state, following justifications
//! from the finalized fringe. The pure requester state is [`super::LfsState`]; the `request_blocks`
//! stream orchestration (request loop + response loop with an idle-resend timeout) is ported here
//! onto tokio channels, mirroring the fs2 `requestStream concurrently responseStream` structure.

use std::collections::BTreeSet;
use std::sync::Arc;
use std::time::Duration;

use rchain_block_storage::block_store::BlockStore;
use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::{BlockMessage, FinalizedFringe};
use rchain_shared::log::{Log, LogSource};

use super::{LfsState, ReceiveInfo};
use crate::protocol::comm_util::CommUtil;
use crate::validate;

/// The block-requester state keyed by block hash (port of `ST[BlockHash]`).
type St = LfsState<BlockHash>;

/// Validate a received block and, if accepted, request its justifications. Returns whether the
/// block was requested and its hash valid (port of `validateReceivedBlock`).
async fn validate_received_block(
    st: &Arc<tokio::sync::Mutex<St>>,
    block: &BlockMessage,
    log: &dyn Log,
    source: LogSource,
) -> bool {
    let block_number = i64::from(block.block_number);
    let (info, minimum_height) = {
        let mut guard = st.lock().await;
        let (new_state, info) = guard.received(&block.block_hash, block_number);
        *guard = new_state;
        (info, guard.lower_bound)
    };
    let ReceiveInfo {
        requested,
        latest,
        last_latest,
    } = info;

    let block_hash_is_valid = requested && validate::block_hash(block);
    if requested && !block_hash_is_valid {
        log.warn(
            source,
            &format!(
                "Received block #{} with invalid hash. Ignored block.",
                block.block_number
            ),
        );
    }

    if block_hash_is_valid {
        if last_latest {
            log.info(
                source,
                &format!("Latest blocks downloaded. Minimum block height is {minimum_height}."),
            );
        }
        let block_is_accepted = latest || (requested && block_number >= minimum_height);
        if block_is_accepted {
            let justifications: BTreeSet<BlockHash> = block.justifications.iter().copied().collect();
            let mut guard = st.lock().await;
            *guard = guard.add(&justifications);
        }
        return requested;
    }
    false
}

/// Save a received block to the store and mark it done (port of `saveBlock`).
async fn save_block(
    st: &Arc<tokio::sync::Mutex<St>>,
    block_store: &BlockStore,
    block: &BlockMessage,
) {
    let already_saved = block_store
        .contains(&[block.block_hash])
        .await
        .unwrap_or_default()
        .first()
        .copied()
        .unwrap_or(false);
    if !already_saved {
        let _ = block_store.put(&[(block.block_hash, block.clone())]).await;
    }
    let mut guard = st.lock().await;
    *guard = guard.done(&block.block_hash);
}

/// Process an incoming block: validate it, save it, and trigger the next request (port of
/// `processBlock`).
async fn process_block(
    st: &Arc<tokio::sync::Mutex<St>>,
    block_store: &BlockStore,
    log: &dyn Log,
    source: LogSource,
    request_tx: &tokio::sync::mpsc::Sender<bool>,
    block: &BlockMessage,
) {
    let is_valid = validate_received_block(st, block, log, source).await;
    if is_valid {
        save_block(st, block_store, block).await;
    }
    // Trigger the request queue (without resending already-requested blocks).
    let _ = request_tx.send(false).await;
}

/// Take the next set of hashes to request, enqueue existing ones for processing, and broadcast
/// requests for the missing ones (port of `requestNext`).
async fn request_next(
    st: &Arc<tokio::sync::Mutex<St>>,
    response_hash_tx: &tokio::sync::mpsc::UnboundedSender<BlockHash>,
    block_store: &BlockStore,
    comm_util: &CommUtil,
    resend: bool,
) {
    let is_end = { st.lock().await.is_finished() };
    let hashes = {
        let mut guard = st.lock().await;
        let (new_state, hashes) = guard.get_next(resend);
        *guard = new_state;
        hashes
    };

    let hashes_vec: Vec<BlockHash> = hashes.iter().copied().collect();
    let contains = block_store.contains(&hashes_vec).await.unwrap_or_default();
    let mut existing = Vec::new();
    let mut missing = Vec::new();
    for (h, c) in hashes_vec.into_iter().zip(contains) {
        if c {
            existing.push(h);
        } else {
            missing.push(h);
        }
    }

    for h in &existing {
        let _ = response_hash_tx.send(*h);
    }
    if !is_end && !missing.is_empty() {
        for h in &missing {
            comm_util.broadcast_request_for_block(h, Some(1)).await;
        }
    }
}

/// Request all blocks needed for the last finalized state (port of `LfsBlockRequester.stream`).
///
/// Returns the final requester state (the Scala stream's `.last` element) once all blocks are
/// received.
pub async fn request_blocks(
    fringe: &FinalizedFringe,
    incoming_blocks: &mut tokio::sync::mpsc::Receiver<BlockMessage>,
    block_heights_before_fringe: i32,
    request_timeout: Duration,
    block_store: &BlockStore,
    comm_util: &CommUtil,
    log: &dyn Log,
) -> St {
    let source = LogSource::new("casper.engine.LfsBlockRequester");

    // Finalized block hashes from which LFS sync starts.
    let finalized_hashes: BTreeSet<BlockHash> = fringe.hashes.iter().copied().collect();
    let st = Arc::new(tokio::sync::Mutex::new(LfsState::new(
        finalized_hashes.clone(),
        finalized_hashes,
        0,
        block_heights_before_fringe,
    )));

    // `true` triggers a resend of already-requested blocks.
    let (request_tx, mut request_rx) = tokio::sync::mpsc::channel::<bool>(2);
    let (response_hash_tx, mut response_hash_rx) =
        tokio::sync::mpsc::unbounded_channel::<BlockHash>();

    // "Light the fire!" / start the first request for blocks.
    let _ = request_tx.send(false).await;

    // Request loop: pull request triggers (or resend on idle timeout) and request next blocks,
    // terminating once all blocks are finished.
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
                        &format!("No block responses for {request_timeout:?}. Resending requests."),
                    );
                    true
                }
            };
            request_next(&st, &response_hash_tx, block_store, comm_util, resend).await;
            if st.lock().await.is_finished() {
                return;
            }
        }
    };

    // Response loop: handle incoming blocks and existing-block hashes in parallel with the request
    // loop.
    let response_loop = async {
        loop {
            tokio::select! {
                block = incoming_blocks.recv() => {
                    match block {
                        Some(block) => {
                            process_block(&st, block_store, log, source, &request_tx, &block).await;
                        }
                        None => return,
                    }
                }
                hash = response_hash_rx.recv() => {
                    match hash {
                        Some(hash) => {
                            let block = block_store
                                .get(&[hash])
                                .await
                                .unwrap_or_default()
                                .into_iter()
                                .flatten()
                                .next();
                            if let Some(block) = block {
                                log.info(
                                    source,
                                    &format!("Process existing block #{}", block.block_number),
                                );
                                process_block(&st, block_store, log, source, &request_tx, &block).await;
                            }
                        }
                        None => return,
                    }
                }
            }
        }
    };

    tokio::pin!(request_loop);
    tokio::pin!(response_loop);
    tokio::select! {
        _ = &mut request_loop => {},
        _ = &mut response_loop => {},
    }

    let guard = st.lock().await;
    guard.clone()
}
